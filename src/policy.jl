#####
##### More Advanced Policy
#####

# TODO: Mutable Binary Heaps from DataStructurtes.jl aren't really that great.
# Consider using a different data structure that doesn't suffer the memory blowup of
# the DataStructures.jl implementation.

# Type parameter: number of bins
mutable struct OptaneTracker{N}
    count::Int

    # The minimum size of each bin.
    bins::NTuple{N,Int}

    # Objects that live in the local heap that haven't been marked as easily evictable.
    regular_objects::NTuple{N,LRU{Block}}
    evictable_objects::NTuple{N,LRU{Block}}

    # For critical regions with concurrent allocations
    movement_enabled::Bool
end

function OptaneTracker(bins::NTuple{N,Int}) where {N}
    count = 0
    regular_objects = ntuple(_ -> LRU{Block}(), Val(N))
    evictable_objects = ntuple(_ -> LRU{Block}(), Val(N))

    return OptaneTracker{N}(
        count,
        bins,
        regular_objects,
        evictable_objects,
        true,
    )
end

# Almost exactly like `Base.findlast`, but instead of returning `N+1` if the predicate
# is never true, return `N`.
@inline function findbin(bins::NTuple{N,Int}, x; inbounds = true) where {N}
    for i in Base.OneTo(N)
        x <= @inbounds(bins[i]) && return i
    end
    return inbounds ? N : (N + 1)
end

function increment!(policy::OptaneTracker)
    x = policy.count
    policy.count += 1
    return x
end

#####
##### Policy API
#####

function Base.push!(
    policy::OptaneTracker,
    block::Block,
    pool = getpool(block);
    len = length(block),
)
    # Don't explicitly track remote blocks.
    pool == Remote && return block
    bin = findbin(policy.bins, len; inbounds = true)
    lru = policy.regular_objects[bin]
    push!(lru, block, increment!(policy))
    return block
end

function Base.delete!(policy::OptaneTracker, block::Block; len = length(block))
    getpool(block) == Remote && return true

    # TODO: Metadata in block so we don't have to do two searches.
    bin = findbin(policy.bins, len; inbounds = true)
    lru = policy.regular_objects[bin]
    if in(block, lru)
        delete!(lru, block)
        return true
    end

    lru = policy.evictable_objects[bin]
    if in(block, lru)
        delete!(lru, block)
        return true
    end
    return false
end

# TODO
update!(policy::OptaneTracker, _) = nothing

function softevict!(policy::OptaneTracker, manager, block)
    # Is this block in any of the live heaps.
    bin = findbin(policy.bins, length(block); inbounds = true)
    lru = policy.regular_objects[bin]
    if in(block, lru)
        delete!(lru, block)
        push!(policy.evictable_objects[bin], block, increment!(policy))
    end
    return nothing
end

#####
##### Policy <---> CacheManager bridge
#####

### policy_new_alloc
function policy_new_alloc(
    policy::OptaneTracker{N},
    manager,
    bytes,
    id,
    priority::AllocationPriority,
) where {N}
    # Can we try to allocate locally?
    if priority != ForceRemote
        @return_if_exists ptr = _try_alloc_local(policy, manager, bytes, id, priority)
    end

    # Fallback path - allocate remotely.
    # If we're getting close to filling up the remote pool, do an emergency full GC.
    # allocated, total = getstate(getheap(manager, RemotePool()))
    # if allocated / total >= 0.80
    #     # Trigger full GC and try to get pending finalizers to run.
    #     GC.gc(true)
    # end

    @return_if_exists ptr = unsafe_alloc_direct(RemotePool(), manager, bytes, id)
    @show manager
    error("Ran out of memory!")
end

function defrag!(manager, policy::OptaneTracker = manager.policy)
    defrag!(getheap(manager, LocalPool())) do id, newblock, oldblock
        # First, we need to check if this is even the primary block for the corresponding
        # object.
        #
        # N.B.: No need to handle the other case since the "defrag!" routine for the heap
        # will preserve siblings.
        #
        # This may change ...
        primary = getprimary(manager, oldblock)
        if primary === oldblock
            old = unsafe_setprimary!(manager, oldblock, newblock; unsafe = true)
            @check delete!(policy, oldblock; len = length(oldblock))
            push!(policy, newblock, pool; len = length(oldblock))
        end
    end
end

function prefetch!(block::Block, policy::OptaneTracker, manager; readonly = false)
    policy.movement_enabled || return nothing
    block.size > sizeof(getheap(manager, LocalPool())) && return nothing

    if getpool(block) == Local
        return nothing
    end

    @check getsibling(block) === nothing
    bytes = length(block)
    if !canalloc(getheap(manager, LocalPool()), bytes)
        _eviction!(policy, manager, bytes)
    end

    if isqueued(block)
        return nothing
    end
    # Allocate and move.
    # Don't free the old block since we're functioning as a cache.
    ptr = unsafe_alloc_direct(LocalPool(), manager, length(block), getid(block))
    ptr === nothing && return nothing
    newblock = unsafe_block(ptr)
    copyto!(newblock, block, manager)
    result = unsafe_setprimary!(manager, block, newblock)
    if result === nothing
        unsafe_free_direct(manager, newblock)
    else
        # Need to update the local policy tracking to know that this is now local.
        push!(policy, newblock, Local)
        link!(newblock, block)

        # If this is not a read-only prefetch, then we need to be conservative and mark the
        # prefetched block as dirty.
        setdirty!(newblock, !readonly)
    end

    return nothing
end

function evict!(A, policy::OptaneTracker, manager)
    policy.movement_enabled || return nothing
    if getpool(metadata(A)) != Remote
        eviction_callback(manager, policy, metadata(A))
    end
    return nothing
end

#####
##### Implementation
#####

# TODO: Time since last GC?
function _try_alloc_local(policy::OptaneTracker, manager, bytes, id, priority)
    allocated, total = getstate(getheap(manager, LocalPool()))
    if allocated / total >= 0.90
        # Trigger full GC and try to get pending finalizers to run.
        GC.gc(false)
    end

    # If allocation is successful, good!
    @return_if_exists ptr = unsafe_alloc_direct(LocalPool(), manager, bytes, id)

    # If we're here, and we really need local, then we have to start evicting.
    if policy.movement_enabled && priority == ForceLocal
        _eviction!(policy, manager, bytes)
        ptr = unsafe_alloc_direct(LocalPool(), manager, bytes, id)
    end
    return ptr
end

function _cleanup!(manager, id)
    cleaned = unsafe_cleanup!(manager, id)
    @check cleaned
    return nothing
end

function eviction_callback(manager, policy, block::Block)
    # Copy back to sibling if one already exists.
    id = getid(block)
    if isqueued(block)
        _cleanup!(manager, id)
        return false
    end

    sibling = getsibling(block)
    if sibling !== nothing
        isdirty(block) && copyto!(sibling, block, manager)
        result = unsafe_setprimary!(manager, block, sibling)
        if result === nothing
            _cleanup!(manager, id)
        else
            unlink!(block, sibling)
            @check delete!(policy, block)
            unsafe_free_direct(manager, block)
        end
        return false
    end

    # Rare case where a pointer may be null.
    # In this case, we need to trigger the abort mechanism in the eviciton process so we
    # don't leave the heap in an undefined state.
    ptr = unsafe_alloc_direct(RemotePool(), manager, length(block), getid(block))
    ptr === nothing && return true

    newblock = unsafe_block(ptr)
    copyto!(newblock, block, manager)
    result = unsafe_setprimary!(manager, block, newblock)

    # Setting primary failed because the current block is queued for freeing.
    # Roll back our change.
    #
    # Otherwise, setting the primary was successful, so we free the old block.
    if result === nothing
        unsafe_free_direct(manager, newblock)
        _cleanup!(manager, id)
    else
        # For now, make sure that something was actually deleted because it should ALWAYS
        # be tracked by the policy.
        @check delete!(policy, block)
        unsafe_free_direct(manager, block)
    end
    return false
end

function _eviction!(policy::OptaneTracker{N}, manager, bytes) where {N}
    # Get a bin number for this allocation.
    # If we have an easily evictable object of the correct size, then use that.
    # Pass the "inbounds = true" flag because if we don't have a block big
    # enough, then we want to evict from the largest bin.
    bin = findbin(policy.bins, bytes; inbounds = true)

    # TODO: Cleanup
    # canabort = manager.abort_callback
    # canabort.bytes = bytes
    cb = x -> eviction_callback(manager, policy, x)
    localheap = getheap(manager, LocalPool())

    # Check this bin and all higher bins.
    for i in bin:N
        lru = policy.evictable_objects[bin]
        if !isempty(lru)
            block = first(lru)
            @check getpool(block) == Local
            evictfrom!(localheap, block, bytes; cb)
            return nothing
        end
    end

    # Try evicting not from the easily evictable trackers.
    for i in bin:N
        lru = policy.regular_objects[bin]
        if !isempty(lru)
            block = first(lru)
            @check getpool(block) == Local
            evictfrom!(localheap, block, bytes; cb)
            return nothing
        end
    end
    return nothing
end

# #####
# ##### Intermediate Tracking
# #####
#
# function _unsafe_track!(policy::OptaneTracker)
#     # Only allow one call site to activate tracking.
#     if policy.tracking == false
#         policy.tracking = true
#         empty!(policy.intermediates)
#         return true
#     else
#         return false
#     end
# end
#
# function _unsafe_untrack!(manager, policy::OptaneTracker, token::Bool, return_value)
#     token == false && return nothing
#     if policy.tracking == false
#         error("Passed a `true` token to a policy that was not tracking!!!")
#     end
#     policy.tracking = false
#
#     # Gather all regions with tracked blocks.
#     # Unsafe free all of those that do not show up in the return value.
#     # Function `onblocks` defined in `lib.jl`.
#     intermediates = policy.intermediates
#     onblocks(x -> delete!(intermediates, x), return_value)
#
#     # Everything left in `intermediates` is was allocated at the entry point and hasn't
#     # escaped by a return value.
#     #
#     # Try to clean everything up.
#     # N.B.: It's possible that during `remove_seen!`, the garbage collector ran.
#     # Since we hold the manager's allocator lock, we know that the freebuffer hasn't
#     # been cleaned, so we can check the `queued` bit to determine if it's save to
#     # `unsafe_free`.
#     for block in intermediates
#         block.queued && continue
#         unsafe_clearprimary!(manager, block)
#         free(manager, block)
#     end
#     return nothing
# end

