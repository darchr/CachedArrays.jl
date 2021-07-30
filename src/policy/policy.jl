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
    vectorcache::ObjectCache{Vector{Int},Tuple{}}

    # Objects that live in the local heap that haven't been marked as easily evictable.
    regular_objects::NTuple{N, LRU{Block}}
    evictable_objects::NTuple{N, LRU{Block}}

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
        ObjectCache{Vector{Int}}(),
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

# function softevict!(policy::OptaneTracker, manager, block)
#     # Is this block in any of the live heaps.
#     bin = findbin(policy.bins, length(block); inbounds = true)
#     local_dict = policy.local_handles[bin]
#     local_handle = get(local_dict, block, nothing)
#     if local_handle !== nothing
#         delete!(policy.live_local_objects[bin], local_dict[block])
#         delete!(local_dict, block)
#
#         # Add to evictable dict.
#         wrapped = Priority(increment!(policy), block)
#         local_evictable_dict = policy.marked_as_evictable[bin]
#         handle = push!(local_evictable_dict, wrapped)
#
#         policy.evictable_handles[bin][block] = handle
#     end
#     return nothing
# end

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
    @return_if_exists ptr = unsafe_alloc_direct(RemotePool(), manager, bytes, id)
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
            old = setprimary!(manager, oldblock, newblock; unsafe = true)
            @check delete!(policy, oldblock; len = length(oldblock))
            push!(policy, newblock, pool; len = length(oldblock))
        end
    end
end

function prefetch!(A, policy::OptaneTracker, manager)
    policy.movement_enabled || return nothing

    block = metadata(A)
    getpool(block) == Local && return nothing
    bytes = length(block)
    if !canalloc(getheap(manager, LocalPool()), bytes)
        _eviction!(policy, manager, bytes)
    end

    isqueued(block) && return nothing
    # Allocate and move.
    # Don't free the old block since we're functioning as a cache.
    newblock = unsafe_block(unsafe_alloc_direct(LocalPool(), manager, length(block), getid(block)))
    copyto!(newblock, block, manager)
    link!(newblock, block)
    result = setprimary!(manager, block, newblock)
    if result === nothing
        unsafe_free_direct(manager, newblock)
    else
        # Need to update the local policy tracking to know that this is now local.
        push!(policy, newblock, Local)
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
    ptr = unsafe_alloc_direct(LocalPool(), manager, bytes, id)
    if ptr === nothing && (policy.movement_enabled == false)
        safeprint("Not evicting because movement is disabled!"; force = true)
    end

    # If we're here, and we really need local, then we have to start evicting.
    if policy.movement_enabled && priority == ForceLocal && ptr === nothing
        _eviction!(policy, manager, bytes)
        ptr = unsafe_alloc_direct(LocalPool(), manager, bytes, id)
    end
    return ptr
end

function eviction_callback(manager, policy, block::Block)
    # Copy back to sibling if one already exists.
    sibling = getsibling(block)
    if sibling !== nothing
        copyto!(sibling, block, manager)
        result = setprimary!(manager, block, sibling)
        if result !== nothing
            unlink!(block, sibling)
            @check delete!(policy, block)
            unsafe_free_direct(manager, block)
        end
        return nothing
    end

    newblock = unsafe_block(unsafe_alloc_direct(RemotePool(), manager, length(block), getid(block)))
    copyto!(newblock, block, manager)
    result = setprimary!(manager, block, newblock)

    # Setting primary failed because the current block is queued for freeing.
    # Roll back our change.
    #
    # Otherwise, setting the primary was successful, so we free the old block.
    if result === nothing
        unsafe_free_direct(manager, newblock)
    else
        @check delete!(policy, block)
        unsafe_free_direct(manager, block)
    end
    return nothing
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

