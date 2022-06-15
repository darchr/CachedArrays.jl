#####
##### More Advanced Policy
#####

# TODO: Mutable Binary Heaps from DataStructurtes.jl aren't really that great.
# Consider using a different data structure that doesn't suffer the memory blowup of
# the DataStructures.jl implementation.

# Type parameter: number of bins
mutable struct OptaneTracker{N,AllowsPrefetch,AllowsUnlinked,AllowsNoescape,AllowsCleanup}
    count::Int

    # The minimum size of each bin.
    bins::NTuple{N,Int}

    # Objects that live in the local heap that haven't been marked as easily evictable.
    regular_objects::NTuple{N,LRU{Block}}
    evictable_objects::NTuple{N,LRU{Block}}

    # For critical regions with concurrent allocations
    movement_enabled::Bool
end

function default_optane_kw()
    return DataStructures.OrderedDict{Symbol,Any}(
        :AllowsPrefetch => :(<:Any),
        :AllowsUnlinked => :(<:Any),
        :AllowsNoescape => :(<:Any),
        :AllowsCleanup => :(<:Any),
    )
end

macro OptaneTracker(args...)
    if length(args) >= 1 && (isa(args[1], Integer) || isa(args[1], Symbol))
        N = esc(args[1])
        args = args[2:end]
    else
        N = :(<:Any)
    end

    # Unpack keyword arguments
    kw = default_optane_kw()
    kw_type_map = Dict(k => Union{Bool,Symbol} for k in keys(kw))
    for i in eachindex(args)
        arg = args[i]
        if arg.head != :(=)
            error("Expected keyword arguments to be in the form `a=b`!")
        end

        # Match implemented keywords
        name = arg.args[1]
        value = arg.args[2]
        expected_type = get(kw_type_map, name, nothing)
        expected_type === nothing && error("Unknown keyword argument $(name)!")
        if !isa(value, expected_type)
            msg = """
            Expected keyword argument $name to have type $expected_type.
            Instead, it has type $(typeof(value))!
            """
            error(msg)
        end
        kw[name] = esc(value)
    end
    kv = collect(values(kw))
    return :(OptaneTracker{$N,$(kv...)})
end

__allows_prefetch(::@OptaneTracker(AllowsPrefetch = T)) where {T} = T
__allows_unlinked(::@OptaneTracker(AllowsUnlinked = T)) where {T} = T
__allows_noescape(::@OptaneTracker(AllowsNoescape = T)) where {T} = T
__allows_cleanup(::@OptaneTracker(AllowsCleanup = T)) where {T} = T

function OptaneTracker(
    bins::NTuple{N,Int};
    allows_prefetch = true,
    allows_unlinked = true,
    allows_noescape = true,
    allows_cleanup = true,
) where {N}
    count = 0
    regular_objects = ntuple(_ -> LRU{Block}(), Val(N))
    evictable_objects = ntuple(_ -> LRU{Block}(), Val(N))

    return OptaneTracker{N,allows_prefetch,allows_unlinked,allows_noescape,allows_cleanup}(
        count, bins, regular_objects, evictable_objects, true
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
    policy::OptaneTracker, block::Block, pool = getpool(block); len = length(block)
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

    # Search through standard and evictible bins.
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
update!(::OptaneTracker, _) = nothing
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
    policy::OptaneTracker, manager, bytes, id, priority::AllocationPriority
)
    # Trigger full GC and try to get pending finalizers to run.
    allocated, total = getstate(getheap(manager, RemotePool()))
    if allocated / total >= 0.80
        println("Preemptively Invoking Garbage Collector!")
        GC.gc(true)
    end

    # Can we try to allocate locally?
    if priority != ForceRemote
        @return_if_exists ptr = _try_alloc_local(policy, manager, bytes, id, priority)
    end
    @return_if_exists ptr = unsafe_alloc_direct(RemotePool(), manager, bytes, id)
    error("Ran out of memory!")
    return nothing
end

# new_allow when unlinked is untrue
function policy_new_alloc(
    policy::@OptaneTracker(AllowsUnlinked = false),
    manager,
    bytes,
    id,
    priority::AllocationPriority,
)
    # Trigger full GC and try to get pending finalizers to run.
    allocated, total = getstate(getheap(manager, RemotePool()))
    if allocated / total >= 0.80
        println("Preemptively Invoking Garbage Collector!")
        GC.gc(true)
    end

    # Always allocate remotely
    remote_ptr = unsafe_alloc_direct(RemotePool(), manager, bytes, id)
    if remote_ptr === nothing
        error("Ran out of memory!")
    end

    if priority != ForceRemote
        # Because we haven't actually registered these regions with the cache manager yet,
        # we need to send the `setprimary = false` flag to the `prefetch!`.
        # This will avoid trying to update the backedge pointer in the manager.
        local_block = forceprefetch!(
            unsafe_block(remote_ptr), policy, manager; setprimary = false, readonly = false
        )
        if local_block !== nothing
            return datapointer(local_block)
        end
    end
    return remote_ptr
end

function defrag!(manager, policy::OptaneTracker)
    cb = () -> unsafe_cleanup!(manager)
    defrag!(getheap(manager, LocalPool()); queued_callback = cb) do _, newblock, oldblock
        # First, we need to check if this is even the primary block for the corresponding
        # object.
        #
        # N.B.: No need to handle the other case since the "defrag!" routine for the heap
        # will preserve siblings.
        #
        # This may change ...
        primary = getprimary(manager, oldblock)
        if primary === oldblock
            _ = unsafe_setprimary!(manager, oldblock, newblock; unsafe = true)
            @check delete!(policy, oldblock; len = length(oldblock))
            push!(policy, newblock, pool; len = length(oldblock))
        end
    end
end

#####
##### prefetch!
#####

# We split `prefetch!` into two levels:
# * `forceprefetch!`: Force prefetch to happen, whether or not the policy technically
#    allows it.
#
# * `prefetch!`: Upper level dispatch for prefetching.
#   This is the level where `OptaneTrackers` with disabled prefetching are allowed to
#   intercept the function calls.

function prefetch!(block::Block, policy::OptaneTracker, manager; kw...)
    return forceprefetch!(block, policy, manager; kw...)
end

function prefetch!(
    block::Block, policy::@OptaneTracker(AllowsPrefetch = false), manager; kw...
)
    return nothing
end

"""
    forceprefetch!(block::Block, policy::OptaneTracker, manager; [readonly], [setprimary])

If `block` is not in `Local` memory, create a new block in `Local` memory linked with
`block` and copy the contents into this new block.

N.B.: This function may trigger eviction of local memory if there is not enough room to
satisfy the requested prefetch.

Keyword Arguments
-----------------
* `readonly`: Hint that this prefetch will be a readonly prefetch.
    The locally-created `Block` will not be marked as dirty and hence will not be writteen
    back to the remote host if evicted (unless it is marked as dirty at some point in
    the future). Default: `false`

* `setprimary`: By default, this function will update the cache manager's view of the
  world to reflect the new primary block to the host `Object`. If `forceprefetch!` is
  called *before* the host `Object` is created, than, than `setprimary = false`
  should be passed. Default: `true`
"""
function forceprefetch!(
    block::Block, policy::OptaneTracker, manager; readonly = false, setprimary = true
)
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

    # Side-effect check
    # If the block was freed by the GC while we were performing eviction, than it
    # will be marked as `isqueued`.
    #
    # If that is the case, then `prefetch!` should abort.
    # However, if we called `prefetch!` on something, than it *should* be rooted,
    # so this should never happen ...
    if isqueued(block)
        return nothing
    end

    # Allocate and move.
    # Don't free the old block since we're functioning as a cache.
    ptr = unsafe_alloc_direct(LocalPool(), manager, length(block), getid(block))
    ptr === nothing && return nothing
    newblock = unsafe_block(ptr)
    copyto!(newblock, block, manager)

    # `unsafe_setprimary!` can fail due to GC related reasons.
    # If that happens, it will return `Nothing` and we should abort the current operation.
    result = setprimary ? unsafe_setprimary!(manager, block, newblock) : block
    if result === nothing
        unsafe_free_direct(manager, newblock)
    else
        # Need to update the local policy tracking to know that this is now local.
        setprimary && push!(policy, newblock, Local)
        link!(newblock, block)

        # If this is not a read-only prefetch, then we need to be conservative and mark the
        # prefetched block as dirty.
        setdirty!(newblock, !readonly)
    end
    return newblock
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

function _try_alloc_local(policy::OptaneTracker, manager, bytes, id, priority)
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
    #safeprint("Evicting block $(getid(block)) with size $(block.size)"; force = true)

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
    for i = bin:N
        lru = policy.evictable_objects[i]
        if !isempty(lru)
            block = first(lru)
            @check getpool(block) == Local
            evictfrom!(localheap, block, bytes; cb)
            return nothing
        end
    end

    # Try evicting not from the easily evictable trackers.
    for i = bin:N
        lru = policy.regular_objects[i]
        if !isempty(lru)
            block = first(lru)
            @check getpool(block) == Local
            evictfrom!(localheap, block, bytes; cb)
            return nothing
        end
    end
    return nothing
end

#####
##### API Disabling Extensions
#####

# Disable `@noescape` for certain policies.
function noescape(
    ::CacheManager{<:@OptaneTracker(AllowsNoescape = false)},
    ::Val,
    f::F,
    args::Vararg{Any,N};
    kw...,
) where {F,N}
    return f(args...; kw...)
end

# Disable `cleanup!` and `preserve!` for certain policies.
function unsafe_free(
    ::Object{<:CacheManager{<:@OptaneTracker(AllowsCleanup = false)}}, ::CleanupContext
)
    return nothing
end
