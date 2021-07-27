# Things to think about
#
# Allow arbitrary hooks for:
#     Usage Updates
#     Marking Dirty
#     Other hints
#
# How do we track state?
# Should the element types of the eviction policies be metadata?
getval(::Type{T}, x::T) where {T} = x

# Policy Hints
setdirty!(x, meta, flag) = nothing
cheapevict(x, meta) = nothing

include("lru.jl")
include("random.jl")

#####
##### More Advanced Policy
#####

# TODO: Mutable Binary Heaps from DataStructurtes.jl aren't really that great.
# Consider using a different data structure that doesn't suffer the memory blowup of
# the DataStructures.jl implementation.
const MutableMinHeap{T} = DataStructures.MutableBinaryHeap{T,Base.ForwardOrdering}

# Type parameter: number of bins
mutable struct OptaneTracker{N}
    count::Int

    # The minimum size of each bin.
    bins::NTuple{N,Int}

    # Objects that live in the local heap that haven't been marked as easily evictable.
    live_local_objects::NTuple{N,MutableMinHeap{Priority{Block}}}
    local_handles::NTuple{N,Dict{Block,Int}}

    # Objects that live in the local heap that HAVE been marked as easily evictable
    # and the amount of space that can potentially be freed.
    marked_as_evictable::NTuple{N,MutableMinHeap{Priority{Block}}}
    evictable_handles::NTuple{N,Dict{Block,Int}}
    movement_enabled::Bool
end

function OptaneTracker(bins::NTuple{N,Int}) where {N}
    count = 0
    live_local_objects = ntuple(_ -> MutableMinHeap{Priority{Block}}(), Val(N))
    local_handles = ntuple(_ -> Dict{Block,Int}(), Val(N))
    marked_as_evictable = ntuple(_ -> MutableMinHeap{Priority{Block}}(), Val(N))
    evictable_handles = ntuple(_ -> Dict{Block,Int}(), Val(N))
    return OptaneTracker{N}(
        count,
        bins,
        live_local_objects,
        local_handles,
        marked_as_evictable,
        evictable_handles,
        true,
    )
end

function increment!(policy::OptaneTracker)
    x = policy.count
    policy.count += 1
    return x
end

# Almost exactly like `Base.findlast`, but instead of returning `N+1` if the predicate
# is never true, return `N`.
@inline function findbin(bins::NTuple{N,Int}, x; inbounds = true) where {N}
    for i in Base.OneTo(N)
        x <= @inbounds(bins[i]) && return i
    end
    return inbounds ? N : (N + 1)
end

#####
##### API
#####

Base.eltype(::OptaneTracker) = Block
function Base.push!(policy::OptaneTracker, block::Block, pool; len = length(block))
    pool == Remote && return block
    bin = findbin(policy.bins, len; inbounds = true)

    wrapped = Priority(increment!(policy), block)
    handle = push!(@inbounds(policy.live_local_objects[bin]), wrapped)
    policy.local_handles[bin][block] = handle
    return block
end

function Base.delete!(policy::OptaneTracker, block::Block; len = length(block))
    getpool(block) == Remote && return true

    # TODO: Metadata in block so we don't have to do two searches.
    bin = findbin(policy.bins, len; inbounds = true)
    local_dict = policy.local_handles[bin]
    deleted = false
    if haskey(local_dict, block)
        deleted = true
        delete!(policy.live_local_objects[bin], local_dict[block])
        delete!(local_dict, block)
    end

    evictable_dict = policy.evictable_handles[bin]
    if haskey(evictable_dict, block)
        deleted = true
        delete!(policy.marked_as_evictable[bin], evictable_dict[block])
        delete!(evictable_dict, block)
    end
    return deleted
end

update!(policy::OptaneTracker, _) = nothing

function softevict!(policy::OptaneTracker, manager, block)
    # Is this block in any of the live heaps.
    bin = findbin(policy.bins, length(block); inbounds = true)
    local_dict = policy.local_handles[bin]
    local_handle = get(local_dict, block, nothing)
    if local_handle !== nothing
        delete!(policy.live_local_objects[bin], local_dict[block])
        delete!(local_dict, block)

        # Add to evictable dict.
        wrapped = Priority(increment!(policy), block)
        local_evictable_dict = policy.marked_as_evictable[bin]
        handle = push!(local_evictable_dict, wrapped)

        policy.evictable_handles[bin][block] = handle
    end
    return nothing
end

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

        # Is defragmentation going to be worth it?
        if policy.movement_enabled
            localheap = getheap(manager, LocalPool())
            localmap = getmap(manager, LocalPool())
            heaplength = sizeof(localheap)
            allocated = getsize(localmap)
            if (heaplength - allocated) >= bytes
                defrag!(manager, policy)
                @return_if_exists ptr = unsafe_alloc(LocalPool(), manager, bytes)
            end
        else
            safeprint("Not defragging because movement is disabled!"; force = true)
        end
    end

    # Fallback path - allocate remotely.
    safeprint("Allocating remote!"; force = true)
    @return_if_exists ptr = unsafe_alloc(RemotePool(), manager, bytes)
    error("Ran out of memory!")
end

function defrag!(manager, policy::OptaneTracker = manager.policy)
    pool = LocalPool()
    localmap = getmap(manager, pool)
    defrag!(getheap(manager, pool)) do id, newblock, oldblock
        old = atomic_ptr_xchg!(localmap[id], datapointer(newblock))
        @check delete!(policy, oldblock; len = length(oldblock))
        push!(policy, newblock, pool; len = length(oldblock))
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
    actuate!(LocalPool(), A, manager; freeblock = false)
    return nothing
end

function evict!(A, policy::OptaneTracker, manager)
    policy.movement_enabled || return nothing

    if getpool(metadata(A)) != Remote
        actuate!(RemotePool(), A, manager)
    end
    return nothing
end

#####
##### Implementation
#####

# TODO: Time since last GC?
function _try_alloc_local(policy::OptaneTracker, manager, bytes, id, priority)
    if getsize(getmap(manager, LocalPool())) >= 0.95 * sizeof(getheap(manager, LocalPool()))
        # Trigger full GC and try to get pending finalizers to run.
        GC.gc(false)
    end

    # If allocation is successful, good!
    ptr = unsafe_alloc(LocalPool(), manager, bytes)

    if ptr === nothing && (policy.movement_enabled == false)
        safeprint("Not evicting because movement is disabled!"; force = true)
    end

    # If we're here, and we really need local, then we have to start evicting.
    if policy.movement_enabled && priority == ForceLocal && ptr === nothing
        _eviction!(policy, manager, bytes)
        ptr = unsafe_alloc(LocalPool(), manager, bytes)
    end
    return ptr
end

function _eviction!(policy::OptaneTracker{N}, manager, bytes) where {N}
    # Get a bin number for this allocation.
    # If we have an easily evictable object of the correct size, then use that.
    # Pass the "inbounds = true" flag because if we don't have a block big
    # enough, then we want to evict from the largest bin.
    bin = findbin(policy.bins, bytes; inbounds = false)

    # TODO: Cleanup
    canabort = manager.abort_callback
    canabort.bytes = bytes
    cb = x -> actuate!(RemotePool(), x, manager; canabort = canabort)

    # Check this bin and all higher bins.
    for i in bin:N
        mutableheap = @inbounds(policy.marked_as_evictable[bin])
        if !isempty(mutableheap)
            block = getval(Block, first(mutableheap))
            @check in(block.id, getmap(manager, LocalPool()))
            evictfrom!(getheap(manager, LocalPool()), block, bytes; cb)
            return nothing
        end
    end

    # Try evicting not from the easily evictable trackers.
    for i in bin:N
        mutableheap = @inbounds(policy.live_local_objects[bin])
        if !isempty(mutableheap)
            block = getval(Block, first(mutableheap))
            @check in(block.id, getmap(manager, LocalPool()))
            # TODO: make this cleaner ...
            evictfrom!(getheap(manager, LocalPool()), block, bytes; cb)
            return nothing
        end
    end
    return nothing
end

