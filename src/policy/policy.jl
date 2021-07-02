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
function Base.push!(policy::OptaneTracker, block::Block, pool)
    pool == Remote && return block
    bin = findbin(policy.bins, length(block); inbounds = true)

    wrapped = Priority(increment!(policy), block)
    handle = push!(@inbounds(policy.live_local_objects[bin]), wrapped)
    policy.local_handles[bin][block] = handle
    return block
end

function Base.delete!(policy::OptaneTracker, block::Block)
    # TODO: Metadata in block so we don't have to do two searches.
    bin = findbin(policy.bins, length(block); inbounds = true)
    local_dict = policy.local_handles[bin]
    if haskey(local_dict, block)
        delete!(policy.live_local_objects[bin], local_dict[block])
        delete!(local_dict, block)
    end

    evictable_dict = policy.evictable_handles[bin]
    if haskey(evictable_dict, block)
        delete!(policy.marked_as_evictable[bin], evictable_dict[block])
        delete!(evictable_dict, block)
    end
    return nothing
end

update!(policy::OptaneTracker, _) = nothing

function policy_new_alloc(
    policy::OptaneTracker{N},
    manager,
    bytes,
    id,
    priority::AllocationPriority,
) where {N}
    # Can we try to allocate locally?
    if priority != ForceRemote
        ptr = _try_alloc_local(policy, manager, bytes, id, priority)
        ptr === nothing || return ptr
    end

    # Fallback path - allocate remotely.
    return unsafe_alloc(RemotePool(), manager, bytes)
end

function prefetch!(A, policy::OptaneTracker, manager)
    bytes = length(metadata(A))
    if !canalloc(getheap(manager, LocalPool()), bytes)
        _eviction!(policy, manager, bytes)
    end
    return actuate!(LocalPool(), A, manager; freeblock = false)
end

@inline function evict!(A, policy::OptaneTracker, manager)
    return actuate!(RemotePool(), A, manager)
end

#####
##### Implementation
#####

function _try_alloc_local(policy::OptaneTracker{N}, manager, bytes, id, priority) where {N}
    if getsize(getmap(manager, LocalPool())) >= 0.9 * sizeof(getheap(manager, LocalPool()))
        GC.gc(false)
    end

    # If allocation is successful, good!
    ptr = unsafe_alloc(LocalPool(), manager, bytes)
    ptr === nothing || return ptr


    # If we're here, and we really need local, then we have to start evicting.
    if priority == ForceLocal
        _eviction!(policy, manager, bytes)
        ptr = unsafe_alloc(LocalPool(), manager, bytes)
        ptr === nothing || return ptr
    end
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
    for i = bin:N
        mutableheap = @inbounds(policy.marked_as_evictable[bin])
        if !isempty(mutableheap)
            block = getval(Block, first(mutableheap))
            evictfrom!(getheap(manager, LocalPool()), block, bytes; cb)
            return nothing
        end
    end

    # Try evicting not from the easily evictable trackers.
    for i = bin:N
        mutableheap = @inbounds(policy.live_local_objects[bin])
        if !isempty(mutableheap)
            block = getval(Block, first(mutableheap))
            # TODO: make this cleaner ...
            evictfrom!(getheap(manager, LocalPool()), block, bytes; cb)
            return nothing
        end
    end
    return nothing
end

