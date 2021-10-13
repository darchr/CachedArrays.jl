const RemotePool = PoolType{Remote}
const LocalPool = PoolType{Local}

#####
##### Backedges
#####

# Back edges from the manager to the GC managed object holding a block.
# Kept as a raw Ptr{Ptr} to avoid GC cycles.
const Backedge = Ptr{Ptr{Nothing}}
unsafe_block(backedge::Backedge) = unsafe_block(unsafe_load(backedge))

backedge(x::Backedge) = x
backedge(x::Ptr) = convert(Backedge, x)
backedge(x) = backedge(blockpointer(x))

mutable struct BackedgeMap
    dict::Dict{UInt,Tuple{Block,Backedge}}
    size::Int
end

BackedgeMap() = BackedgeMap(Dict{UInt,Backedge}(), 0)

Base.getindex(map::BackedgeMap, id) = map.dict[id]
Base.setindex!(map::BackedgeMap, bb::Tuple{Block,Backedge}, id) = setindex!(map.dict, bb, id)
function set!(map::BackedgeMap, block::Block, backedge::Backedge)
    id = getid(block)
    sz = length(block)

    @check !in(id, map)
    map.dict[id] = (block, backedge)
    map.size += sz
    return backedge
end

function Base.delete!(map::BackedgeMap, id::UInt, sz::Integer)
    delete!(map.dict, id)
    @check map.size >= sz
    map.size -= sz
    return nothing
end

getsize(map::BackedgeMap) = map.size
Base.in(id, map::BackedgeMap) = haskey(map.dict, id)
Base.haskey(map::BackedgeMap, id) = haskey(map.dict, id)
Base.length(map::BackedgeMap) = length(map.dict)
Base.isempty(map::BackedgeMap) = isempty(map.dict)
Base.keys(map::BackedgeMap) = keys(map.dict)

Base.iterate(map::BackedgeMap, s...) = iterate(map.dict, s...)

#####
##### Cache Manager
#####

struct NoTelemetry end
mutable struct CacheManager{C,R,L,T}
    map::BackedgeMap

    # local datastructures
    policy::C
    remote_heap::CompactHeap{R}
    local_heap::CompactHeap{L}

    # Create a new ID for each object registered in the cache.
    idcount::Threads.Atomic{UInt64}

    # Synchronize access
    alloc_lock::Base.Threads.SpinLock
    freebuffer::FreeBuffer{Block}
    telemetry::T
end

# Telemetry Utils
gettelemetry(manager::CacheManager) = manager.telemetry
telemetry_enabled(::CacheManager) = true
telemetry_enabled(::CacheManager{<:Any,<:Any,<:Any,NoTelemetry}) = false

macro telemetry(manager, expr)
    return quote
        if telemetry_enabled($(esc(manager)))
            $(esc(expr))
        end
    end
end

# Accessor Utils
getmap(manager::CacheManager, id::Integer) = manager.map[id]
getmap(manager::CacheManager) = manager.map

getheap(manager::CacheManager, ::PoolType{Local}) = manager.local_heap
getheap(manager::CacheManager, ::PoolType{Remote}) = manager.remote_heap

candrain(manager::CacheManager) = candrain(manager.freebuffer)

"""
$(TYPEDSIGNATURES)

Returns the allocation lock for `manager` without acquiring it.
"""
alloc_lock(manager::CacheManager) = manager.alloc_lock

mb(x) = x * 1_000_000

function CacheManager(
    local_allocator,
    remote_allocator;
    localsize = mb(1_000),
    remotesize = mb(1_000),
    minallocation = 10,
    policy = OptaneTracker((2^minallocation,)),
    telemetry = NoTelemetry(),
)
    map = BackedgeMap()

    # Initialize Heaps
    remote_heap = CompactHeap(remote_allocator, remotesize; pool = Remote, minallocation)
    local_heap = CompactHeap(local_allocator, localsize; pool = Local, minallocation)

    # Construct the manager.
    manager = CacheManager(
        map,
        policy,
        remote_heap,
        local_heap,
        Threads.Atomic{UInt64}(1),
        Threads.SpinLock(),
        FreeBuffer{Block}(),
        telemetry,
    )

    # Add this to the global manager list to ensure that it outlives any of its users.
    push!(GlobalManagers, manager)
    return manager
end

getid(manager::CacheManager) = Threads.atomic_add!(manager.idcount, one(UInt64))

# Can only GC if all objects tracked via backedges have been collected.
function cangc(manager::CacheManager)
    # Empty out the cleanlist to deal with anything that's been GC'd
    @spinlock alloc_lock(manager) unsafe_cleanup!(manager)
    return isempty(manager.map)
end

function Base.show(io::IO, M::CacheManager)
    println(io, "Cache Manager")
    println(io, "    $(length(M.map)) Objects")
    println(io, "    $(getsize(M.map) / 1E9) GB Memory Used.")
    allocated, total = getstate(M.local_heap) ./ 1E9
    println(io, "    Local Heap utilization: $allocated of $total ($(allocated / total) %)")
    allocated, total = getstate(M.remote_heap) ./ 1E9
    println(
        io,
        "    Remote Heap utilization: $allocated of $total ($(allocated / total) %)",
    )
end

#####
##### Object (De)Registration
#####

function unsafe_register!(manager::CacheManager, object::Object) where {T}
    @requires alloc_lock(manager)
    block = metadata(object)
    set!(getmap(manager), block, backedge(object))
    push!(manager.policy, block)
    return nothing
end

function unsafe_unregister!(manager::CacheManager, block::Block) where {T}
    @requires alloc_lock(manager)
    delete!(getmap(manager), getid(block), length(block))
    delete!(manager.policy, block)
    return nothing
end

#####
##### Lowest Level Alloc and Free
#####

function unsafe_free_direct(manager::CacheManager, block::Block)
    ptr = datapointer(block)
    pool = getpool(block)
    if pool == Local
        free(getheap(manager, LocalPool()), ptr)
    elseif pool == Remote
        free(getheap(manager, RemotePool()), ptr)
    end
    return nothing
end

function unsafe_alloc_direct(
    pool::PoolType{T},
    manager::CacheManager,
    bytes,
    id::UInt,
) where {T}
    @requires alloc_lock(manager)
    ptr = alloc(getheap(manager, pool), bytes, id)
    if ptr !== nothing
        @telemetry manager telemetry_alloc(gettelemetry(manager), manager, bytes, id, ptr, T)
    end
    return ptr
end

function direct_alloc(pool::PoolType, manager::CacheManager, bytes, id::UInt)
    @spinlock alloc_lock(manager) begin
        # TODO: What to do about zero sized allocations ...
        ptr = iszero(bytes) ? Ptr{Nothing}() : unsafe_alloc_direct(pool, manager, bytes, id)
        ptr === nothing && throw(AllocationException())
        return Object(ptr, manager)
    end
end

#####
##### Cleanup Finalizer
#####

# In general, we don't know when the GC will run.
# So, we make the finalizer really short, just appending the block to be cleaned up
# to a vector.
#
# When we're trying to allocate (i.e., holding the lock) - THEN we'll call the `_cleanup`
# method below which will put back all of the blocks on the `cleanlist`.
prepare_cleanup!(manager::CacheManager) = unsafe_swap!(manager.freebuffer)
maybe_cleanup!(M::CacheManager, args...) = candrain(M) && unsafe_cleanup!(M, args...)
function unsafe_cleanup!(M::CacheManager, id = nothing)
    @requires alloc_lock(M)
    prepare_cleanup!(M)
    id_cleaned = false

    while true
        cleanlist = unsafe_get(M.freebuffer)

        # Free all blocks in the cleanlist
        for block in cleanlist
            @check !isfree(block)

            id == getid(block) && (id_cleaned = true)
            @telemetry M telemetry_gc(gettelemetry(M), getid(block))
            sibling = getsibling(block)
            if sibling !== nothing
                @check getid(sibling) == getid(block)
                @check getpool(sibling) != getpool(block)
                @check getsibling(sibling) == block

                delete!(M.policy, sibling)
                unsafe_free_direct(M, sibling)
            end
            unsafe_unregister!(M, block)
            unsafe_free_direct(M, block)
        end
        empty!(cleanlist)

        # Process other buffer as well.
        # Safe to do since we already hold the "remove_lock".
        # The next trip around the loop will be with the new buffer.
        if candrain(M.freebuffer)
            unsafe_swap!(M.freebuffer)
        else
            break
        end
    end
    return id_cleaned
end

#####
##### Policy Level Allocations
#####

function unsafe_alloc_through_policy(
    manager::CacheManager,
    bytes,
    priority::AllocationPriority,
    id = getid(manager),
)
    # Only attempt draining at the beginning of an allocation round.
    candrain(manager.freebuffer) && unsafe_cleanup!(manager)
    ptr = policy_new_alloc(manager.policy, manager, bytes, id, priority)
    ptr === nothing && error("Allocation failed!")
    return ptr
end

# Alloc through the policy
function alloc(
    manager::CacheManager,
    bytes::Int,
    priority::AllocationPriority = PreferLocal,
    id::UInt = getid(manager),
)
    @spinlock alloc_lock(manager) begin
        # TODO: What to do about zero sized allocations ...
        ptr =
            iszero(bytes) ? Ptr{Nothing}() :
            unsafe_alloc_through_policy(manager, bytes, priority, id)
        return Object(ptr, manager)
    end
end

#####
##### Top level Frees
#####

# Note: `push!` for the `FreeBuffer` is thread safe, so no synchonizations needs
# to happen at this level.
@static if ALLOW_UNSAFE_FREE
    # If we allow "unsafe_free", then the pointer in a object will be replaced by
    # a null pointer, in which case we definitely don't want to add this to the
    # free buffer.
    function free(manager::CacheManager, ptr::Ptr)
        isnull(ptr) || free(manager, unsafe_block(ptr))
    end

    free(manager::CacheManager, block::Block) = push!(manager.freebuffer, block)

    # TODO: What if this is called while movement is happening ...
    function unsafe_free(object::Object)
        ptr = unsafe_pointer(object)
        ptrptr = Ptr{Ptr{Nothing}}(blockpointer(object))
        old = atomic_ptr_xchg!(ptrptr, Ptr{Nothing}())
        isnull(old) || free(manager(object), ptr)
    end
else
    free(manager::CacheManager, ptr::Ptr) = push!(manager.freebuffer, unsafe_block(ptr))
    unsafe_free(::Object) = nothing
end

#####
##### Set primary
#####

"""
    unsafe_setprimary!(manager::CacheManager, current::Block, next::Block)::Union{Block, Nothing}

Switch a primary segment from `current` to `next`.
This operation may fail if `current` has been garbage collected.
If this is the case, this function will return `nothing`.
"""
function unsafe_setprimary!(
    manager::CacheManager,
    current::Block,
    next::Block;
    unsafe = false,
)
    # Need to check if the current is queued for cleanup.
    # If so, this means that the backedge is invalid.
    isqueued(current) && return nothing
    id = getid(current)
    unsafe || @check getid(next) == id
    map = getmap(manager)
    primary, backedge = map[id]
    @check primary == current

    # Small change GC ran. Make sure that `current` is still not queued before comitting.
    isqueued(primary) && return nothing
    old = atomic_ptr_xchg!(backedge, datapointer(next))
    map[id] = (next, backedge)

    # Check that in fact the passed `current` block IS the primary segment.
    @check old == datapointer(current)
    return old
end

function unsafe_clearprimary!(manager::CacheManager, current::Block)
    isqueued(current) && return nothing
    id = getid(current)
    map = getmap(manager)

    _, backedge = map[id]
    old = atomic_ptr_xchg!(backedge, Ptr{Nothing}())
    map[id] = (Block(), backedge)

    @check old == datapointer(current)
    return old
end

getprimary(manager::CacheManager, block::Block) = getprimary(manager, getid(block))
getprimary(manager::CacheManager, id::UInt) = first(getmap(manager, id))

#####
##### Copyto!
#####

function Base.copyto!(dst::Block, src::Block, manager::CacheManager; include_header = false)
    nthreads = getpool(dst) == Remote ? 4 : Threads.nthreads()
    @telemetry manager telemetry_move(
        gettelemetry(manager),
        getid(src),
        getpool(dst),
        sizeof(src),
    )

    if include_header
        _memcpy!(pointer(dst), pointer(src), length(src); nthreads)
    else
        _memcpy!(datapointer(dst), datapointer(src), length(src); nthreads)
    end
    return nothing
end

#####
##### Utils
#####

function visible_ids(manager::CacheManager, pool; primary_only = false)
    ids = Set{UInt64}()
    for (id, bb) in getmap(manager)
        _, ptr = bb
        block = unsafe_block(ptr)
        sibling = getsibling(block)
        if getpool(block) == pool
            push!(ids, id)
        elseif !primary_only && (sibling !== nothing)
            @check getpool(sibling) == pool
            push!(ids, id)
        end
    end
    return ids
end

#####
##### Policy
#####

@inline function setdirty!(manager::CacheManager, block::Block, flag = true)
    return setdirty!(manager.policy, block, flag)
end

update!(manager::CacheManager, block::Block) = update!(manager.policy, block)

