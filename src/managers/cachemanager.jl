mutable struct Region{T}
    # TODO: Store the allocation function in the block header to avoid taking space in
    # the region struct?
    ptr::Ptr{Nothing}
    manager::T

    function Region(ptr::Ptr{Nothing}, manager::T) where {T}
        region = new{T}(ptr, manager)
        unsafe_register!(manager, backedge(region), ptr)
        finalizer(free, region)
        return region
    end
end

Base.pointer(region::Region) = region.ptr
datapointer(region::Region) = pointer_from_objref(region)

free(region::Region) = free(manager(region), pointer(region))
metastyle(::Region) = BlockMeta()
manager(region::Region) = region.manager

"""
$(TYPEDSIGNATURES)

Allocate `bytes` from `regions`'s manager.
If `id` is not given, it will be selected automatically.
"""
function alloc(
    region::Region,
    bytes::Integer,
    priority::AllocationPriority = PreferLocal,
    id = getid(region.manager),
)
    return alloc(manager(region), bytes, priority, id)
end

#####
##### Backedges
#####

# Back edges from the manager to the GC managed object holding a block.
# Kept as a raw Ptr{Ptr} to avoid GC cycles.
const Backedge = Ptr{Ptr{Nothing}}

backedge(x::Backedge) = x
backedge(x::Ptr) = convert(Backedge, x)
backedge(x) = backedge(datapointer(x))

struct BackedgeMap
    dict::Dict{UInt,Backedge}
    size::Ref{Int}
end

BackedgeMap() = BackedgeMap(Dict{UInt,Backedge}(), 0)

Base.getindex(map::BackedgeMap, id) = map.dict[id]
function set!(map::BackedgeMap, backedge::Backedge, id, sz::Integer)
    map.dict[id] = backedge
    map.size[] += sz
    return backedge
end

function Base.delete!(map::BackedgeMap, id::UInt, sz::Integer)
    delete!(map.dict, id)
    @check map.size[] >= sz
    map.size[] -= sz
    return nothing
end

getsize(map::BackedgeMap) = map.size[]
Base.in(id, map::BackedgeMap) = haskey(map.dict, id)
Base.length(map::BackedgeMap) = length(map.dict)
Base.isempty(map::BackedgeMap) = isempty(map.dict)
Base.keys(map::BackedgeMap) = keys(map.dict)

#####
##### Aborting Calllbacks
#####

mutable struct AbortCallback{H}
    heap::CompactHeap{H}
    bytes::Int
end

(f::AbortCallback)() = canalloc(f.heap, f.bytes)

#####
##### Cache Manager
#####

const RemotePool = PoolType{Remote}
const LocalPool = PoolType{Local}

struct NoTelemetry end
mutable struct CacheManager{C,T}
    localmap::BackedgeMap
    remotemap::BackedgeMap

    # local datastructures
    policy::C
    remote_heap::CompactHeap{MmapAllocator}
    local_heap::CompactHeap{AlignedAllocator}

    # Create a new ID for each object registered in the cache.
    idcount::Threads.Atomic{UInt64}

    # Synchronize access
    alloc_lock::Base.Threads.SpinLock
    freebuffer::FreeBuffer{Block}

    ## local tunables
    #gc_when_over_local::Int
    #gc_when_over_remote::Int
    abort_callback::AbortCallback{AlignedAllocator}

    ## Prevent arrays from moving
    #allow_movement::Bool
    telemetry::T
end

getmap(manager::CacheManager, pool, id) = getindex(getmap(manager, pool), id)

getheap(manager::CacheManager, ::PoolType{Local}) = manager.local_heap
getmap(manager::CacheManager, ::PoolType{Local}) = manager.localmap

getheap(manager::CacheManager, ::PoolType{Remote}) = manager.remote_heap
getmap(manager::CacheManager, ::PoolType{Remote}) = manager.remotemap


"""
$(TYPEDSIGNATURES)

Returns the allocation lock for `manager` without acquiring it.
"""
alloc_lock(manager::CacheManager) = manager.alloc_lock

mb(x) = x * 1_000_000
function CacheManager(
    path::AbstractString;
    localsize = 1_000_000_000,
    remotesize = 1_000_000_000,
    gc_when_over = 0.95,
    minallocation = 10,
    policy = OptaneTracker((2^minallocation, mb(4), mb(8), mb(16), mb(32))),
    telemetry = NoTelemetry(),
)
    localmap = BackedgeMap()
    remotemap = BackedgeMap()

    # Initialize Heaps
    remote_heap = CompactHeap(
        MmapAllocator(path),
        remotesize;
        pool = Remote,
        minallocation = minallocation,
    )

    local_heap = CompactHeap(
        AlignedAllocator(),
        localsize;
        pool = Local,
        minallocation = minallocation,
    )

    # Allow movement by default
    allow_movement = true

    # Construct the manager.
    manager = CacheManager(
        localmap,
        remotemap,
        policy,
        remote_heap,
        local_heap,
        Threads.Atomic{UInt64}(1),
        Threads.SpinLock(),
        FreeBuffer{Block}(),

        # tunables,
        AbortCallback(local_heap, 0),

        # telemetry
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
    return isempty(manager.localmap) && isempty(manager.remotemap)
end

function Base.show(io::IO, M::CacheManager)
    println(io, "Cache Manager")
    println(io, "    $(length(M.localmap)) Local Objects")
    println(io, "    $(length(M.remotemap)) Remote Objects")
    println(
        io,
        "    $(getsize(M.localmap) / 1E9) GB Local Memory Used of $(M.local_heap.len / 1E9)",
    )
    println(
        io,
        "    $(getsize(M.remotemap) / 1E9) GB Remote Memory Used of $(M.remote_heap.len / 1E9)",
    )
end

#####
##### Telemetry
#####

gettelemetry(manager::CacheManager) = manager.telemetry
telemetry_enabled(::CacheManager) = true
telemetry_enabled(::CacheManager{<:Any,NoTelemetry}) = false

macro telemetry(manager, expr)
    return quote
        if telemetry_enabled($(esc(manager)))
            $(esc(expr))
        end
    end
end

# TODO: Finalize API
function telemetry_alloc end
function telemetry_gc end
function telemetry_change end
function telemetry_move end

#####
##### Enable/Disable movement
#####

enable_movement!(M::CacheManager) = (M.allow_movement = true)
disable_movement!(M::CacheManager) = (M.allow_movement = false)

#####
##### Actuators
#####

# Chain of allocation function calls.
# - "alloc": Top level, acquires necessary locks and wraps the returned pointer
#   in a "Region" struct.
#
# - "unsafe_alloc": Like "alloc" but assumes necessary locks are held and just returns
#   a raw pointer.
#
# - "_pool_alloc": Lower level allocation function that performs allocation from a
#   specific heap. Logic at this layer includes potentially invoking the GC, performing
#   eviction, etc.
#
# - "alloc (from a heap)": Actually requests memory from a heap.
function alloc(
    manager::CacheManager,
    bytes::Int,
    priority::AllocationPriority = PreferLocal,
    id::UInt = getid(manager),
)
    return @spinlock alloc_lock(manager) begin
        ptr = unsafe_alloc(manager, bytes, priority, id)
        return Region(ptr, manager)
    end
end

# Note: `push!` for the `FreeBuffer` is thread safe, so no synchonizations needs
# to happen at this level.
@static if ALLOW_UNSAFE_FREE
    # If we allow "unsafe_free", then the pointer in a region will be replaced by
    # a null pointer, in which case we definitely don't want to add this to the
    # free buffer.
    function free(manager::CacheManager, ptr::Ptr)
        isnull(ptr) || push!(manager.freebuffer, unsafe_block(ptr))
    end

    # TODO: What if this is called while movement is happening ...
    function unsafe_free(region::Region)
        ptr = pointer(region)
        old = atomic_ptr_xchg!(datapointer(region), Ptr{Nothing}())
        isnull(old) || free(manager, ptr)
    end
else
    free(manager::CacheManager, ptr::Ptr) = push!(manager.freebuffer, unsafe_block(ptr))
    # no-op
    unsafe_free(region::Region) = nothing
end

"""
$(TYPEDSIGNATURES)

Perform some kind of movement on `x` with regards to `pool`. The exact sequence of
actions depends on the keywords provided. No kind of action is taken if `x` already
resides in `pool`.

If a sibling of `x` in `pool` does not already exist, then one will be created.

## Keywords
* `copydata::Bool` - Copy the contents of `x` to its sibling in `pool`.
* `updatebackedge::Bool` - Update the managers backedge to the struct `r` containing `x`
    so `r` points to `x`'s sibling in `pool`.
* `freeblock::Bool` - If true, `x` will be removed from `pool`. Note, `freeblock = true`
    automatically applies `updatebackedge = true`.
* `canabort::Callable` - Callback to notify potential eviction routines that get calledc
    as a side effect of `actuate!` to indicate that early termination is acceptable.
"""
actuate!(pool, x, m = manager(x); kw...) = actuate!(pool, metadata(x), m; kw...)

#####
##### Internal Cache Manager Functions
#####

"""
$(TYPEDSIGNATURES)

Register `allocated_pointer` and `backedge` with `manager`.
Argument `allocated_pointer` must come from a heap owned by `manager` and `backedge` must
point to the `Region` (i.e. mutable struct) holding `allocated_pointer`.

The best way to obtain `backedge` is `Base.pointer_from_objref(region)` where
`region::Region`.
"""
function unsafe_register!(
    manager::CacheManager,
    backedge::Backedge,
    allocated_pointer::Ptr{Nothing},
)
    block = unsafe_block(allocated_pointer)
    # Register with the appropriate pool.
    # Statically dispatch to avoid a potential allocation.
    p = block.pool
    if p == Local
        unsafe_register!(LocalPool(), manager, block, backedge)
    elseif p == Remote
        unsafe_register!(RemotePool(), manager, block, backedge)
    else
        error("Unknown Pool: $p")
    end
    return nothing
end

"""
$(TYPEDSIGNATURES)

Register `block` and `backedge` to `pool` in `manager`.
`block` must have been allocated from the corresponding `pool` and `backedge` must point
to the `Region` holding the pointer associated with `block`.
"""
function unsafe_register!(
    pool::PoolType{T},
    manager::CacheManager,
    block::Block,
    backedge::Backedge,
) where {T}
    @requires alloc_lock(manager)
    @check block.pool == T
    id = getid(block)
    map = getmap(manager, pool)
    @check !in(id, map)
    set!(map, backedge, id, length(block))
    push!(manager.policy, block, T)

    return nothing
end

update!(::LocalPool, M::CacheManager, A) = update!(M.policy, metadata(A))

# Top level entry points
function unregister!(pool, M::CacheManager, A)
    @spinlock alloc_lock(M) unsafe_unregister!(pool, M, metadata(A))
    return nothing
end

function unsafe_unregister!(
    pool::PoolType{T},
    manager::CacheManager,
    block::Block,
) where {T}
    @requires alloc_lock(manager)
    delete!(getmap(manager, pool), getid(block), length(block))
    delete!(manager.policy, block)
    return nothing
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

# Have an optional, ignored argument for a similar signature to `unsafe_cleanup!`.
# function cleanup(manager::CacheManager, _id::Union{Integer,Nothing} = nothing)
#     @spinlock remove_lock(manager.freebuffer) begin
#         prepare_cleanup!(manager)
#         unsafe_cleanup!(manager)
#     end
# end

prepare_cleanup!(manager::CacheManager) = unsafe_swap!(manager.freebuffer)

# N.B. - `free_lock` must be held to call this.
const TIMES = UInt64[]
function unsafe_cleanup!(M::CacheManager, id = nothing)
    @requires alloc_lock(M) #remove_lock(M.freebuffer)
    prepare_cleanup!(M)
    start = time_ns()
    id_cleaned = false

    while true
        cleanlist = unsafe_get(M.freebuffer)

        # Free all blocks in the cleanlist
        for block in cleanlist
            @check !isfree(block) || block.evicting

            # If this block is in Remote, make sure it doesn't have a sibling - otherwise, that would
            # be an error.
            id == getid(block) && (id_cleaned = true)
            @telemetry M telemetry_gc(gettelemetry(M), getid(block))

            pool = getpool(block)
            if pool == Remote
                # TODO: Need to remove this check.
                @check getsibling(block) === nothing

                # Deregister from Remote. If there are zero remaining references,
                # then we free the block
                unsafe_unregister!(RemotePool(), M, block)
                free(RemotePool(), M, block)
            elseif pool == Local
                unsafe_unregister!(LocalPool(), M, block)

                # Does this block have a sibling?
                sibling = getsibling(block)
                if sibling !== nothing
                    @check getid(block) == getid(sibling)
                    @check getpool(sibling) == Remote

                    # use `unsafe_unregister!` to completely remove this block.
                    unsafe_unregister!(RemotePool(), M, block)
                    free(RemotePool(), M, sibling)
                end
                free(LocalPool(), M, block)
            end
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
    push!(TIMES, time_ns() - start)
    return id_cleaned
end

#####
##### `alloc` entry point
#####

function unsafe_alloc(
    manager::CacheManager,
    bytes,
    priority::AllocationPriority,
    id = getid(manager),
)
    return policy_new_alloc(manager.policy, manager, bytes, id, priority)
end

function unsafe_alloc(
    pool::PoolType{T},
    manager::CacheManager,
    bytes,
    id::UInt = getid(manager);
    canabort = alwaysfalse
    # eviction_function::G = doeviction!,
    # kw...,
) where {T,F}
    @requires alloc_lock(manager)
    heap = getheap(manager, pool)

    # Try to cleanup the freebuffer.
    # The cleanup function *may* take an ID, in which case it will return "true" if
    # the id we're trying to allocate was freed as part of the cleanup process.
    #
    # If this is the case, then we simply abort.
    if candrain(manager.freebuffer)
        # If this block gets cleaned up, then first check if it's okay to abort
        # the eviction operation.
        #
        # If so, return `nothing` do indicate such.
        #
        # If it is NOT okay to abort the eviction operation, but the block was
        # cleaned, return a null ptr.
        cleaned = unsafe_cleanup!(manager, id)
        canabort() && return nothing
        cleaned && return Ptr{Nothing}()
    end

    return alloc(heap, bytes, id)
end

# Dispatch plumbing!
#
# Try to ultimately convert an object to a pointer to the start of the user accessible
# data region.
_free_convert(x::Ptr{Nothing}) = x
_free_convert(x::Ptr) = convert(Ptr{Nothing}, x)
_free_convert(x::Block) = _free_convert(datapointer(x))

function free(pool::PoolType, manager::CacheManager, x)
    return free(getheap(manager, pool), _free_convert(x))
end

#####
##### Moving Objects
#####

# TODO: Generalize?
poolname(::PoolType{T}) where {T} = T
complement(::PoolType{Local}) = PoolType{Remote}()
complement(::PoolType{Remote}) = PoolType{Local}()

function actuate!(
    pool::PoolType{T},
    block::Block,
    manager::CacheManager;
    # Steps to take
    copydata = true,
    updatebackedge = true,
    freeblock = true,
    # Callback for early termination.
    canabort = alwaysfalse,
) where {T}
    @requires alloc_lock(manager) #remove_lock(manager.freebuffer)

    # If the data is already local, mark a usage and return.
    if getpool(block) == T
        # TODO: Fix me!
        #update!(PoolType{Local}(), manager, A)
        return nothing
    end

    # Either create or find the sibling block in the other pool.
    sibling = getsibling(block)
    createsibling = (sibling === nothing)
    id = getid(block)
    if createsibling
        sibling_ptr = unsafe_alloc(
            pool,
            manager,
            length(block),
            id;
            canabort = canabort,
        )

        # Cleanup happened and we have enough space to fulfill the original request.
        # This only happens if "canabort" returs true
        sibling_ptr === nothing && return true

        # Note: Checked allocation will return a null pointer if the requested ID was
        # freed during a cleanup trying to service this allocation.
        #
        # If that's the case, then we don't need to actually bother moving this data block
        # because, well, it's free!
        isnull(sibling_ptr) && return nothing
        sibling = unsafe_block(sibling_ptr)
    else
        sibling_ptr = datapointer(sibling)
    end

    # Copy data to sibling.
    # Otherwise, assume that the newblock is dirty and it will be written.
    if copydata
        _memcpy!(sibling_ptr, datapointer(block), length(block))

        # Performing the memcpy may trigger garbage collection.
        # Since we hold the remove-lock - no one else has tried to cleanup the manager.
        # It's possible the block we just moved has now been queued for deletion.
        # Check that case.
        if isqueued(block)
            unsafe_cleanup!(manager)
            # Abort operation
            createsibling && free(pool, manager, sibling)
            return canabort() ? true : nothing
        end
    else
        setdirty!(newblock, flag)
        setdirty!(manager.policy, newblock, flag)
    end

    # Update backedge to visible struct if necessary.
    # Note: `freeblock` automatically implies `updatebackedge`.
    updatebackedge = updatebackedge || freeblock
    if createsibling || updatebackedge
        backedge = getmap(manager, complement(pool), id)
        if updatebackedge
            # Make sure no funny business is happening!
            old = atomic_ptr_xchg!(backedge, sibling_ptr)
            @check old == datapointer(block)
        end
        createsibling && unsafe_register!(pool, manager, sibling, backedge)
    end

    # Perform necessary cleanup or linking.
    if freeblock
        # In the case where we're copying back to an existing sibling, but freeing
        # the current block, we need to clear the "sibling" field of the metadata block.
        setsibling!(sibling, nothing)
        unsafe_unregister!(complement(pool), manager, block)
        free(complement(pool), manager, block)
    elseif createsibling
        setsibling!(sibling, block)
        setsibling!(block, sibling)
    end

    @telemetry manager telemetry_move(gettelemetry(manager), id, T, length(block))
    return nothing
end

#####
##### Policy
#####

@inline function setdirty!(manager::CacheManager, block::Block, flag = true)
    return setdirty!(manager.policy, block, flag)
end

update!(manager::CacheManager, block::Block) = update!(manager.policy, block)

