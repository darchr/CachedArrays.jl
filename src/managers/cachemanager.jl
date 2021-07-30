mutable struct Object{T}
    # TODO: Store the allocation function in the block header to avoid taking space in
    # the object struct?
    ptr::Ptr{Nothing}
    manager::T

    function Object(ptr::Ptr{Nothing}, manager::T) where {T}
        object = new{T}(ptr, manager)
        if !isnull(ptr)
            unsafe_register!(manager, object)
            finalizer(free, object)
        end
        return object
    end
end

Base.pointer(object::Object) = object.ptr
unsafe_pointer(object::Object) = object.ptr
blockpointer(object::Object) = pointer_from_objref(object)

free(object::Object) = free(manager(object), unsafe_pointer(object))
metastyle(::Object) = BlockMeta()
manager(object::Object) = object.manager

"""
$(TYPEDSIGNATURES)

Allocate `bytes` from `objects`'s manager.
If `id` is not given, it will be selected automatically.
"""
function alloc(
    object::Object,
    bytes::Integer,
    priority::AllocationPriority = PreferLocal,
    id = getid(object.manager),
)
    return alloc(manager(object), bytes, priority, id)
end

#####
##### Backedges
#####

# Back edges from the manager to the GC managed object holding a block.
# Kept as a raw Ptr{Ptr} to avoid GC cycles.
const Backedge = Ptr{Ptr{Nothing}}

backedge(x::Backedge) = x
backedge(x::Ptr) = convert(Backedge, x)
backedge(x) = backedge(blockpointer(x))

mutable struct BackedgeMap
    dict::Dict{UInt,Backedge}
    size::Int
end

BackedgeMap() = BackedgeMap(Dict{UInt,Backedge}(), 0)

Base.getindex(map::BackedgeMap, id) = map.dict[id]
function set!(map::BackedgeMap, backedge::Backedge, id, sz::Integer)
    @check !in(id, map)
    map.dict[id] = backedge
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
Base.length(map::BackedgeMap) = length(map.dict)
Base.isempty(map::BackedgeMap) = isempty(map.dict)
Base.keys(map::BackedgeMap) = keys(map.dict)

#####
##### Cache Manager
#####

const RemotePool = PoolType{Remote}
const LocalPool = PoolType{Local}

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
    localsize = 1_000_000_000,
    remotesize = 1_000_000_000,
    gc_when_over = 0.95,
    minallocation = 10,
    policy = OptaneTracker((2^minallocation,)),
    telemetry = NoTelemetry(),
)
    map = BackedgeMap()

    # Initialize Heaps
    remote_heap = CompactHeap(
        remote_allocator,
        remotesize;
        pool = Remote,
        minallocation = minallocation,
    )

    local_heap = CompactHeap(
        local_allocator,
        localsize;
        pool = Local,
        minallocation = minallocation,
    )

    # Construct the manager.
    manager = CacheManager(
        map,
        policy,
        remote_heap,
        local_heap,
        Threads.Atomic{UInt64}(1),
        Threads.SpinLock(),
        FreeBuffer{Block}(),

        # tunables,
        # AbortCallback(local_heap, 0),

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
    return isempty(manager.map)
end

function Base.show(io::IO, M::CacheManager)
    println(io, "Cache Manager")
    println(io, "    $(length(M.map)) Objects")
    println(io, "    $(getsize(M.localmap) / 1E9) GB Memory Used.")
end

#####
##### Telemetry
#####

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

# TODO: Finalize API
function telemetry_alloc end
function telemetry_gc end
function telemetry_change end
function telemetry_move end

#####
##### Alloc and Free
#####

function alloc(
    manager::CacheManager,
    bytes::Int,
    priority::AllocationPriority = PreferLocal,
    id::UInt = getid(manager),
)
    return @spinlock alloc_lock(manager) begin
        ptr = iszero(bytes) ? Ptr{Nothing}() : unsafe_alloc(manager, bytes, priority, id)
        return Object(ptr, manager)
    end
end

# Note: `push!` for the `FreeBuffer` is thread safe, so no synchonizations needs
# to happen at this level.
@static if ALLOW_UNSAFE_FREE
    # If we allow "unsafe_free", then the pointer in a object will be replaced by
    # a null pointer, in which case we definitely don't want to add this to the
    # free buffer.
    function free(manager::CacheManager, ptr::Ptr)
        isnull(ptr) || push!(manager.freebuffer, unsafe_block(ptr))
    end

    # TODO: What if this is called while movement is happening ...
    function unsafe_free(object::Object)
        ptr = unsafe_pointer(object)

        ptrptr = Ptr{Ptr{Nothing}}(blockpointer(object))
        old = atomic_ptr_xchg!(ptrptr, Ptr{Nothing}())
        isnull(old) || free(manager(object), ptr)
    end
else
    free(manager::CacheManager, ptr::Ptr) = push!(manager.freebuffer, unsafe_block(ptr))
    # no-op
    unsafe_free(object::Object) = nothing
end

# """
# $(TYPEDSIGNATURES)
#
# Perform some kind of movement on `x` with regards to `pool`. The exact sequence of
# actions depends on the keywords provided. No kind of action is taken if `x` already
# resides in `pool`.
#
# If a sibling of `x` in `pool` does not already exist, then one will be created.
#
# ## Keywords
# * `copydata::Bool` - Copy the contents of `x` to its sibling in `pool`.
# * `updatebackedge::Bool` - Update the managers backedge to the struct `r` containing `x`
#     so `r` points to `x`'s sibling in `pool`.
# * `freeblock::Bool` - If true, `x` will be removed from `pool`. Note, `freeblock = true`
#     automatically applies `updatebackedge = true`.
# * `canabort::Callable` - Callback to notify potential eviction routines that get calledc
#     as a side effect of `actuate!` to indicate that early termination is acceptable.
# """
# actuate!(pool, x, m = manager(x); kw...) = actuate!(pool, metadata(x), m; kw...)

#####
##### Internal Cache Manager Functions
#####

# """
# $(TYPEDSIGNATURES)
#
# Register `allocated_pointer` and `backedge` with `manager`.
# Argument `allocated_pointer` must come from a heap owned by `manager` and `backedge` must
# point to the `Object` (i.e. mutable struct) holding `allocated_pointer`.
#
# The best way to obtain `backedge` is `Base.pointer_from_objref(object)` where
# `object::Object`.
# """
# function unsafe_register!(
#     manager::CacheManager,
#     object::Object,
# )
#     block = metadata(object)
#     # Register with the appropriate pool.
#     # Statically dispatch to avoid a potential allocation.
#     p = block.pool
#     if p == Local
#         unsafe_register!(LocalPool(), manager, object)
#     elseif p == Remote
#         unsafe_register!(RemotePool(), manager, object)
#     else
#         error("Unknown Pool: $p")
#     end
#     return nothing
# end

# """
# $(TYPEDSIGNATURES)
#
# Register `block` and `backedge` to `pool` in `manager`.
# `block` must have been allocated from the corresponding `pool` and `backedge` must point
# to the `Object` holding the pointer associated with `block`.
# """
function unsafe_register!(
    manager::CacheManager,
    object::Object,
) where {T}
    @requires alloc_lock(manager)
    block = metadata(object)
    set!(getmap(manager), backedge(object), getid(block), length(block))
    push!(manager.policy, block)
    return nothing
end

update!(::LocalPool, M::CacheManager, A) = update!(M.policy, metadata(A))

function unsafe_unregister!(
    manager::CacheManager,
    block::Block,
) where {T}
    @requires alloc_lock(manager)
    delete!(getmap(manager), getid(block), length(block))
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
prepare_cleanup!(manager::CacheManager) = unsafe_swap!(manager.freebuffer)
function unsafe_cleanup!(M::CacheManager, id = nothing)
    @requires alloc_lock(M)
    prepare_cleanup!(M)
    id_cleaned = false

    while true
        cleanlist = unsafe_get(M.freebuffer)

        # Free all blocks in the cleanlist
        for block in cleanlist
            @check !isfree(block) || block.evicting

            id == getid(block) && (id_cleaned = true)
            @telemetry M telemetry_gc(gettelemetry(M), getid(block))

            sibling = getsibling(block)
            if sibling !== nothing
                @check getid(sibling) == getid(block)
                @check getpool(sibling) != getpool(block)
                free(M, sibling)
            end
            unsafe_unregister!(M, block)
            free(M, block)
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
##### `alloc` entry point
#####

function unsafe_alloc(
    manager::CacheManager,
    bytes,
    priority::AllocationPriority,
    id = getid(manager),
)
    # Only attempt draining at the beginning of an allocation round.
    candrain(manager.freebuffer) && unsafe_cleanup!(manager)
    return policy_new_alloc(manager.policy, manager, bytes, id, priority)
end

function unsafe_alloc(pool::PoolType, manager::CacheManager, bytes, id::UInt)
    @requires alloc_lock(manager)
    return alloc(getheap(heap, pool), bytes, id)
end

function free(manager::CacheManager, block::Block)
    ptr = datapointer(block)
    pool = getpool(block)
    if pool == Local
        free(getheap(manager, LocalPool), ptr)
    elseif pool == Remote
        free(getheap(manager, RemotePool), ptr)
    end
    return nothing
end

#####
##### Set primary
#####

function setprimary!(manager::CacheManager, block::Block)
    backedge = getmap(manager, getid(block))
    return atomic_ptr_xchg!(backedge, datapointer(block))
end

#####
##### Copyto!
#####

function Base.copyto!(dst::Block, src::Block, manager::CacheManager; include_header = false)
    numthreads = getpool(dst) == Remote ? 4 : Threads.nthreads()
    if include_header
        _memcpy!(pointer(dst), pointer(src), length(src); nthreads)
    else
        _memcpy!(datapointer(dst), datapointer(src), length(src); nthreads)
    end
    return nothing
end

# #####
# ##### Moving Objects
# #####
#
# # TODO: Generalize?
# poolname(::PoolType{T}) where {T} = T
# complement(::PoolType{Local}) = PoolType{Remote}()
# complement(::PoolType{Remote}) = PoolType{Local}()
#
# function actuate!(
#     pool::PoolType{T},
#     block::Block,
#     manager::CacheManager;
#     # Steps to take
#     copydata = true,
#     updatebackedge = true,
#     freeblock = true,
#     # Callback for early termination.
#     canabort = alwaysfalse,
# ) where {T}
#     @requires alloc_lock(manager) #remove_lock(manager.freebuffer)
#
#     # If the data is already local, mark a usage and return.
#     if getpool(block) == T
#         # TODO: Fix me!
#         #update!(PoolType{Local}(), manager, A)
#         return nothing
#     end
#
#     # Either create or find the sibling block in the other pool.
#     sibling = getsibling(block)
#     createsibling = (sibling === nothing)
#     id = getid(block)
#     if createsibling
#         sibling_ptr = unsafe_alloc(
#             pool,
#             manager,
#             length(block),
#             id;
#             canabort = canabort,
#         )
#
#         # Cleanup happened and we have enough space to fulfill the original request.
#         # This only happens if "canabort" returs true
#         sibling_ptr === nothing && return true
#
#         # Note: Checked allocation will return a null pointer if the requested ID was
#         # freed during a cleanup trying to service this allocation.
#         #
#         # If that's the case, then we don't need to actually bother moving this data block
#         # because, well, it's free!
#         isnull(sibling_ptr) && return nothing
#         sibling = unsafe_block(sibling_ptr)
#     else
#         sibling_ptr = datapointer(sibling)
#     end
#
#     # Copy data to sibling.
#     # Otherwise, assume that the newblock is dirty and it will be written.
#     if copydata
#         nthreads = (T == Local) ? Threads.nthreads() : 4
#         _memcpy!(sibling_ptr, datapointer(block), length(block); nthreads)
#
#         # Performing the memcpy may trigger garbage collection.
#         # Since we hold the remove-lock - no one else has tried to cleanup the manager.
#         # It's possible the block we just moved has now been queued for deletion.
#         # Check that case.
#         if isqueued(block)
#             unsafe_cleanup!(manager)
#             # Abort operation
#             createsibling && free(pool, manager, sibling)
#             return canabort() ? true : nothing
#         end
#     else
#         setdirty!(newblock, flag)
#         setdirty!(manager.policy, newblock, flag)
#     end
#
#     # Update backedge to visible struct if necessary.
#     # Note: `freeblock` automatically implies `updatebackedge`.
#     updatebackedge = updatebackedge || freeblock
#     if createsibling || updatebackedge
#         backedge = getmap(manager, complement(pool), id)
#         if updatebackedge
#             # Make sure no funny business is happening!
#             old = atomic_ptr_xchg!(backedge, sibling_ptr)
#             @check old == datapointer(block)
#         end
#         createsibling && unsafe_register!(pool, manager, sibling, backedge)
#     end
#
#     # Perform necessary cleanup or linking.
#     if freeblock
#         # In the case where we're copying back to an existing sibling, but freeing
#         # the current block, we need to clear the "sibling" field of the metadata block.
#         setsibling!(sibling, nothing)
#         unsafe_unregister!(complement(pool), manager, block)
#         free(complement(pool), manager, block)
#     elseif createsibling
#         setsibling!(sibling, block)
#         setsibling!(block, sibling)
#     end
#
#     @telemetry manager telemetry_move(gettelemetry(manager), id, T, length(block))
#     return nothing
# end

#####
##### Policy
#####

@inline function setdirty!(manager::CacheManager, block::Block, flag = true)
    return setdirty!(manager.policy, block, flag)
end

update!(manager::CacheManager, block::Block) = update!(manager.policy, block)

