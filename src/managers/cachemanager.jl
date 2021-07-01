# Back edges from the manager to the GC managed object holding a block.
# Kept as a raw Ptr{Ptr} to avoid GC cycles.
const Backedge = Ptr{Ptr{Nothing}}

backedge(x::Backedge) = x
backedge(x::Ptr) = convert(Backedge, x)
backedge(x) = backedge(datapointer(x))

@enum AllocationPriority ForceLocal PreferLocal ForceRemote

mutable struct Region{T}
    # TODO: Store the allocation function in the block header to avoid taking space in
    # the region struct?
    ptr::Ptr{Nothing}
    manager::T

    function Region(ptr::Ptr{Nothing}, manager::T) where {T}
        region = new{T}(ptr, manager)
        register!(manager, backedge(region), ptr)
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
    ptr = @spinlock alloc_lock(manager(region)) unsafe_alloc(
        manager(region),
        bytes,
        priority,
        id,
    )
    return Region(ptr, manager(region))
end

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
    # Reference to local objecTts
    # NOTE: These need to be pointers and not `Region`s because we don't want the
    # the manager to protect these objects from being garbage collected.
    local_objects::Dict{UInt,Backedge}
    size_of_local::Int

    # Create a new ID for each object registered in the cache.
    idcount::Threads.Atomic{UInt64}

    # All objects with remote memory.
    remote_objects::Dict{UInt,Backedge}
    size_of_remote::Int

    # local datastructures
    policy::C
    remote_heap::CompactHeap{MmapAllocator}
    local_heap::CompactHeap{AlignedAllocator}

    # Synchronize access
    alloc_lock::Base.Threads.SpinLock
    freebuffer::FreeBuffer{Block}

    ## local tunables
    gc_when_over_local::Int
    gc_when_over_remote::Int
    abort_callback::AbortCallback{AlignedAllocator}

    ## Prevent arrays from moving
    allow_movement::Bool
    telemetry::T
end

"""
$(TYPEDSIGNATURES)

Returns the allocation lock for `manager` without acquiring it.
"""
alloc_lock(manager::CacheManager) = manager.alloc_lock

function CacheManager(
    path::AbstractString;
    localsize = 1_000_000_000,
    remotesize = 1_000_000_000,
    policy = LRU{Block}(),
    gc_when_over = 0.95,
    minallocation = 10,
    telemetry = NoTelemetry(),
)
    # If we're in 2LM, pass a nullptr.
    # MemKindAllocator gets swapped to something that throws an error if called.

    local_objects = Dict{UInt,Backedge}()
    size_of_local = 0

    remote_objects = Dict{UInt,Backedge}()
    size_of_remote = 0

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
        local_objects,
        size_of_local,
        Threads.Atomic{UInt64}(1),
        remote_objects,
        size_of_remote,
        policy,
        remote_heap,
        local_heap,
        Threads.SpinLock(),
        FreeBuffer{Block}(),

        # tunables,
        floor(Int, gc_when_over * sizeof(local_heap)),
        floor(Int, gc_when_over * sizeof(remote_heap)),
        AbortCallback(local_heap, 0),

        # runtime settings
        allow_movement,

        # telemetry
        telemetry,
    )

    # Add this to the global manager list to ensure that it outlives any of its users.
    push!(GlobalManagers, manager)
    return manager
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

# Try to allocate from Local first.
# Then, try to allocate from Remote
function alloc(
    manager::CacheManager,
    bytes::Int,
    priority::AllocationPriority = PreferLocal,
    id::UInt = getid(manager),
)
    ptr = @spinlock alloc_lock(manager) unsafe_alloc(manager, bytes, priority, id)
    return Region(ptr, manager)
end

# Note: `push!` for the `FreeBuffer` is thread safe, so no synchonizations needs
# to happen at this level.
free(manager::CacheManager, ptr::Ptr) = push!(manager.freebuffer, unsafe_block(ptr))

"""
    moveto!(::PoolType{T}, obj, [manager]; kw...)

Move the memory for `obj` to pool `T`

## Keywords
* `dirty::Bool` - Indicate that this block is dirty. If memory is
"""
moveto!(pool, A, M = manager(A); kw...) = moveto!(pool, metadata(A), M; kw...)

# Temporary intermediate functions.
function moveto!(
    pool::LocalPool,
    block::Block,
    manager::CacheManager;
    canabort = alwaysfalse,
)
    return actuate!(
        pool,
        block,
        manager;
        copydata = true,
        updatebackedge = true,
        freeblock = false,
        canabort,
    )
end

function moveto!(
    pool::RemotePool,
    block::Block,
    manager::CacheManager;
    canabort = alwaysfalse,
)
    return actuate!(
        pool,
        block,
        manager;
        copydata = true,
        updatebackedge = true,
        freeblock = true,
        canabort,
    )
end

#####
##### Cacheable API
#####

isdirty(A) = isdirty(metadata(A))
id(A) = getid(metadata(A))
pool(A) = getpool(metadata(A))

function prefetch!(A; kw...)
    _manager = manager(A)
    @spinlock alloc_lock(_manager) remove_lock(_manager.freebuffer) begin
        moveto!(LocalPool(), A, _manager; kw...)
    end
end

function shallowfetch!(A; kw...)
    _manager = manager(A)
    @spinlock alloc_lock(_manager) remove_lock(_manager.freebuffer) begin
        moveto!(LocalPool(), A, manager(A); kw..., nocopy = true)
    end
end

function evict!(A; kw...)
    _manager = manager(A)
    @spinlock alloc_lock(_manager) remove_lock(_manager.freebuffer) begin
        moveto!(RemotePool(), A, manager(A); kw...)
    end
end

manager(x) = error("Implement `manager` for $(typeof(x))")

# Note: `Threads.atomid_add!` returns the old value of the atomic.
getid(manager::CacheManager) = Threads.atomic_add!(manager.idcount, one(UInt64))

# Have all objects tracking the manager been cleaned up?
function cangc(manager::CacheManager)
    # Empty out the cleanlist to deal with anything that's been GC'd
    @spinlock alloc_lock(manager) cleanup(manager)
    return (localsize(manager) == 0) && (remotesize(manager) == 0)
end

function Base.show(io::IO, M::CacheManager)
    println(io, "Cache Manager")
    println(io, "    $(length(M.local_objects)) Local Objects")
    println(io, "    $(length(M.remote_objects)) Remote Objects")
    println(
        io,
        "    $(localsize(M) / 1E9) GB Local Memory Used of $(M.local_heap.len / 1E9)",
    )
    println(
        io,
        "    $(remotesize(M) / 1E9) GB Remote Memory Used of $(M.remote_heap.len / 1E9)",
    )
end

# TODO: Rename these
inlocal(manager, x) = haskey(manager.local_objects, getid(metadata(x)))
inremote(manager, x) = haskey(manager.remote_objects, getid(metadata(x)))
localsize(manager::CacheManager) = manager.size_of_local
remotesize(manager::CacheManager) = manager.size_of_remote

#####
##### API for adding and removing items from the
#####

getobjects(::LocalPool, M::CacheManager) = M.local_objects
getobjects(::RemotePool, M::CacheManager) = M.remote_objects

adjust_size!(::LocalPool, M::CacheManager, x) = (M.size_of_local += x)
adjust_size!(::RemotePool, M::CacheManager, x) = (M.size_of_remote += x)

"""
$(TYPEDSIGNATURES)

Register `allocated_pointer` and `backedge` with `manager`.
Argument `allocated_pointer` must come from a heap owned by `manager` and `backedge` must
point to the `Region` (i.e. mutable struct) holding `allocated_pointer`.

The best way to obtain `backedge` is `Base.pointer_from_objref(region)` where
`region::Region`.
"""
function register!(
    manager::CacheManager,
    backedge::Backedge,
    allocated_pointer::Ptr{Nothing},
)
    @spinlock alloc_lock(manager) begin
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
    objects = getobjects(pool, manager)
    @check !haskey(objects, id)

    # Add to data structures
    #
    # If adding to Local, also add this block to the eviction policy
    if T == Local
        push!(manager.policy, block)
    end
    objects[id] = backedge
    adjust_size!(pool, manager, length(block))
    return nothing
end

update!(::LocalPool, M::CacheManager, A) = update!(M.policy, metadata(A))

# Top level entry points
function unregister!(pool, M::CacheManager, A)
    @spinlock alloc_lock(M) unsafe_unregister!(pool, M, metadata(A))
    return nothing
end

function unsafe_unregister!(pool::PoolType{T}, M::CacheManager, block::Block) where {T}
    @requires alloc_lock(M)
    id = getid(block)
    objects = getobjects(pool, M)

    # Just remove backedges
    delete!(objects, id)

    # If we're unregistering from Local, also remove this block from the eviction policy.
    if T == Local
        in(block, M.policy) && delete!(M.policy, block)
    end
    adjust_size!(pool, M, -length(block))
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
function cleanup(manager::CacheManager, _id::Union{Integer,Nothing} = nothing)
    @spinlock remove_lock(manager.freebuffer) begin
        prepare_cleanup!(manager)
        unsafe_cleanup!(manager)
    end
end

prepare_cleanup!(manager::CacheManager) = unsafe_swap!(manager.freebuffer)

# N.B. - `free_lock` must be held to call this.
const TIMES = UInt64[]
function unsafe_cleanup!(M::CacheManager, id = nothing)
    @requires alloc_lock(M) remove_lock(M.freebuffer)
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
    # Don't bother trying to allocate to the local heap if the requested allocation
    # is bigger than the heap anyways.
    if bytes <= sizeof(manager.local_heap) && priority != ForceRemote
        ptr = _pool_alloc(LocalPool(), manager, bytes, id; priority)
        if ptr !== nothing
            @telemetry manager telemetry_alloc(
                gettelemetry(manager),
                manager,
                bytes,
                id,
                ptr,
                Local,
            )
            return ptr
        end
    end

    # No free space. Try to allocate from Remote
    ptr = _pool_alloc(RemotePool(), manager, bytes, id)
    @telemetry manager telemetry_alloc(
        gettelemetry(manager),
        manager,
        bytes,
        id,
        ptr,
        Remote,
    )
    return ptr
end

function _pool_alloc(
    ::LocalPool,
    manager::CacheManager,
    bytes,
    id::UInt = getid(manager);
    priority::AllocationPriority = PreferLocal,
    cleanup_function::F = cleanup,
    eviction_function::G = doeviction!,
    kw...,
) where {F,G}
    @requires alloc_lock(manager)
    heap = manager.local_heap

    # Try to cleanup the freebuffer.
    # The cleanup function *may* take an ID, in which case it will return "true" if
    # the id we're trying to allocate was freed as part of the cleanup process.
    #
    # If this is the case, then we simply abort.
    if candrain(manager.freebuffer) && cleanup_function(manager, id)
        return Ptr{Nothing}()
    end

    # See if we should heuristically trigger an incremental GC
    # TODO: Move this to the policy to avoid over-calling `GC.gc()`?
    already_gc = false
    if manager.size_of_local >= manager.gc_when_over_local
        GC.gc(false)
        already_gc = true
    end

    # Try allocating the pointer.
    # If allocation fails try cleaning up the free buffer.
    ptr = alloc(heap, bytes, id)
    if ptr === nothing
        already_gc || GC.gc(false)
        candrain(manager.freebuffer) && cleanup_function(manager)
        ptr = alloc(heap, bytes, id)
    end

    # Still failed, try moving some local objects to remote memory.
    if ptr === nothing && manager.allow_movement
        canabort = manager.abort_callback
        canabort.bytes = bytes
        eviction_function(manager, bytes; canabort = canabort)
        ptr = alloc(manager.local_heap, bytes, id)
    end
    return ptr
end

function _pool_alloc(
    ::RemotePool,
    manager::CacheManager,
    bytes,
    id::UInt = getid(manager);
    cleanup_function::F = cleanup,
    canabort::A = alwaysfalse,
    kw...,
) where {F,A}
    @requires alloc_lock(manager)

    heap = manager.remote_heap
    if manager.size_of_remote >= manager.gc_when_over_remote
        GC.gc(true)
    end

    if candrain(manager.freebuffer)
        # If this block gets cleaned up, then first check if it's okay to abort
        # the eviction operation.
        #
        # If so, return `nothing` do indicate such.
        #
        # If it is NOT okay to abort the eviction operation, but the block was
        # cleaned, return a null ptr.
        cleaned = cleanup_function(manager, id)
        canabort() && return nothing
        cleaned && return Ptr{Nothing}()
    end
    ptr = alloc(heap, bytes, id)
    ptr === nothing && error("Cannot Allocate from PM")
    return ptr
end

# Dispatch plumbing!
#
# Try to ultimately convert an object to a pointer to the start of the user accessible
# data region.
free_convert(x::Ptr) = convert(Ptr{Nothing}, x)
free_convert(x::Block) = datapointer(x)

free(pool::PoolType, manager::CacheManager, x) = free(pool, manager, free_convert(x))
function free(::LocalPool, manager::CacheManager, ptr::Ptr{Nothing})
    return free(manager.local_heap, ptr)
end
function free(::RemotePool, manager::CacheManager, ptr::Ptr{Nothing})
    return free(manager.remote_heap, ptr)
end

# Eviction entry point
"""
$(TYPEDSIGNATURES)

Safely try to free `bytes` bytes from the fast memory in `manager`.
Keyword argument `canabort` is zero-argument a callback returning `true` if it is safe
to abort the eviction procedure potentially before all space has been cleared.

NOTE: `manager`'s allocation lock (`alloc_lock(manager)`) must be held before this
function is called.
"""
function doeviction!(manager::CacheManager, bytes; canabort::F = alwaysfalse) where {F}
    @spinlock remove_lock(manager.freebuffer) unsafe_eviction!(manager, bytes; canabort)
end

"""
$(TYPEDSIGNATURES)

Similar to [`doeviction!`](@ref) but assumes that both `alloc_lock(manager)` and
`remove_lock(manager.freebuffer)` are held.

Try to free `bytes` bytes from the fast memory in `manager`.
Keyword argument `canabort` is zero-argument a callback returning `true` if it is safe
to abort the eviction procedure potentially before all space has been cleared.
"""
function unsafe_eviction!(manager::CacheManager, bytes; canabort::F = alwaysfalse) where {F}
    @requires alloc_lock(manager) remove_lock(manager.freebuffer)

    # The eviction callback
    cb = block -> moveto!(RemotePool(), block, manager; canabort)

    isempty(manager.policy) && return nothing
    token = fullpop!(manager.policy)
    block = getval(Block, token)

    # Block is queued for freeing - no need to move it.
    # Put it back into the policy tracker to avoid messing it up and perform
    # a cleanup.
    if isqueued(block)
        push!(manager.policy, token)
        unsafe_cleanup(manager)

        # TODO: Might be able to check at this point if we can allocate for real.
        return nothing
    end

    # Perform our managed eviction.
    evictfrom!(manager.local_heap, block, bytes; cb = cb)
    return nothing
end

#####
##### Moving Objects
#####

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
    @requires alloc_lock(manager) remove_lock(manager.freebuffer)

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
        sibling_ptr = _pool_alloc(
            pool,
            manager,
            length(block),
            id;
            cleanup_function = unsafe_cleanup!,
            eviction_function = unsafe_eviction!,
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
            free(pool, manager, sibling)
            return canabort() ? true : nothing
        end
    else
        setdirty!(newblock, flag)
        setdirty!(manager.policy, newblock, flag)
    end

    # Update backedge to visible struct if necessary.
    if createsibling || updatebackedge
        backedge = getobjects(complement(pool), manager)[id]
        updatebackedge && unsafe_store!(backedge, sibling_ptr)
        createsibling && unsafe_register!(pool, manager, sibling, backedge)
    end

    if freeblock
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

