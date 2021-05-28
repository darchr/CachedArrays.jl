_datapointer(x) = convert(Ptr{Ptr{Nothing}}, datapointer(x))

mutable struct Region{T}
    # TODO: Store the allocation function in the block header to avoid taking space in
    # the region struct?
    ptr::Ptr{Nothing}
    manager::T

    function Region(ptr::Ptr{Nothing}, manager::T) where {T}
        region = new{T}(ptr, manager)

        # Connect with the manager
        register!(manager, _datapointer(region), ptr)
        finalizer(cleanup, region)
        return region
    end
end

cleanup(region::Region) = cleanup(manager(region), pointer(region))
metastyle(::Region) = BlockMeta()
manager(region::Region) = region.manager

function alloc(region::Region, bytes::Integer, id = getid(region.manager))
    ptr = @spinlock alloc_lock(manager(region)) unsafe_alloc(manager(region), bytes, id)
    return Region(ptr, manager(region))
end

Base.pointer(region::Region) = region.ptr
datapointer(region::Region) = pointer_from_objref(region)

mutable struct AbortCallback{H}
    heap::CompactHeap{H}
    bytes::Int
end

(f::AbortCallback)() = canalloc(f.heap, f.bytes)

# Maintains Cache management.
mutable struct CacheManager{C}
    # Reference to local objects
    # NOTE: These need to be pointers and not `Region`s because we don't want the
    # the manager to protect these objects from being garbage collected.
    local_objects::Dict{UInt,Ptr{Ptr{Nothing}}}
    size_of_local::Int

    # Create a new ID for each object registered in the cache.
    idcount::Threads.Atomic{UInt64}

    # All objects with remote memory.
    remote_objects::Dict{UInt,Ptr{Ptr{Nothing}}}
    size_of_remote::Int

    # local datastructures
    policy::C
    pmm_heap::CompactHeap{MmapAllocator}
    dram_heap::CompactHeap{AlignedAllocator}

    # Synchronize access
    alloc_lock::Base.Threads.SpinLock
    freebuffer::FreeBuffer{Block}

    ## local tunables
    gc_when_over::Int
    abort_callback::AbortCallback{AlignedAllocator}

    ## Prevent arrays from moving
    allow_movement::Bool
end

free_lock(manager::CacheManager) = manager.free_lock
alloc_lock(manager::CacheManager) = manager.alloc_lock

function CacheManager(
    path::AbstractString;
    localsize = 1_000_000_000,
    remotesize = 1_000_000_000,
    policy = LRU{Block}(),
    gc_when_over = 0.95,
    minallocation = 10,
)
    # If we're in 2LM, pass a nullptr.
    # MemKindAllocator gets swapped to something that throws an error if called.
    remote_allocator = MmapAllocator(path)

    local_objects = Dict{UInt,Ptr{Ptr{Nothing}}}()
    size_of_local = 0

    remote_objects = Dict{UInt,Ptr{Ptr{Nothing}}}()
    size_of_remote = 0

    # Initialize Heaps
    pmm_heap = CompactHeap(
        remote_allocator,
        remotesize;
        pool = Remote,
        minallocation = minallocation,
    )

    dram_heap = CompactHeap(
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
        pmm_heap,
        dram_heap,
        Threads.SpinLock(),
        FreeBuffer{Block}(),

        # tunables,
        floor(Int, gc_when_over * sizeof(dram_heap)),
        AbortCallback(dram_heap, 0),

        # runtime settings
        allow_movement,
    )

    # Add this to the global manager list to ensure that it outlives any of its users
    push!(GlobalManagers, manager)
    return manager
end

#####
##### Policy Hints
#####

# Marking a block as dirty both notifies the policy and sets the objects metadata.
setdirty!(A, flag::Bool = true) = setdirty!(A, manager(A), flag)
setdirty!(A, M::CacheManager, flag) = setdirty!(metadata(A), M, flag)

function setdirty!(block::Block, M::CacheManager, flag)
    # Only need to worry about updating the state of the block and the policy if
    # this particular object lives in Local
    if getpool(block) == Local
        setdirty!(block, flag)
        # TODO: Enforce synchronization.
        setdirty!(M.policy, block, flag)
    end
    return nothing
end

#####
##### Enable/Disable movement
#####

enable_movement!(M::CacheManager) = (M.allow_movement = true)
disable_movement!(M::CacheManager) = (M.allow_movement = false)

#####
##### Cacheable API
#####

isdirty(A) = isdirty(metadata(A))
id(A) = getid(metadata(A))
pool(A) = getpool(metadata(A))

function prefetch!(A; kw...)
    _manager = manager(A)
    @spinlock alloc_lock(_manager) remove_lock(_manager.freebuffer) begin
        moveto!(PoolType{Local}(), A, _manager; kw...)
    end
end

function shallowfetch!(A; kw...)
    _manager = manager(A)
    @spinlock alloc_lock(_manager) remove_lock(_manager.freebuffer) begin
        moveto!(PoolType{Local}(), A, manager(A); kw..., write_before_read = true)
    end
end

function evict!(A; kw...)
    _manager = manager(A)
    @spinlock alloc_lock(_manager) remove_lock(_manager.freebuffer) begin
        moveto!(PoolType{Remote}(), A, manager(A); kw...)
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
        "    $(localsize(M) / 1E9) GB Local Memory Used of $(M.dram_heap.len / 1E9)",
    )
    println(
        io,
        "    $(remotesize(M) / 1E9) GB Remote Memory Used of $(M.pmm_heap.len / 1E9)",
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

getobjects(::PoolType{Local}, M::CacheManager) = M.local_objects
getobjects(::PoolType{Remote}, M::CacheManager) = M.remote_objects

adjust_size!(::PoolType{Local}, M::CacheManager, x) = M.size_of_local += x
adjust_size!(::PoolType{Remote}, M::CacheManager, x) = M.size_of_remote += x

function register!(
    manager::CacheManager,
    datapointer::Ptr{Ptr{Nothing}},
    allocated_pointer::Ptr{Nothing},
)
    @spinlock alloc_lock(manager) begin
        block = unsafe_block(allocated_pointer)
        # Register with the appropriate pool.
        # Statically dispatch to avoid a potential allocation.
        p = block.pool
        if p == Local
            unsafe_register!(PoolType{Local}(), manager, block, datapointer)
        elseif p == Remote
            unsafe_register!(PoolType{Remote}(), manager, block, datapointer)
        else
            error("Unknown Pool: $p")
        end
    end
    return nothing
end

function unsafe_register!(pool::PoolType{T}, M::CacheManager, block::Block, ptr) where {T}
    @requires alloc_lock(M)
    @check block.pool == T
    id = getid(block)
    objects = getobjects(pool, M)
    @check !haskey(objects, id)

    # Add to data structures
    #
    # If adding to Local, also add this block to the eviction policy
    if T == Local
        push!(M.policy, block)
    end
    objects[id] = convert(Ptr{Ptr{Nothing}}, ptr)
    adjust_size!(pool, M, length(block))
    return nothing
end

update!(::PoolType{Local}, M::CacheManager, A) = update!(M.policy, metadata(A))

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

# Note: `push!` for the `FreeBuffer` is thread safe, so no synchonizations needs
# to happen at this level.
cleanup(manager::CacheManager, ptr::Ptr) = push!(manager.freebuffer, unsafe_block(ptr))

# Have an optional, ignored argument for a similar signature to `unsafe_cleanup!`.
function cleanup(manager, _id::Union{Integer,Nothing} = nothing)
    @spinlock remove_lock(manager.freebuffer) begin
        prepare_cleanup!(manager)
        unsafe_cleanup!(manager)
    end
end

prepare_cleanup!(manager::CacheManager) = unsafe_swap!(manager.freebuffer)

# N.B. - `free_lock` must be held to call this.
function unsafe_cleanup!(M::CacheManager, id = nothing)
    @requires alloc_lock(M) remove_lock(M.freebuffer)
    id_cleaned = false

    while true
        cleanlist = unsafe_get(M.freebuffer)

        # Free all blocks in the cleanlist
        for block in cleanlist
            @check !isfree(block) || block.evicting

            # If this block is in Remote, make sure it doesn't have a sibling - otherwise, that would
            # be an error.
            id == getid(block) && (id_cleaned = true)
            pool = getpool(block)
            if pool == Remote
                @check getsibling(block) === nothing

                # Deregister from Remote. If there are zero remaining references,
                # then we free the block
                unsafe_unregister!(PoolType{Remote}(), M, block)
                free(PoolType{Remote}(), M, block)
            elseif pool == Local
                unsafe_unregister!(PoolType{Local}(), M, block)

                # Does this block have a sibling?
                sibling = getsibling(block)
                if sibling !== nothing
                    @check getid(block) == getid(sibling)
                    @check getpool(sibling) == Remote

                    # use `unsafe_unregister!` to completely remove this block.
                    unsafe_unregister!(PoolType{Remote}(), M, block)
                    free(PoolType{Remote}(), M, sibling)
                end
                free(PoolType{Local}(), M, block)
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
    return id_cleaned
end

#####
##### `alloc` entry point
#####

# Try to allocate from Local first.
# Then, try to allocate from Remote
function alloc(manager::CacheManager, bytes::Int, id::UInt = getid(manager))
    ptr = @spinlock alloc_lock(manager) unsafe_alloc(manager, bytes, id)
    return Region(ptr, manager)
end

function unsafe_alloc(manager::CacheManager, bytes, id = getid(manager))
    ptr = unsafe_alloc(PoolType{Local}(), manager, bytes, id)
    ptr === nothing || return ptr

    # No free space. Try to allocate from Remote
    return unsafe_alloc(PoolType{Remote}(), manager, bytes, id)
end

const TIMES = UInt64[]

function unsafe_alloc(
    ::PoolType{Local},
    manager::CacheManager,
    bytes,
    id::UInt = getid(manager),
    cleanup_function::F = cleanup,
    eviction_function::G = doeviction!,
) where {F,G}
    @requires alloc_lock(manager)

    start = time_ns()
    heap = manager.dram_heap
    if candrain(manager.freebuffer) && cleanup_function(manager, id)
        push!(TIMES, time_ns() - start)
        return Ptr{Nothing}()
    end

    # See if we should trigger an incremental GC
    already_gc = false
    if manager.size_of_local >= manager.gc_when_over
        GC.gc(false)
        already_gc = true
    end

    ptr = alloc(heap, bytes, id)

    # If allocation failed, try a GC
    if ptr === nothing
        # Trigger full GC, then incremental GC to try to get finalizers to run.
        already_gc || GC.gc(false)
        candrain(manager.freebuffer) && cleanup_function(manager)
        ptr = alloc(heap, bytes, id)
    end

    if ptr === nothing && manager.allow_movement
        canabort = manager.abort_callback
        canabort.bytes = bytes

        eviction_function(manager, bytes; canabort = canabort)
        ptr = alloc(manager.dram_heap, bytes, id)
    end

    push!(TIMES, time_ns() - start)
    return ptr
end

function unsafe_alloc(
    ::PoolType{Remote},
    manager::CacheManager,
    bytes,
    id::UInt = getid(manager),
    cleanup_function::F = cleanup;
    canabort::A = alwaysfalse,
) where {F,A}
    @requires alloc_lock(manager)

    heap = manager.pmm_heap
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
free_convert(x::Array) = pointer(x)
free_convert(x::Ptr) = convert(Ptr{Nothing}, x)
free_convert(x::Block) = _datapointer(x)

free(pool::PoolType, manager::CacheManager, x) = free(pool, manager, free_convert(x))
function free(::PoolType{Local}, manager::CacheManager, ptr::Ptr{Nothing})
    return free(manager.dram_heap, ptr)
end
function free(::PoolType{Remote}, manager::CacheManager, ptr::Ptr{Nothing})
    return free(manager.pmm_heap, ptr)
end

# Eviction entry point
function doeviction!(manager::CacheManager, bytes; canabort::F = alwaysfalse) where {F}
    @spinlock remove_lock(manager.freebuffer) unsafe_eviction!(manager, bytes; canabort)
end

function unsafe_eviction!(manager::CacheManager, bytes; canabort::F = alwaysfalse) where {F}
    @requires alloc_lock(manager) remove_lock(manager.freebuffer)

    # The eviction callback
    cb = block -> moveto!(PoolType{Remote}(), block, manager; canabort)

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
    evictfrom!(manager.dram_heap, block, bytes; cb = cb)
    return nothing
end

#####
##### Moving Objects
#####

moveto!(pool, A, M = manager(A); kw...) = moveto!(pool, metadata(A), M; kw...)

function moveto!(
    dram::PoolType{Local},
    block::Block,
    M::CacheManager;
    dirty = true,
    write_before_read = false,
)
    @requires alloc_lock(M) remove_lock(M.freebuffer)

    id = getid(block)

    # If the data is already local, mark a usage and return.
    if getpool(block) == Local
        # TODO: Fix
        #update!(PoolType{Local}(), M, A)
        return nothing
    end

    # If this block is NOT in Local, then it shouldn't have a sibling.
    @check getsibling(block) === nothing

    # Otherwise, we need to allocate some local data for it.
    storage_ptr = unsafe_alloc(
        PoolType{Local}(),
        M,
        length(block),
        id,
        unsafe_cleanup!,
        unsafe_eviction!,
    )

    # Null pointer returned - corresponding block in Remote has been freed.
    # No need to finish the move.
    if isnull(storage_ptr)
        return nothing
    end

    # If we're assured that we're doing a write before a read, then we can avoid
    # copying over the the remote storage
    if !write_before_read
        _memcpy!(storage_ptr, datapointer(block), length(block))
    end

    # Get the block for the newly created Array.
    # Here, we have to maintain some metadata.
    newblock = unsafe_block(storage_ptr)

    setsibling!(newblock, block)
    setsibling!(block, newblock)

    # Check if this block has multiple watchers.
    # If so, update all references to this block.
    pmm = PoolType{Remote}()
    ptr = getobjects(pmm, M)[id]
    unsafe_store!(ptr, storage_ptr)
    unsafe_register!(dram, M, newblock, ptr)

    # Set the dirty flag.
    # If `write_before_read` is true, then we MUST mark this block as dirty.
    flag = write_before_read | dirty
    setdirty!(newblock, flag)
    setdirty!(M.policy, newblock, flag)
    return nothing
end

function moveto!(
    pool::PoolType{Remote},
    block::Block,
    M::CacheManager;
    canabort = alwaysfalse,
)
    @requires alloc_lock(M) remove_lock(M.freebuffer)

    dram = PoolType{Local}()
    id = getid(block)
    # If already in Remote, do nothing
    getpool(block) == Remote && return nothing
    sibling = getsibling(block)

    # If this block has a sibling and it is clean, we can elide write back.
    # Otherwise, we have to write back.
    createstorage = (sibling === nothing)
    if createstorage
        storage_ptr = unsafe_alloc(
            PoolType{Remote}(),
            M,
            length(block),
            id,
            unsafe_cleanup!;
            canabort = canabort,
        )

        # Cleanup happened and we now have enough space to fulfill the original request.
        if storage_ptr === nothing
            return true
        end

        # Note: Checked allocation will return a null pointer if the requested ID was
        # freed during a cleanup trying to service this allocation.
        #
        # If that's the case, then we don't need to actually bother moving this data block
        # because, well, it's free!
        if isnull(storage_ptr)
            return nothing
        end
        sibling = unsafe_block(storage_ptr)
    else
        storage_ptr = datapointer(sibling)
    end

    @check getid(sibling) == id

    if isdirty(block) || createstorage
        # Since `storage_ptr` is just a pointer, we have to manually inform the copy engine
        # how many threads to use.
        nthreads = min(Threads.nthreads(), 4)
        _memcpy!(storage_ptr, datapointer(block), length(block); nthreads = nthreads)
    end

    # Performing the memcpy may trigger garbage collection.
    # Since we hold the remove-lock - no one else has tried to cleanup the manager.
    # It's possible the block we just moved has now been queued for deletion.
    # Check that case.
    if isqueued(block)
        unsafe_cleanup!(M)
        # Abort operation
        free(PoolType{Remote}(), M, sibling)
        return canabort() ? true : nothing
    end

    # Update back edges
    ptr = getobjects(dram, M)[id]
    unsafe_store!(ptr, storage_ptr)
    createstorage && unsafe_register!(pool, M, sibling, ptr)

    # Deregister this object from local tracking.
    unsafe_unregister!(dram, M, block)

    # Free the block we just removed.
    free(dram, M, block)
    setsibling!(sibling, Block())
    return nothing
end

#####
##### Validation
#####

function check(manager::CacheManager)
    passed = true
    if !check(manager.pmm_heap)
        println("Remote Heap failed!")
        passed = false
    end

    if !check(manager.dram_heap)
        println("Local Heap failed!")
        passed = false
    end

    # Now - check if the manager's recorded stats align with the each of the heaps.
    # Remote Heap
    seen_ids = Set{UInt64}()
    size_allocated = 0
    for block in manager.pmm_heap
        if !isfree(block)
            push!(seen_ids, CachedArrays.getid(block))
            size_allocated += length(block)
        end
    end
    if manager.size_of_remote != size_allocated
        println("Manager and heap Remote objects size mismatch. Manager sees: $(manager.size_of_remote). Heap sees: $size_allocated.")
        passed = false
    end

    issubset(seen_ids, keys(manager.remote_objects)) || (passed = false)
    issubset(keys(manager.remote_objects), seen_ids) || (passed = false)

    # Local Heap
    seen_ids = Set{UInt64}()
    size_allocated = 0
    for block in manager.dram_heap
        if !isfree(block)
            push!(seen_ids, CachedArrays.getid(block))
            size_allocated += length(block)
        end
    end
    if manager.size_of_local != size_allocated
        println("Manager and heap Local objects size mismatch. Manager sees: $(manager.size_of_local). Heap sees: $size_allocated.")
        println("    Manager IDS: $(Int.(sort(collect(keys(manager.local_objects)))))")
        println("    Heap IDS: $(Int.(sort(collect(seen_ids))))")
        passed = false
    end

    if !issubset(seen_ids, keys(manager.local_objects))
        println("Manager sees $(length(manager.local_objects)) in Local. Heap sees $(length(seen_ids))")
        passed = false
    end
    if !issubset(keys(manager.local_objects), seen_ids)
        println("Manager sees $(length(manager.local_objects)) in Local. Heap sees $(length(seen_ids))")
        passed = false
    end

    return passed
end
