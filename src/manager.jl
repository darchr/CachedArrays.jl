# TODO: New Strategy for swapping pointers
# When objects register themselves, they will register their `pointer_from_objref` and
# the offset of their data pointer.
#
# For eviction, we sneakily swap out the pointer field.
#
# The primary motivation for this is to decrease the total number of allocations and thus
# hopefully speed up GC times.

const PMM_ALLOCATOR_TYPE = IS_2LM ? AlignedAllocator : MemKindAllocator

_datapointer(x) = convert(Ptr{Ptr{Nothing}}, datapointer(x))

mutable struct Region{T}
    # TODO: Store the allocation function in the block header to avoid taking space in
    # the region struct?
    ptr::Ptr{Nothing}
    manager::T

    function Region(ptr::Ptr{Nothing}, manager::T) where {T}
        @timeit "allocating" begin
            region = new{T}(ptr, manager)

            # Connect with the manager
            register!(manager, _datapointer(region), ptr)
            finalizer(cleanup, region)
        end
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
    pmm_heap::CompactHeap{PMM_ALLOCATOR_TYPE}
    dram_heap::CompactHeap{AlignedAllocator}

    # Synchronize access
    alloc_lock::Base.Threads.SpinLock
    freebuffer::FreeBuffer{Block}

    ## local tunables

    # We trigger a full GC before trying to evict items from the local cache.
    # Try to take advantage of that GC to flush items from the cache until (1-FLUSHPERCENT)
    # of the cache is free.
    flushpercent::Float32

    # Sometimes, doing a GC before flushing isn't helpful and just results in us spending
    # too much time in the GC.
    #
    # TODO: In the future, I'd like to have this be auto-determined by whether GC actually
    # freed anything, but for now set or disable it manually.
    gc_before_evict::Bool

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
    flushpercent = Float32(1),
    gc_before_evict = false,
    minallocation = 10,
)

    # Argument conversion
    flushpercent = Float32(flushpercent)

    # If we're in 2LM, pass a nullptr.
    # MemKindAllocator gets swapped to something that throws an error if called.
    if IS_2LM
        pmm_allocator = AlignedAllocator()
    else
        # For now, pass 0 - which essentially allows unlimited memory
        pmm_allocator = MemKindAllocator(MemKind.create_pmem(path, 0))
    end

    local_objects = Dict{UInt,Ptr{Ptr{Nothing}}}()
    size_of_local = 0

    remote_objects = Dict{UInt,Ptr{Ptr{Nothing}}}()
    size_of_remote = 0

    # Initialize Heaps
    pmm_heap =
        CompactHeap(pmm_allocator, remotesize; pool = PMM, minallocation = minallocation)

    dram_heap = CompactHeap(
        AlignedAllocator(),
        localsize;
        pool = DRAM,
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
        flushpercent,
        gc_before_evict,

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
    # this particular object lives in DRAM
    if getpool(block) == DRAM
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
    @spinlock alloc_lock(_manager) begin
        @spinlock remove_lock(_manager.freebuffer) begin
            moveto!(PoolType{DRAM}(), A, _manager; kw...)
        end
    end
end

function shallowfetch!(A; kw...)
    _manager = manager(A)
    @spinlock alloc_lock(_manager) begin
        @spinlock remove_lock(_manager.freebuffer) begin
            moveto!(PoolType{DRAM}(), A, manager(A); kw..., write_before_read = true)
        end
    end
end

function evict!(A; kw...)
    _manager = manager(A)
    @spinlock alloc_lock(_manager) begin
        @spinlock remove_lock(_manager.freebuffer) begin
            moveto!(PoolType{PMM}(), A, manager(A); kw...)
        end
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

getobjects(::PoolType{DRAM}, M::CacheManager) = M.local_objects
getobjects(::PoolType{PMM}, M::CacheManager) = M.remote_objects

adjust_size!(::PoolType{DRAM}, M::CacheManager, x) = M.size_of_local += x
adjust_size!(::PoolType{PMM}, M::CacheManager, x) = M.size_of_remote += x

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
        if p == DRAM
            unsafe_register!(PoolType{DRAM}(), manager, block, datapointer)
        elseif p == PMM
            unsafe_register!(PoolType{PMM}(), manager, block, datapointer)
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
    println("Registering block: $id")
    objects = getobjects(pool, M)
    @check !haskey(objects, id)

    # Add to data structures
    #
    # If adding to DRAM, also add this block to the eviction policy
    if T == DRAM
        push!(M.policy, block)
    end
    objects[id] = convert(Ptr{Ptr{Nothing}}, ptr)
    adjust_size!(pool, M, length(block))
    return nothing
end

update!(::PoolType{DRAM}, M::CacheManager, A) = update!(M.policy, metadata(A))

# Top level entry points
function unregister!(pool, M::CacheManager, A)
    @spinlock alloc_lock(M) unsafe_unregister!(pool, M, metadata(A))
    return nothing
end

function unsafe_unregister!(pool::PoolType{T}, M::CacheManager, block::Block) where {T}
    @requires alloc_lock(M)
    id = getid(block)
    println("Unregistering block: $id")
    objects = getobjects(pool, M)

    # Just remove backedges
    @check haskey(objects, id)
    delete!(objects, id)

    # If we're unregistering from DRAM, also remove this block from the eviction policy.
    if T == DRAM
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

function cleanup(manager)
    @spinlock remove_lock(manager.freebuffer) begin
        prepare_cleanup!(manager)
        unsafe_cleanup!(manager)
    end
end

prepare_cleanup!(manager::CacheManager) = unsafe_swap!(manager.freebuffer)

# N.B. - `free_lock` must be held to call this.
function unsafe_cleanup!(M::CacheManager)
    @requires alloc_lock(M) remove_lock(M.freebuffer)
    while true
        cleanlist = unsafe_get(M.freebuffer)
        println("Running cleanup on $(length(cleanlist)) blocks")
        # Free all blocks in the cleanlist
        for block in cleanlist
            @check !isfree(block)

            # If this block is in PMM, make sure it doesn't have a sibling - otherwise, that would
            # be an error.
            pool = getpool(block)
            if pool == PMM
                @check getsibling(block) === nothing

                # Deregister from PMM. If there are zero remaining references,
                # then we free the block
                unsafe_unregister!(PoolType{PMM}(), M, block)
                free(PoolType{PMM}(), M, block)
            elseif pool == DRAM
                unsafe_unregister!(PoolType{DRAM}(), M, block)

                # Does this block have a sibling?
                sibling = getsibling(block)
                if sibling !== nothing
                    @check getid(block) == getid(sibling)
                    @check getpool(sibling) == PMM

                    # use `unsafe_unregister!` to completely remove this block.
                    unsafe_unregister!(PoolType{PMM}(), M, block)
                    free(PoolType{PMM}(), M, sibling)
                end
                free(PoolType{DRAM}(), M, block)
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
    return nothing
end

#####
##### `alloc` entry point
#####

# Try to allocate from DRAM first.
# Then, try to allocate from PMM
function alloc(manager::CacheManager, bytes::Int, id::UInt = getid(manager))
    ptr = @spinlock alloc_lock(manager) unsafe_alloc(manager, bytes, id)
    return Region(ptr, manager)
end

function unsafe_alloc(manager::CacheManager, bytes, id = getid(manager))
    ptr = unsafe_alloc(PoolType{DRAM}(), manager, bytes, id)
    ptr === nothing || return ptr

    # No free space. Try to allocate from PMM
    return unsafe_alloc(PoolType{PMM}(), manager, bytes, id)
end

# Step 1: Try to just do a normal allocation.
# Step 2: Run a quick GC and try to allocate again.
# Step 3: Run a full GC - try again.
# Step 4: Grab the bottom most item from the policy that we can evict to make room for
#         the object we want to allocate.
#
#         Do that free operation.

function unsafe_alloc(
    ::PoolType{DRAM},
    manager::CacheManager,
    bytes,
    id::UInt = getid(manager),
    cleanup_function::F = cleanup,
    eviction_function::G = doeviction!,
) where {F,G}
    @requires alloc_lock(manager)

    @timeit "DRAM Alloc" begin
        heap = manager.dram_heap
        candrain(manager.freebuffer) && cleanup_function(manager)
        ptr = alloc(heap, bytes, id)

        # If allocation failed, try a GC
        if manager.gc_before_evict && ptr === nothing
            printstyled("Trying full GC cleanup\n"; color = :red, bold = true)
            @timeit "DRAM Alloc GC" begin
                # Trigger full GC, then incremental GC to try to get finalizers to run.
                GC.gc(true)
                # candrain(manager.freebuffer) || cleanup_function(manager)
                ptr = alloc(heap, bytes, id)
            end
        end

        if ptr === nothing && manager.allow_movement
            printstyled("Trying Eviction\n"; color = :red, bold = true)
            @timeit "DRAM Alloc Eviction" begin
                eviction_function(manager, bytes)
                ptr = alloc(manager.dram_heap, bytes, id)
            end
        end
    end

    return ptr
end

function unsafe_alloc(
    ::PoolType{PMM},
    manager::CacheManager,
    bytes,
    id::UInt = getid(manager),
    cleanup_function::F = cleanup,
) where {F}
    @requires alloc_lock(manager)

    @timeit "PMM Alloc" begin
        heap = manager.pmm_heap
        candrain(manager.freebuffer) && cleanup_function(manager)
        ptr = alloc(heap, bytes, id)

        # # If we still don't have anything, run a full GC.
        # if ptr === nothing
        #     @timeit "PMM Alloc GC" begin
        #         GC.gc(true)
        #         cleanup_function(manager)
        #         ptr = alloc(heap, bytes, id)
        #     end
        # end
    end

    if ptr === nothing
        error("Cannot Allocate from PMM")
    end

    return ptr
end

free_convert(x::Array) = pointer(x)
free_convert(x::Ptr) = convert(Ptr{Nothing}, x)
free_convert(x::Block) = _datapointer(x)

free(pool::PoolType, manager::CacheManager, x) = free(pool, manager, free_convert(x))
free(::PoolType{DRAM}, manager::CacheManager, ptr::Ptr{Nothing}) =
    free(manager.dram_heap, ptr)
free(::PoolType{PMM}, manager::CacheManager, ptr::Ptr{Nothing}) =
    free(manager.pmm_heap, ptr)

doeviction!(manager::CacheManager, bytes) =
    @spinlock remove_lock(manager.freebuffer) unsafe_eviction!(manager, bytes)
function unsafe_eviction!(manager::CacheManager, bytes)
    @requires alloc_lock(manager) remove_lock(manager.freebuffer)

    # The eviction callback
    cb = block -> moveto!(PoolType{PMM}(), block, manager)

    local block
    while true
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
            continue
        end

        @check canallocfrom(manager.dram_heap, block, bytes)
        break
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
    dram::PoolType{DRAM},
    block::Block,
    M::CacheManager;
    dirty = true,
    write_before_read = false,
)
    @requires alloc_lock(M) remove_lock(M.freebuffer)

    id = getid(block)
    println("Moving block $id to DRAM")

    # If the data is already local, mark a usage and return.
    if getpool(block) == DRAM
        # TODO: Fix
        #update!(PoolType{DRAM}(), M, A)
        return nothing
    end

    # If this block is NOT in DRAM, then it shouldn't have a sibling.
    @check getsibling(block) === nothing

    # Otherwise, we need to allocate some local data for it.
    storage_ptr = unsafe_alloc(
        PoolType{DRAM}(),
        M,
        length(block),
        id,
        unsafe_cleanup!,
        unsafe_eviction!,
    )

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
    pmm = PoolType{PMM}()
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

function moveto!(pool::PoolType{PMM}, block::Block, M::CacheManager)
    @requires alloc_lock(M) remove_lock(M.freebuffer)

    id = getid(block)
    println("Moving block $id to PM")
    # If already in PMM, do nothing
    getpool(block) == PMM && return nothing
    sibling = getsibling(block)

    # If this block has a sibling and it is clean, we can elide write back.
    # Otherwise, we have to write back.
    createstorage = (sibling === nothing)
    if createstorage
        storage_ptr = unsafe_alloc(PoolType{PMM}(), M, length(block), id, unsafe_cleanup!)
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

    # Update back edges
    dram = PoolType{DRAM}()
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

