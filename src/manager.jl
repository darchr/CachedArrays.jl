# TODO: New Strategy for swapping pointers
# When objects register themselves, they will register their `pointer_from_objref` and
# the offset of their data pointer.
#
# For eviction, we sneakily swap out the pointer field.
#
# The primary motivation for this is to decrease the total number of allocations and thus
# hopefully speed up GC times.

# Maintains Cache management.
mutable struct CacheManager{C,P,Q}
    # Reference to local objects
    #
    # NOTE: Make sure that the manager is updated whenever an object that enters itself
    # into the cache is finalized.
    local_objects::Dict{UInt,WeakRef}
    size_of_local::Int

    # Create a new ID for each object registerd in the cache.
    idcount::UInt

    # All objects with remote memory.
    remote_objects::Dict{UInt,WeakRef}
    size_of_remote::Int

    # local datastructures
    policy::C
    pmm_heap::P
    dram_heap::Q

    # Keep track of objects that we've force freed.
    # If they get finalized, don't do any freeing.
    #force_freed::Set{UInt}

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
end

function CacheManager(
        path::AbstractString;
        localsize = 1_000_000_000,
        remotesize = 1_000_000_000,
        policy = LRU{Block}(),
        flushpercent = Float32(1),
        gc_before_evict = false,
        minallocation = 22
    ) where {T}

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

    local_objects = Dict{UInt,WeakRef}()
    size_of_local = 0
    idcount = one(UInt)

    remote_objects = Dict{UInt,WeakRef}()
    size_of_remote = 0

    # Initialize Heaps
    pmm_heap = CompactHeap(
        pmm_allocator,
        remotesize;
        pool = PMM,
        minallocation = minallocation
    )

    dram_heap = CompactHeap(
        AlignedAllocator(),
        localsize;
        pool = DRAM,
        minallocation = minallocation
    )

    # Construct the manager.
    manager = CacheManager(
        local_objects,
        size_of_local,
        idcount,
        remote_objects,
        size_of_remote,
        policy,
        pmm_heap,
        dram_heap,

        # tunables,
        flushpercent,
        gc_before_evict,
    )

    # Add this to the global manager list to ensure that it outlives any of its users
    push!(GlobalManagers, manager)

    return manager
end

# Get the block for the given object.
Base.get(M::CacheManager, block) = M.local_objects[getid(block)].value

#####
##### Policy Hints
#####

# Marking a block as dirty both notifies the policy and sets the objects metadata.
setdirty!(A, flag::Bool = true) = setdirty!(A, manager(A), flag)
function setdirty!(A, M::CacheManager, flag)
    block = metadata(A)
    # Only need to worry about updating the state of the block and the policy if
    # this particular object lives in DRAM
    if getpool(block) == DRAM
        setdirty!(block, flag)
        setdirty!(M.policy, block, flag)
    end
    return nothing
end

# Notify the policy that this block is a cheap evict.
# Defaults to NO-oP
cheapevict(A) = cheapevict(manager(A).policy, metadata(A))

#####
##### Cacheable API
#####

isdirty(A) = isdirty(metadata(A))
id(A) = getid(metadata(A))
pool(A) = getpool(metadata(A))

prefetch!(A; kw...) = moveto!(PoolType{DRAM}(), A, manager(A); kw...)
shallowfetch!(A; kw...) = moveto!(PoolType{DRAM}(), A, manager(A); kw..., write_before_read = true)

if IS_2LM
    # No eviction in 2LM
    evict!(A; kw...) = nothing
else
    evict!(A; kw...) = moveto!(PoolType{PMM}(), A, manager(A); kw...)
end

manager(x) = error("Implement `manager` for $(typeof(x))")

function getid(manager::CacheManager)
    id = manager.idcount
    manager.idcount += 1
    return id
end

# Have all objects tracking the manager been cleaned up?
cangc(manager::CacheManager) = (localsize(manager) == 0) && (remotesize(manager) == 0)

function Base.show(io::IO, M::CacheManager)
    println(io, "Cache Manager")
    println(io, "    $(length(M.local_objects)) Local Objects")
    println(io, "    $(length(M.remote_objects)) Remote Objects")
    println(io, "    $(localsize(M) / 1E9) GB Local Memory Used of $(M.dram_heap.len / 1E9)")
    println(io, "    $(remotesize(M) / 1E9) GB Remote Memory Used of $(M.pmm_heap.len / 1E9)")
end

# TODO: Rename these
inlocal(manager, x) = haskey(manager.local_objects, getid(metadata(x)))
inremote(manager, x) = haskey(manager.remote_objects, getid(metadata(x)))
localsize(manager::CacheManager) = manager.size_of_local
remotesize(manager::CacheManager) = manager.size_of_remote

#####
##### API for adding and removing items from the
#####

register!(A) = register!(manager(A), A)
function register!(M::CacheManager, A)
    # Register with the appropriate pool.
    # Statically dispatch to avoid a potential allocation.
    p = pool(A)
    if p == DRAM
        register!(PoolType{DRAM}(), M, A)
    elseif p == PMM
        register!(PoolType{PMM}(), M, A)
    else
        error("Unknown Pool: $p")
    end

    # Attach a finalizer to `A`.
    finalizer(cleanup, A)
    return nothing
end

function register!(::PoolType{DRAM}, M::CacheManager, A)
    block = metadata(A)

    # Correctness Checks
    @check block.pool == DRAM
    @check !haskey(M.local_objects, getid(block))

    # Add to local data structures
    push!(M.policy, block)
    M.local_objects[getid(block)] = WeakRef(A)
    M.size_of_local += sizeof(A)
    return nothing
end

update!(::PoolType{DRAM}, M::CacheManager, A) = update!(M.policy, metadata(A))

function unregister!(::PoolType{DRAM}, M::CacheManager, A)
    block = metadata(A)

    # Correctness checks
    @check haskey(M.local_objects, getid(block))

    delete!(M.local_objects, getid(block))
    in(block, M.policy) && delete!(M.policy, block)
    M.size_of_local -= sizeof(A)
    return nothing
end

function register!(::PoolType{PMM}, M::CacheManager, A)
    block = metadata(A)

    @check !haskey(M.remote_objects, getid(block))
    @check block.pool == PMM
    M.remote_objects[getid(block)] = WeakRef(A)
    M.size_of_remote += sizeof(A)
    return nothing
end

function unregister!(::PoolType{PMM}, M::CacheManager, A)
    block = metadata(A)

    @check haskey(M.remote_objects, getid(block))
    delete!(M.remote_objects, getid(block))
    M.size_of_remote -= sizeof(A)
    return nothing
end

#####
##### Cleanup Finalizer
#####

const CLEANUP_LIST = UInt[]

function cleanup(A, M = manager(A))
    # Get the block for this object.
    block = metadata(A)
    push!(CLEANUP_LIST, getid(block))

    # If this block is in PMM, make sure it doesn't have a sibling - otherwise, that would
    # be an error.
    pool = getpool(block)
    if pool == PMM
        @check isnothing(getsibling(block))

        # Deregister from PMM
        unregister!(PoolType{PMM}(), M, A)
        # Free this buffer
        free(PoolType{PMM}(), M, block)
    elseif pool == DRAM
        unregister!(PoolType{DRAM}(), M, A)

        # Does this block have a sibling?
        sibling = getsibling(block)
        if !isnothing(sibling)
            @check getid(block) == getid(sibling)
            @check getpool(sibling) == PMM
            unregister!(PoolType{PMM}(), M, A)
            free(PoolType{PMM}(), M, sibling)
        end
        free(PoolType{DRAM}(), M, block)
    end
    return nothing
end

#####
##### Allocate remote arrays
#####

unsafe_alloc(::Type{T}, x...; kw...) where {T} = convert(Ptr{T}, unsafe_alloc(x...; kw...))

# Remote alloc without a finalizer
function unsafe_alloc(
        ::PoolType{PMM},
        manager::CacheManager,
        bytes,
        id::UInt = getid(manager),
    ) where {T,N}

    @timeit "PMM Alloc" begin
        ptr = alloc(manager.pmm_heap, bytes, id)

        # If we still don't have anything, run a full GC.
        if isnothing(ptr)
            @timeit "PMM Alloc GC" begin
                GC.gc(true)
                ptr = alloc(manager.pmm_heap, bytes, id)
            end
        end
    end

    if isnothing(ptr)
        error("Cannot Allocate from PMM")
    end

    return ptr
end

#####
##### Local Allocation.
#####

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
    )

    @timeit "DRAM Alloc" begin
        ptr = alloc(manager.dram_heap, bytes, id)

        # If allocation failed, try a GC
        if manager.gc_before_evict && isnothing(ptr)
            @timeit "DRAM Alloc GC" begin
                GC.gc(true)
                ptr = alloc(manager.dram_heap, bytes, id)
            end
        end

        if isnothing(ptr)
            @timeit "DRAM Alloc Eviction" begin
                doeviction!(manager, bytes)
                ptr = alloc(manager.dram_heap, bytes, id)
            end
        end
    end

    if isnothing(ptr)
        error("Something's gone horribly wrong!")
    end

    return ptr
end

free_convert(x::Array) = pointer(x)
free_convert(x::Ptr) = convert(Ptr{Nothing}, x)
free_convert(x::Block) = datapointer(x)

free(pool, manager::CacheManager, x) = free(pool, manager, free_convert(x))
free(::PoolType{DRAM}, manager::CacheManager, ptr::Ptr{Nothing}) = free(manager.dram_heap, ptr)
free(::PoolType{PMM}, manager::CacheManager, ptr::Ptr{Nothing}) = free(manager.pmm_heap, ptr)

function doeviction!(manager, bytes)
    # The full storage storage types for policies may contain
    # extra metadata.
    stack = fulleltype(manager.policy)[]

    # The eviction callback
    cb = block -> moveto!(PoolType{PMM}(), manager, block)

    # TODO: Since the heap upgrade, we can restructure this.
    local block
    while true
        # Pop an item from the policy
        push!(stack, fullpop!(manager.policy))
        block = getval(Block, last(stack))

        canallocfrom(manager.dram_heap, block, bytes) && break
        if isempty(manager.policy)
            display(stack)
        end
    end

    # Put back all of the items in the LRU that we didn't use.
    while !isempty(stack)
        push!(manager.policy, pop!(stack))
    end

    # Perform our managed eviction.
    evictfrom!(manager.dram_heap, block, bytes; cb = cb)

    # Now that we've guarenteed that we have a block available in our requested size,
    # Clean up items from the cache.
    target = ceil(Int, manager.dram_heap.len * manager.flushpercent)

    while localsize(manager) >= target
        block = pop!(manager.policy)
        @check canallocfrom(manager.dram_heap, block, block.size - headersize())

        # Need to use the more powerful "evictfrom!" since it correctly puts the
        # heap back together.
        evictfrom!(manager.dram_heap, block, block.size - headersize(); cb = cb)
    end

    return nothing
end

#####
##### Moving Objects
#####

function moveto!(
        ::PoolType{DRAM},
        A,
        M = manager(A);
        dirty = true,
        write_before_read = false
    )
    # Get the metadata for this object.
    block = metadata(A)

    # If the data is already local, mark a usage and return.
    if getpool(block) == DRAM
        update!(PoolType{DRAM}(), M, A)
        return nothing
    end

    # If this block is NOT in DRAM, then it shouldn't have a sibling.
    @check isnothing(getsibling(block))

    # Otherwise, we need to allocate some local data for it.
    storage_ptr = unsafe_alloc(
        eltype(A),
        PoolType{DRAM}(),
        M,
        sizeof(A),
        getid(block),
    )

    # If we're assured that we're doing a write before a read, then we can avoid
    # copying over the the remote storage
    if !write_before_read
        _memcpy!(storage_ptr, pointer(A), length(A))
    end

    # Get the block for the newly created Array.
    # Here, we have to maintain some metadata.
    newblock = unsafe_block(storage_ptr)

    setsibling!(newblock, block)
    setsibling!(block, newblock)
    replace!(A, storage_ptr)

    # Track this object
    register!(PoolType{DRAM}(), M, A)

    # Set the dirty flag.
    # If `write_before_read` is true, then we MUST mark this block as dirty.
    flag = write_before_read | dirty
    setdirty!(newblock, flag)
    setdirty!(M.policy, newblock, flag)
    return nothing
end

function moveto!(pool::PoolType{PMM}, manager::CacheManager, block)
    return moveto!(pool, get(manager, block), manager)
end

function moveto!(::PoolType{PMM}, A, M::CacheManager = manager(A))
    block = metadata(A)

    # If already in PMM, do nothing
    getpool(block) == PMM && return nothing
    sibling = getsibling(block)

    # If this block has a sibling and it is clean, we can elide write back.
    # Otherwise, we have to write back.
    createstorage = isnothing(sibling)
    if createstorage
        storage_ptr = unsafe_alloc(eltype(A), PoolType{PMM}(), M, sizeof(A), getid(block))
        sibling = unsafe_block(storage_ptr)
    else
        storage_ptr = convert(Ptr{eltype(A)}, datapointer(sibling))
    end

    @check getid(sibling) == getid(block)

    if isdirty(block) || createstorage
        # Since `storage_ptr` is just a pointer,, we have to manually inform the copy engine
        # how many threads to use.
        nthreads = min(Threads.nthreads(), 4)
        _memcpy!(storage_ptr, pointer(A), length(A); nthreads = nthreads)
    end

    # Deregister this object from local tracking.
    unregister!(PoolType{DRAM}(), M, A)
    replace!(A, storage_ptr)

    # Free the block we just removed.
    free(PoolType{DRAM}(), M, block)
    setsibling!(sibling, Block())

    # If we created storage for this array, we need to register the array in the remote
    # storage tracking.
    createstorage && register!(PoolType{PMM}(), M, A)
    return nothing
end

