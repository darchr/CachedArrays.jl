# Maintains Cache management.
mutable struct CacheManager{C,P,Q}
    # Reference to local objects
    #
    # This dict is keyed by an object's `id` which should be obtained from the manager upon
    # object creation using `getid`. The value is a `WeakRef` to allow the object to be
    # GC'd even if it lives in this cache.
    #
    # NOTE: Make sure that the manager is updated whenever an object that enters itself
    # into the cache is finalized.
    local_objects::Dict{UInt,Tuple{Block,WeakRef}}
    size_of_local::Int

    # Create a new ID for each object registerd in the cache.
    idcount::UInt

    # All objects with remote memory.
    # Useful for defragmentation.
    remote_objects::Dict{UInt,Tuple{Block,WeakRef}}
    size_of_remote::Int

    # local datastructures
    policy::C
    pmm_heap::P
    dram_heap::Q

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
        remotesize = 1_000_000_000
    ) where {T}

    # If we're in 2LM, pass a nullptr.
    # MemKindAllocator gets swapped to something that throws an error if called.
    if IS_2LM
        kind = MemKind.Kind(Ptr{Nothing}(0))
    else
        # For now, pass 0 - which essentially allows unlimited memory
        kind = MemKind.create_pmem(path, 0)
    end

    local_objects = Dict{UInt,Tuple{Block,WeakRef}}()
    size_of_local = 0
    idcount = one(UInt)

    remote_objects = Dict{UInt,Tuple{Block,WeakRef}}()
    size_of_remote = 0

    # Initialize the cache
    policy = LRU{UInt}(localsize)

    pmm_heap = BuddyHeap(MemKindAllocator(kind), remotesize; pool = PMM)
    dram_heap = BuddyHeap(AlignedAllocator(), localsize; pool = DRAM)

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
        Float32(1),     # flushpercent
        true,           # gc_before_evict
    )

    # Add this to the global manager list to ensure that it outlives any of its users
    push!(GlobalManagers, manager)

    return manager
end

# Get the block for the given object.
Block(M::CacheManager, id::Integer) = first(M.local_objects[id])
Base.get(M::CacheManager, id::Integer) = last(M.local_objects[id]).value

# Mark as Dirty
# Add a bit of indirection so in the future, we can notify the manager that a block gets
# transitioned to dirty
setdirty!(A, flag::Bool = true) = setdirty!(A, manager(A), flag)
setdirty!(A, M::CacheManager, flag::Bool = true) = (Block(A).dirty = flag)

isdirty(A) = Block(A).dirty
id(x) = Block(x).id
pool(x) = Block(x).pool

prefetch!(A; kw...) = moveto!(PoolType{DRAM}(), A, manager(A); kw...)
evict!(A; kw...) = moveto!(PoolType{PMM}(), A, manager(A); kw...)

manager(x) = error("Implement `manager` for $(typeof(x))")

function getid(manager::CacheManager)
    id = manager.idcount
    manager.idcount += 1
    return id
end

cangc(manager::CacheManager) = (localsize(manager) == 0) && (remotesize(manager) == 0)

function Base.show(io::IO, M::CacheManager)
    println(io, "Cache Manager")
    println(io, "    $(length(M.local_objects)) Local Objects")
    println(io, "    $(length(M.remote_objects)) Remote Objects")
    println(io, "    $(localsize(M) / 1E9) GB Local Memory Used of $(M.dram_heap.len / 1E9)")
    println(io, "    $(remotesize(M) / 1E9) GB Remote Memory Used of $(M.pmm_heap.len / 1E9)")
end

function Base.resize!(M::CacheManager, maxsize; maxallocation = nothing)
    # Make sure there's nothing in the cache.
    if !isempty(M.local_objects)
        error("Can only resize empty caches!")
    end

    # Step 2 - replace the heap with the new size.
    newheap = BuddyHeap(M.dram_heap.allocator, maxsize; maxallocation = maxallocation)
    M.dram_heap = newheap
    return nothing
end

function resize_remote!(M::CacheManager, maxsize; maxallocation = nothing)
    if !isempty(M.remote_objects)
        error("Can only resize empty caches!")
    end
    newheap = BuddyHeap(M.pmm_heap.allocator, maxsize; maxallocation = maxallocation)
    M.pmm_heap = newheap
    return nothing
end

# TODO: Rename these
inlocal(manager, x) = haskey(manager.local_objects, id(x))
inremote(manager, x) = haskey(manager.remote_objects, id(x))
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
    block = Block(A)
    id = block.id

    # Correctness Checks
    @check block.pool == DRAM
    @check !haskey(M.local_objects, id)

    # Add to local data structures
    push!(M.policy, id)
    M.local_objects[id] = (block, WeakRef(A))
    M.size_of_local += sizeof(A)
    return nothing
end

update!(::PoolType{DRAM}, M::CacheManager, A) = update!(M.policy, id(A))

function unregister!(::PoolType{DRAM}, M::CacheManager, A)
    block = Block(A)
    id = block.id

    # Correctness checks
    @check haskey(M.local_objects, id)

    delete!(M.local_objects, id)
    delete!(M.policy, id)
    M.size_of_local -= sizeof(A)
    return nothing
end

function register!(::PoolType{PMM}, M::CacheManager, A)
    block = Block(A)
    id = block.id

    @check !haskey(M.remote_objects, id)
    @check block.pool == PMM
    M.remote_objects[id] = (block, WeakRef(A))
    M.size_of_remote += sizeof(A)
    return nothing
end

function unregister!(::PoolType{PMM}, M::CacheManager, A)
    block = Block(A)
    id = block.id

    @check haskey(M.remote_objects, id)
    delete!(M.remote_objects, id)
    M.size_of_remote -= sizeof(A)
    return nothing
end

#####
##### Cleanup Finalizer
#####

function cleanup(A, M = manager(A))
    # Get the block for this object.
    block = Block(A)

    # If this block is in PMM, make sure it doesn't have a sibling - otherwise, that would
    # be an error.
    if block.pool == PMM
        @check isnull(block.sibling)

        # Deregister from PMM
        unregister!(PoolType{PMM}(), M, A)
        # Free this buffer
        free(PoolType{PMM}(), M, block)
    elseif block.pool == DRAM
        unregister!(PoolType{DRAM}(), M, A)

        # Does this block have a sibling?
        sibling = block.sibling
        if !isnull(sibling)
            @check sibling.id == block.id
            @check sibling.pool == PMM
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

function alloc(
        ::PoolType{PMM},
        manager::CacheManager,
        ::Type{Array{T,N}},
        dims::NTuple{N,Int}
    ) where {T,N}

    A = unsafe_alloc(PoolType{PMM}(), manager, Array{T,N}, dims)
    finalizer(A) do x
        free(manager.pmm_heap, convert(Ptr{Nothing}, pointer(x)))
    end
    return A
end

# Remote alloc without a finalizer
function unsafe_alloc(
        ::PoolType{PMM},
        manager::CacheManager,
        ::Type{Array{T,N}},
        dims::NTuple{N,Int},
        id::UInt = getid(manager),
    ) where {T,N}

    allocsize = sizeof(T) * prod(dims)
    ptr = alloc(manager.pmm_heap, allocsize, id)

    # If we still don't have anything, run a full GC.
    if isnothing(ptr)
        GC.gc(true)
        ptr = alloc(manager.pmm_heap, allocsize)
    end

    return unsafe_wrap(Array, convert(Ptr{T}, ptr), dims; own = false)
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

function alloc(
        ::PoolType{DRAM},
        manager::CacheManager,
        ::Type{Array{T,N}},
        dims::NTuple{N,Int}
    ) where {T,N}

    A = unsafe_alloc(PoolType{DRAM}(), manager, Array{T,N}, dims)
    finalizer(A) do x
        free(manager.dram_heap, convert(Ptr{Nothing}, pointer(x)))
    end
    return A
end

function unsafe_alloc(
        ::PoolType{DRAM},
        manager::CacheManager,
        ::Type{Array{T,N}},
        dims::NTuple{N,Int},
        id::UInt = getid(manager),
    ) where {T,N}

    allocsize = sizeof(T) * prod(dims)
    ptr = alloc(manager.dram_heap, allocsize, id)

    # If allocation failed, try a GC
    if manager.gc_before_evict && isnothing(ptr)
        GC.gc(true)
        ptr = alloc(manager.dram_heap, allocsize, id)
    end

    if isnothing(ptr)
        doeviction!(manager, allocsize)
        ptr = alloc(manager.dram_heap, allocsize, id)
    end

    if isnothing(ptr)
        error("Something's gone horribly wrong!")
    end

    ptr = convert(Ptr{T}, ptr)
    A = unsafe_wrap(Array, ptr, dims; own = false)
    return A
end

free_convert(x::Array) = pointer(x)
free_convert(x::Ptr) = convert(Ptr{Nothing}, x)
free_convert(x::Block) = datapointer(x)

free(pool, manager::CacheManager, x) = free(pool, manager, free_convert(x))
free(::PoolType{DRAM}, manager::CacheManager, ptr::Ptr{Nothing}) = free(manager.dram_heap, ptr)
free(::PoolType{PMM}, manager::CacheManager, ptr::Ptr{Nothing}) = free(manager.pmm_heap, ptr)

function doeviction!(manager, allocsize)
    # The full storage storage types for policies may contain
    # extra metadata.
    stack = fulleltype(manager.policy)[]

    # The eviction callback
    cb = id -> moveto!(PoolType{PMM}(), manager, id)

    local block
    while true
        # Pop an item from the LRU
        push!(stack, fullpop!(manager.policy))
        id = last(stack).val

        # Get the heap Block for this object.
        block = Block(manager, id)

        # Repeat until we get a block that will allow us to do this allocation.
        canallocfrom(manager.dram_heap, block, allocsize) && break

        if isempty(manager.policy)
            display(stack)
        end
    end

    # Put back all of the items in the LRU that we didn't use.
    while !isempty(stack)
        push!(manager.policy, pop!(stack))
    end

    # Perform our managed eviction.
    evictfrom!(manager.dram_heap, block, allocsize; cb = cb)

    # Now that we've guarenteed that we have a block available in our requested size,
    # Clean up items from the cache.
    target = ceil(Int, manager.dram_heap.len * manager.flushpercent)

    while localsize(manager) >= target
        block = Block(manager, pop!(manager.policy))
        evict!(manager.dram_heap, block; cb = cb)
    end

    return nothing
end

#####
##### Moving Objects
#####

function moveto!(::PoolType{DRAM}, A, M = manager(A); dirty = true)
    # Get the metadata for this object.
    block = Block(A)

    # If the data is already local, mark a usage and return.
    if block.pool == DRAM
        update!(PoolType{DRAM}(), M, A)
        return nothing
    end

    # If this block is NOT in DRAM, then it shouldn't have a sibling.
    @check isnull(block.sibling)

    # Otherwise, we need to allocate some local data for it.
    storage = unsafe_alloc(PoolType{DRAM}(), M, arraytype(A), size(A), block.id)
    memcpy!(storage, A)

    # Get the block for the newly created Array.
    # Here, we have to maintain some metadata.
    newblock = unsafe_block(pointer(storage))

    newblock.sibling = block
    block.sibling = newblock
    replace!(A, storage)

    # Track this object
    register!(PoolType{DRAM}(), M, A)

    # Set the dirty flag.
    newblock.dirty = dirty
    return nothing
end

moveto!(pool::PoolType{PMM}, M::CacheManager, id::UInt) = moveto!(pool, get(M, id), M)
function moveto!(::PoolType{PMM}, A, M = manager(A))
    block = Block(A)

    # If already in PMM, do nothing
    block.pool == PMM && return nothing
    sibling = block.sibling

    # If this block has a sibling and it is clean, we can elide write back.
    # Otherwise, we have to write back.
    createstorage = isnull(sibling)
    if createstorage
        storage = unsafe_alloc(PoolType{PMM}(), M, arraytype(A), size(A), block.id)
        sibling = unsafe_block(pointer(storage))
    else
        storage = unsafe_wrap(
            arraytype(A),
            convert(Ptr{eltype(A)}, datapointer(sibling)),
            size(A),
        )
    end

    @check sibling.id == block.id

    if block.dirty || createstorage
        memcpy!(storage, A, true)
    end

    # Deregister this object from local tracking.
    unregister!(PoolType{DRAM}(), M, A)
    replace!(A, storage)

    # Free the block we just removed.
    free(PoolType{DRAM}(), M, block)
    sibling.sibling = Block()

    # If we created storage for this array, we need to register the array in the remote
    # storage tracking.
    createstorage && register!(PoolType{PMM}(), M, A)
    return nothing
end

