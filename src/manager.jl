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

# Maintains Cache management.
mutable struct CacheManager{C}
    # Reference to local objects
    #
    # When multiple objects own the same chunk of memory, then we move the mapping from
    # ID to pointer from the `local_objects` field to the `local_alias` field.
    #
    # This means we still have no allocations for single allocations, but still have
    # the capacity to support multiple CachedArrays to own the same `Block`.
    local_objects::Dict{UInt,Ptr{Ptr{Nothing}}}
    size_of_local::Int

    # Create a new ID for each object registerd in the cache.
    idcount::UInt

    # All objects with remote memory.
    remote_objects::Dict{UInt,Ptr{Ptr{Nothing}}}
    size_of_remote::Int

    # local datastructures
    policy::C
    pmm_heap::CompactHeap{PMM_ALLOCATOR_TYPE}
    dram_heap::CompactHeap{AlignedAllocator}
    cleanlist::Vector{Block}

    # Synchronize access ...
    lock::ReentrantLock

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

    local_objects = Dict{UInt,Ptr{Ptr{Nothing}}}()
    size_of_local = 0
    idcount = one(UInt)

    remote_objects = Dict{UInt,Ptr{Ptr{Nothing}}}()
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

    # Allow movement by default
    allow_movement = true

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
        Block[],
        ReentrantLock(),

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
function cangc(manager::CacheManager)
    # Empty out the cleanlist to deal with anything that's been GC'd
    _cleanup(manager)
    return (localsize(manager) == 0) && (remotesize(manager) == 0)
end

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

getobjects(::PoolType{DRAM}, M::CacheManager) = M.local_objects
getobjects(::PoolType{PMM}, M::CacheManager) = M.remote_objects

adjust_size!(::PoolType{DRAM}, M::CacheManager, x) = M.size_of_local += x
adjust_size!(::PoolType{PMM}, M::CacheManager, x) = M.size_of_remote += x

register!(A) = register!(manager(A), A)
function register!(M::CacheManager, A)
    lock(M.lock) do
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
    end
    return nothing
end

update!(::PoolType{DRAM}, M::CacheManager, A) = update!(M.policy, metadata(A))

# Top level entry points
register!(pool, M::CacheManager, A) = register!(pool, M, metadata(A), _datapointer(A))
unregister!(pool, M::CacheManager, A) = unregister!(pool, M, metadata(A))

## Register a single - no aliasing objects
function register!(pool::PoolType{T}, M::CacheManager, block::Block, ptr) where {T}
    @check block.pool == T
    id = getid(block)
    objects = getobjects(pool, M )
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

# This is called when moving - doesn't maintain reference counting.
function unregister!(pool::PoolType{T}, M::CacheManager, block::Block) where {T}
    id = getid(block)
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
cleanup(A, M = manager(A)) = push!(M.cleanlist, metadata(A))

# Put back all items on the clean list.
function _cleanup(M::CacheManager)
    # Free all blocks in the cleanlist
    for block in M.cleanlist
        # Blocks in the cleanlist can become free if the GC runs while eviction is happening.
        # If that is the case, then the block should:
        #
        # 1. Be marked as free
        # 2. Belong to DRAM
        # 3. Have the `evicting` tag set.
        #
        # If these criteria are set, we don't have any further work we need to do to
        # process this block.
        if isfree(block)
            @check getpool(block) == DRAM
            #@check block.evicting
            continue
        end

        # If this block is in PMM, make sure it doesn't have a sibling - otherwise, that would
        # be an error.
        pool = getpool(block)
        if pool == PMM
            @check getsibling(block) === nothing

            # Deregister from PMM. If there are zero remaining references,
            # then we free the block
            unregister!(PoolType{PMM}(), M, block)
            free(PoolType{PMM}(), M, block)
        elseif pool == DRAM
            unregister!(PoolType{DRAM}(), M, block)

            # Does this block have a sibling?
            sibling = getsibling(block)
            if sibling !== nothing
                @check getid(block) == getid(sibling)
                @check getpool(sibling) == PMM

                # use `unsafe_unregister!` to completely remove this block.
                unregister!(PoolType{PMM}(), M, block)
                free(PoolType{PMM}(), M, sibling)
            end
            free(PoolType{DRAM}(), M, block)
        end
    end
    empty!(M.cleanlist)
    return nothing
end

#####
##### Allocate remote arrays
#####

# TODO: Deprecate?
unsafe_alloc(::Type{T}, x...; kw...) where {T} = convert(Ptr{T}, unsafe_alloc(x...; kw...))

# Try to allocate from DRAM first.
# Then, try to allocate from PMM
function unsafe_alloc(manager::CacheManager, bytes, id = getid(manager))
    ptr = unsafe_alloc(PoolType{DRAM}(), manager, bytes, id)
    ptr === nothing || return ptr

    # No free space. Try to allocate from PMM
    return unsafe_alloc(PoolType{PMM}(), manager, bytes, id)
end

# Remote alloc without a finalizer
function unsafe_alloc(
        ::PoolType{PMM},
        manager::CacheManager,
        bytes,
        id::UInt = getid(manager),
    )

    @timeit "PMM Alloc" begin
        isempty(manager.cleanlist) || _cleanup(manager)
        ptr = alloc(manager.pmm_heap, bytes, id)

        # If we still don't have anything, run a full GC.
        if ptr === nothing
            @timeit "PMM Alloc GC" begin
                GC.gc(true)
                _cleanup(manager)
                ptr = alloc(manager.pmm_heap, bytes, id)
            end
        end
    end

    if ptr === nothing
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
        isempty(manager.cleanlist) || _cleanup(manager)
        ptr = alloc(manager.dram_heap, bytes, id)

        # If allocation failed, try a GC
        if manager.gc_before_evict && ptr === nothing
            @timeit "DRAM Alloc GC" begin
                GC.gc(true)
                _cleanup(manager)
                ptr = alloc(manager.dram_heap, bytes, id)
            end
        end

        if ptr === nothing && manager.allow_movement
            @timeit "DRAM Alloc Eviction" begin
                doeviction!(manager, bytes)
                ptr = alloc(manager.dram_heap, bytes, id)
            end
        end
    end
    return ptr
end

free_convert(x::Array) = pointer(x)
free_convert(x::Ptr) = convert(Ptr{Nothing}, x)
free_convert(x::Block) = _datapointer(x)

free(pool::PoolType, manager::CacheManager, x) = free(pool, manager, free_convert(x))
free(::PoolType{DRAM}, manager::CacheManager, ptr::Ptr{Nothing}) = free(manager.dram_heap, ptr)
free(::PoolType{PMM}, manager::CacheManager, ptr::Ptr{Nothing}) = free(manager.pmm_heap, ptr)

function doeviction!(manager, bytes)
    # The full storage storage types for policies may contain
    # extra metadata.
    stack = fulleltype(manager.policy)[]

    # The eviction callback
    cb = block -> moveto!(PoolType{PMM}(), block, manager)

    # TODO: Since the heap upgrade, we can restructure this.
    local block
    while true
        # Pop an item from the policy
        push!(stack, fullpop!(manager.policy))
        block = getval(Block, last(stack))

        canallocfrom(manager.dram_heap, block, bytes) && break

        # Display the stack because what in the world happened??
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

moveto!(pool, A, M = manager(A); kw...) = moveto!(pool, metadata(A), M; kw...)

function moveto!(
        dram::PoolType{DRAM},
        block::Block,
        M::CacheManager;
        dirty = true,
        write_before_read = false
    )

    id = getid(block)

    @lock M.lock begin
        # If the data is already local, mark a usage and return.
        if getpool(block) == DRAM
            # TODO: Fix
            #update!(PoolType{DRAM}(), M, A)
            return nothing
        end

        # If this block is NOT in DRAM, then it shouldn't have a sibling.
        @check getsibling(block) === nothing

        # Otherwise, we need to allocate some local data for it.
        storage_ptr = unsafe_alloc(PoolType{DRAM}(), M, length(block), id)

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
        register!(dram, M, newblock, ptr)

        # Set the dirty flag.
        # If `write_before_read` is true, then we MUST mark this block as dirty.
        flag = write_before_read | dirty
        setdirty!(newblock, flag)
        setdirty!(M.policy, newblock, flag)
    end
    return nothing
end

function moveto!(pool::PoolType{PMM}, block::Block, M::CacheManager)
    id = getid(block)
    @lock M.lock begin
        # If already in PMM, do nothing
        getpool(block) == PMM && return nothing
        sibling = getsibling(block)

        # If this block has a sibling and it is clean, we can elide write back.
        # Otherwise, we have to write back.
        createstorage = (sibling === nothing)
        if createstorage
            storage_ptr = unsafe_alloc(PoolType{PMM}(), M, length(block), id)
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
        createstorage && register!(pool, M, sibling, ptr)

        # Deregister this object from local tracking.
        unregister!(dram, M, block)

        # Free the block we just removed.
        free(dram, M, block)
        setsibling!(sibling, Block())
    end
    return nothing
end

