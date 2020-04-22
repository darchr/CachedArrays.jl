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
    local_objects::Dict{UInt,Tuple{Ptr{Nothing},WeakRef}}
    size_of_local::Int

    # Create a new ID for each object registerd in the cache.
    object_count::UInt

    # All objects with remote memory.
    # Useful for defragmentation.
    remote_objects::Dict{UInt,WeakRef}
    size_of_remote::Int

    # local datastructures
    policy::C
    remote_heap::P
    local_heap::Q

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

function CacheManager{T}(
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

    local_objects = Dict{UInt,Tuple{Ptr{Nothing},WeakRef}}()
    size_of_local = 0
    object_count = one(UInt)

    remote_objects = Dict{UInt,WeakRef}()
    size_of_remote = 0

    # Initialize the cache
    policy = LRU{UInt}(localsize)

    remote_heap = BuddyHeap(MemKindAllocator(kind), remotesize)
    local_heap = BuddyHeap(AlignedAllocator(), localsize)

    # Construct the manager.
    manager = CacheManager{T,typeof(remote_heap),typeof(local_heap)}(
        local_objects,
        size_of_local,
        object_count,
        remote_objects,
        size_of_remote,
        policy,
        remote_heap,
        local_heap,
        # tunables,
        Float32(1),     # flushpercent
        true,           # gc_before_evict
    )

    return manager
end

# Get the block for the given object.
Block(M::CacheManager, id::Integer) = Block(first(M.local_objects[id]) - headersize())

function Base.show(io::IO, M::CacheManager)
    println(io, "Cache Manager")
    println(io, "    $(length(M.local_objects)) Local Objects")
    println(io, "    $(length(M.remote_objects)) Remote Objects")
    println(io, "    $(localsize(M) / 1E9) GB Local Memory Used of $(M.local_heap.len / 1E9)")
    println(io, "    $(remotesize(M) / 1E9) GB Remote Memory Used of $(M.remote_heap.len / 1E9)")
end

function Base.resize!(M::CacheManager, maxsize; maxallocation = nothing)
    # Make sure there's nothing in the cache.
    if !isempty(M.local_objects)
        error("Can only resize empty caches!")
    end

    # Step 2 - replace the heap with the new size.
    newheap = BuddyHeap(M.local_heap.allocator, maxsize; maxallocation = maxallocation)
    M.local_heap = newheap
    return nothing
end

function resize_remote!(M::CacheManager, maxsize; maxallocation = nothing)
    if !isempty(M.remote_objects)
        error("Can only resize empty caches!")
    end
    newheap = BuddyHeap(M.remote_heap.allocator, maxsize; maxallocation = maxallocation)
    M.remote_heap = newheap
    return nothing
end

inlocal(manager, x) = haskey(manager.local_objects, id(x))
inremote(manager, x) = haskey(manager.remote_objects, id(x))

function getid(manager::CacheManager)
    id = manager.object_count
    manager.object_count += 1
    return id
end

id(x) = error("Implement `id` for $(typeof(x))")
manager(x) = error("Implement `manager` for $(typeof(x))")

localsize(manager::CacheManager) = manager.size_of_local
remotesize(manager::CacheManager) = manager.size_of_remote

# Manage the eviction of an item from the cache.
function managed_evict(manager::CacheManager, id::UInt, x = last(manager.local_objects[id]).value)
    # Move this object to the remote store.
    move_to_remote!(x)

    # We could be evicting an object that is already tracking a remote object.
    # If so, there's no need to register.
    if !haskey(manager.remote_objects, id)
        registerremote!(x)
    end

    # Perform out own cleanup.
    # Since this happens on a callback, we can be sure that this object is not in the
    # local cache.
    #
    # However, we perform a debug check anyways.
    @check !in(id, manager.policy)
    @check haskey(manager.local_objects, id)

    # Free this object from our local tracking.
    delete!(manager.local_objects, id)
    manager.size_of_local -= sizeof(x)

    return nothing
end

#####
##### API for adding and removing items from the
#####

function registerlocal!(A, M::CacheManager = manager(A))
    # This object MUST come from our managed heap.
    @check M.local_heap.base <= pointer(A) < M.local_heap.base + M.local_heap.len
    @check !haskey(M.local_objects, id(A))

    # Add this array to the list of local objects.
    push!(M.policy, id(A))
    M.local_objects[id(A)] = (pointer(A), WeakRef(A))
    M.size_of_local += sizeof(A)

    return nothing
end

updatelocal!(A, M::CacheManager = manager(A)) = update!(M.policy, id(A))

function freelocal!(A, M::CacheManager = manager(A))
    _id = id(A)
    @check haskey(M.local_objects, _id)

    delete!(M.local_objects, _id)
    delete!(M.policy, _id)
    M.size_of_local -= sizeof(A)

    return nothing
end

function registerremote!(A, M::CacheManager = manager(A))
    @check !haskey(M.remote_objects, id(A))

    M.remote_objects[id(A)] = WeakRef(A)
    M.size_of_remote += sizeof(A)

    return nothing
end

function freeremote!(A, M::CacheManager = manager(A))
    @check haskey(M.remote_objects, id(A))

    delete!(M.remote_objects, id(A))
    M.size_of_remote -= sizeof(A)

    return nothing
end

#####
##### Allocate remote arrays
#####

function remote_alloc(manager::CacheManager, ::Type{Array{T,N}}, dims::NTuple{N,Int}) where {T,N}
    A = unsafe_remote_alloc(manager, Array{T,N}, dims)
    finalizer(A) do x
        free(manager.remote_heap, convert(Ptr{Nothing}, pointer(x)))
    end
    return A
end

# Remote alloc without a finalizer
function unsafe_remote_alloc(
        manager::CacheManager,
        ::Type{Array{T,N}},
        dims::NTuple{N,Int}
    ) where {T,N}

    allocsize = sizeof(T) * prod(dims)
    ptr = alloc(manager.remote_heap, allocsize)

    # If we still don't have anything, run a full GC.
    if isnothing(ptr)
        GC.gc(true)
        ptr = alloc(manager.remote_heap, allocsize)
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

function local_alloc(manager::CacheManager, ::Type{Array{T,N}}, dims::NTuple{N,Int}) where {T,N}
    A = unsafe_local_alloc(manager, Array{T,N}, dims)
    finalizer(A) do x
        free(manager.local_heap, convert(Ptr{Nothing}, pointer(x)))
    end
    return A
end

function unsafe_local_alloc(
        manager::CacheManager,
        ::Type{Array{T,N}},
        dims::NTuple{N,Int},
        id::UInt
    ) where {T,N}

    allocsize = sizeof(T) * prod(dims)
    ptr = alloc(manager.local_heap, allocsize, id)

    # If allocation failed, try a GC
    if manager.gc_before_evict && isnothing(ptr)
        GC.gc(true)
        ptr = alloc(manager.local_heap, allocsize, id)
    end

    if isnothing(ptr)
        doeviction!(manager, allocsize)
        ptr = alloc(manager.local_heap, allocsize, id)
    end

    if isnothing(ptr)
        error("Something's gone horribly wrong!")
    end

    ptr = convert(Ptr{T}, ptr)
    A = unsafe_wrap(Array, ptr, dims; own = false)
    return A
end

local_free(manager::CacheManager, array::Array) = local_free(manager, pointer(array))
local_free(manager::CacheManager, ptr::Ptr) = local_free(manager, convert(Ptr{Nothing}, ptr))
local_free(manager::CacheManager, ptr::Ptr{Nothing}) = free(manager.local_heap, ptr)

function doeviction!(manager, allocsize)
    # The full storage storage types for policies may contain
    # extra metadata.
    stack = fulleltype(manager.policy)[]

    # The eviction callback
    cb = id -> managed_evict(manager, id)

    local block
    while true
        # Pop an item from the LRU
        push!(stack, fullpop!(manager.policy))
        id = last(stack).val

        # Get the heap Block for this object.
        block = Block(manager, id)

        # Repeat until we get a block that will allow us to do this allocation.
        canallocfrom(manager.local_heap, block, allocsize) && break

        if isempty(manager.policy)
            display(stack)
        end
    end

    # Put back all of the items in the LRU that we didn't use.
    pop!(stack)
    while !isempty(stack)
        push!(manager.policy, pop!(stack))
    end

    # Perform our managed eviction.
    evictfrom!(manager.local_heap, block, allocsize; cb = cb)

    # Now that we've guarenteed that we have a block available in our requested size,
    # Clean up items from the cache.
    target = ceil(Int, manager.local_heap.len * manager.flushpercent)

    while localsize(manager) >= target
        block = Block(manager, pop!(manager.policy))
        evict!(manager.local_heap, block; cb = cb)
    end

    return nothing
end

