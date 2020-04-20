# Maintains Cache management.

# Tunable Constants
const SMALL_ALLOC_SIZE = -1   # Arrays with a small enough size aren't tracked

# TODO: Maybe make one of these per thread to cut down on the number of locks that have
# to be grabbed?
#
# TODO: Start to think about eviction policy.
mutable struct CacheManager{C,P,Q}
    # Kind pointer from MemKind
    # This keeps track of the remote memory.
    kind::MemKind.Kind

    # Reference to local objects
    #
    # This dict is keyed by an object's `id` which should be obtained from the manager upon
    # object creation using `getid`. The value is a `WeakRef` to allow the object to be
    # GC'd even if it lives in this cache.
    #
    # NOTE: Make sure that the manager is updated whenever an object that enters itself
    # into the cache is finalized.
    local_objects::Dict{UInt,WeakRef}

    # Create a new ID for each object registerd in the cache.
    object_count::UInt

    # All objects with remote memory.
    # Useful for defragmentation.
    remote_objects::Dict{UInt,WeakRef}

    # The aggregate size of remote allocations.
    size_of_remote::Int
    cache::C
    remote_pool::P
    local_heap::Q
end

function CacheManager{T}(path::AbstractString, maxsize = 1_000_000_000) where {T}
    # Allocate the backing memory
    #
    # For now, pass 0 - which essentially allows unlimited memory
    kind = MemKind.create_pmem(path, 0)

    local_objects = Dict{UInt,WeakRef}()
    object_count = one(UInt)

    remote_objects = Dict{UInt,WeakRef}()
    size_of_remote = 0

    # Initialize the cache
    cache = LRUCache{UInt}(maxsize)

    remote_pool = SimplePool(MemKindAllocator(kind))
    local_heap = Heap(AlignedAllocator(), maxsize)

    # Construct the manager.
    manager = CacheManager{T,typeof(remote_pool),typeof(local_heap)}(
        kind,
        local_objects,
        object_count,
        remote_objects,
        size_of_remote,
        cache,
        remote_pool,
        local_heap,
    )

    return manager
end

function Base.show(io::IO, M::CacheManager)
    println(io, "Cache Manager")
    println(io, "    $(length(M.local_objects)) Local Objects")
    println(io, "    $(length(M.remote_objects)) Remote Objects")
    println(io, "    $(localsize(M) / 1E9) GB Local Memory Used")
    println(io, "    $(remotesize(M) / 1E9) GB Remote Memory Used")
end

function Base.resize!(M::CacheManager, maxsize)
    # Step 1 - resize the LRU cache.
    resize!(M.cache, maxsize; cb = x -> managed_evict(M, x))

    # Step 2 is tricky - we need to resize the heap manager ...
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

# Defer to the local cache
localsize(manager::CacheManager) = currentsize(manager.cache)
remotesize(manager::CacheManager) = manager.size_of_remote

# Manage the eviction of an item from the cache.
function managed_evict(manager::CacheManager, id::UInt, x = manager.local_objects[id].value)
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
    @check !in(id, manager.cache)

    # Thus the only local cleanup we have to do is remove this object from the list
    # of locally tracked objects.
    delete!(manager.local_objects, id)
    return nothing
end

#####
##### API for adding and removing items from the
#####

function registerlocal!(A, M::CacheManager = manager(A))
    @check !haskey(M.local_objects, id(A))

    # Add this array to the list of local objects.
    push!(M.cache, id(A), sizeof(A); cb = x -> managed_evict(M, x))
    M.local_objects[id(A)] = WeakRef(A)

    return nothing
end

function updatelocal!(A, M::CacheManager = manager(A))
    update!(M.cache, id(A), sizeof(A))
end

function freelocal!(A, M::CacheManager = manager(A))
    _id = id(A)
    @check haskey(M.local_objects, _id)

    delete!(M.local_objects, _id)
    delete!(M.cache, _id, sizeof(A))

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

    ptr = convert(Ptr{T}, alloc(manager.remote_pool, sizeof(T) * prod(dims)))
    A = unsafe_wrap(Array, ptr, dims; own = false)
    finalizer(A) do x
        free(manager.remote_pool, convert(Ptr{Nothing}, pointer(x)))
    end
    return A
end

function local_alloc(manager::CacheManager, ::Type{Array{T,N}}, dims::NTuple{N,Int}) where {T,N}
    ptr = alloc(manager.local_heap, sizeof(T) * prod(dims))

    # If allocation failed, try a quick GC
    if isnothing(ptr)
        GC.gc(false)
        ptr = alloc(manager.local_heap, sizeof(T) * prod(dims))
    end

    # If that still didn't work, try a full GC
    if isnothing(ptr)
        GC.gc(true)
        ptr = alloc(manager.local_heap, sizeof(T) * prod(dims))
    end

    ptr = convert(Ptr{T}, ptr)
    A = unsafe_wrap(Array, ptr, dims; own = false)
    finalizer(A) do x
        free(manager.local_heap, convert(Ptr{Nothing}, pointer(x)))
    end
    return A
end

