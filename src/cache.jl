# Maintains Cache management.
# TODO: Maybe make one of these per thread to cut down on the number of locks that have
# to be grabbed?
#
# TODO: Start to think about eviction policy.
mutable struct CacheManager{T}
    # Kind pointer from MemKind
    # This keeps track of the remote memory.
    kind::MemKind.Kind

    # TODO: Reintroduce if contention becomse an issue
    # Otherwise, for now, keep main function calls single-threaded.
    #lock::ReentrantLock

    # Reference to local objects
    #
    # Use a WeakKeyDict to avoid holding onto these items during GC.
    # Objects themselves are responsible for updating this dict during finalization.
    local_objects::Dict{UInt,WeakRef}

    # The aggregate size of local allocations.
    size_of_local::Int

    # All objects with remote memory.
    # Useful for defragmentation.
    remote_objects::Dict{UInt,WeakRef}

    # The aggregate size of remote allocations.
    size_of_remote::Int
end

function CacheManager{T}(path::AbstractString, sz = 0) where {T}
    # Allocate the backing memory
    kind = MemKind.create_pmem(path, sz)

    local_objects = Dict{UInt,WeakRef}()
    remote_objects = Dict{UInt,WeakRef}()

    # Construct the manager.
    manager = CacheManager{T}(
        kind,
        local_objects,
        0,
        remote_objects,
        0,
    )

    return manager
end

inlocal(manager, x) = haskey(manager.local_objects, objectid(x))
inremote(manager, x) = haskey(manager.remote_objects, objectid(x))

#####
##### API for adding and removing items from the
#####

function registerlocal!(manager::CacheManager{T}, A::U) where {T, U <: T}
    # Add this array to the list of local objects.
    before = length(manager.local_objects)
    manager.local_objects[objectid(A)] = WeakRef(A)
    if length(manager.local_objects) > before
        manager.size_of_local += sizeof(A)
    end

    return nothing
end

function freelocal!(manager::CacheManager{T}, A::U) where {T, U <: T}
    # Keep track of the pre and post length so we only
    # have to make one lookup in the hash table
    before = length(manager.local_objects)
    delete!(manager.local_objects, objectid(A))
    if length(manager.local_objects) < before
        manager.size_of_local -= sizeof(A)
    end

    return nothing
end

function registerremote!(manager::CacheManager{T}, A::U) where {T, U <: T}
    # Add this array to the list of local objects.
    before = length(manager.remote_objects)
    manager.remote_objects[objectid(A)] = WeakRef(A)
    if length(manager.remote_objects) > before
        manager.size_of_remote += sizeof(A)
    end

    return nothing
end

function freeremote!(manager::CacheManager{T}, A::U) where {T, U <: T}
    # Keep track of the pre and post length so we only
    # have to make one lookup in the hash table
    before = length(manager.remote_objects)
    delete!(manager.remote_objects, objectid(A))
    if length(manager.remote_objects) < before
        manager.size_of_remote -= sizeof(A)
    end

    return nothing
end

#####
##### Defrag support
#####

# TODO: Have arrays be able to opt-in to this kind of defrag updating.
"""
    updateremote!(x, A::Array)

Update the remote storage in `x` to `A`.
"""
function updateremote! end

