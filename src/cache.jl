# Maintains Cache management.
mutable struct CacheManager{T}
    # Kind pointer from MemKind
    # This keeps track of the remote memory.
    kind::MemKind.Kind

    # Reference to local objects
    #
    # Use a WeakKeyDict to avoid holding onto these items during GC.
    # Objects themselves are responsible for updating this dict during finalization.
    local_objects::WeakKeyDict{T}

    # All objects with remote memory.
    # Useful for defragmentation.
    remote_objects::WeakKeyDict{T}
end

function CacheManager{T}(path::AbstractString, sz = 0)
    # Allocate the backing memory
    kind = MemKind.create_pmem(path, sz)

    local_objects = WeakKeyDict{T,Nothing}()
    remote_objects = WeakKeyDict{T,Nothing}()

    # Construct the manager.
    manager = CacheManager{T}(
        kind,
        local_objects,
        remote_objects,
    )

    return manager
end

"""
    updateremote!(x, A::Array)

Update the remote storage in `x` to `A`.
"""
function updateremote! end

