mutable struct Dummy{T}
    size::Int
    id::UInt
    manager::T
    isremote::Bool
end

function Dummy(size, manager::CachedArrays.CacheManager, isremote = false)
    D = Dummy(
        size,
        CachedArrays.getid(manager),
        manager,
        isremote,
    )

    # Clean this up from the cache when we're done.
    finalizer(D) do x
        if x.isremote
            CachedArrays.freeremote!(x.manager, x)
        else
            CachedArrays.freelocal!(x.manager, x)
        end
    end

    return D
end

CachedArrays.id(x::Dummy) = x.id

function CachedArrays.move_to_remote!(x::Dummy)
    x.isremote = true
    CachedArrays.registerremote!(x.manager, x)
    return nothing
end

function makedummy(size, manager)
    # Make the object and register it with the manager.
    D = Dummy(size, manager, false)
    CachedArrays.registerlocal!(manager, D)
    return D
end

function prefetch!(D::Dummy)
    D.isremote = false
    CachedArrays.registerlocal!(D.manager, D)
    CachedArrays.freeremote!(D.manager, D)
    return nothing
end

# Hijack `sizeof` to see if the manager updates correctly.
Base.sizeof(D::Dummy) = D.size

@testset "Testing Operation of Cache Manager" begin
    # Now, we create a new cache manager with a smaller size to make sure that elements
    # of the caching mechanisms are working correctly.
    manager = CachedArrays.CacheManager{CachedArrays.LRUCache{UInt}}(@__DIR__, 1000)
    A = makedummy(600, manager)
    @test CachedArrays.localsize(manager) == sizeof(A)
    @test A.isremote == false

    B = makedummy(300, manager)
    @test CachedArrays.localsize(manager) == sizeof(A) + sizeof(B)
    @test CachedArrays.remotesize(manager) == 0
    @test A.isremote == false
    @test B.isremote == false

    # When we insert C, A should be evicted.
    C = makedummy(500, manager)
    @test CachedArrays.localsize(manager) == sizeof(B) + sizeof(C)
    @test CachedArrays.remotesize(manager) == sizeof(A)
    @test A.isremote == true
    @test B.isremote == false
    @test C.isremote == false

    # Prefetch A, should kick out both B and C
    prefetch!(A)
    @test CachedArrays.localsize(manager) == sizeof(A)
    @test CachedArrays.remotesize(manager) == sizeof(B) + sizeof(C)
    @test A.isremote == false
    @test B.isremote == true
    @test C.isremote == true
end
