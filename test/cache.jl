mutable struct Dummy
    size::Int
end

# Hijack `sizeof` to see if the manager updates correctly.
Base.sizeof(D::Dummy) = D.size

@testset "Testing Operation of Cache Manager" begin
    manager = CachedArrays.CacheManager{Dummy}(@__DIR__)

    x = Dummy(10)
    CachedArrays.registerlocal!(manager, x)
    @test CachedArrays.inlocal(manager, x)
    @test manager.size_of_local == sizeof(x)

    # Make sure adding this again doesn't change the recorded size.
    # TODO: Consider whether registering more than once should be an error.
    # It seems like it should be ...
    CachedArrays.registerlocal!(manager, x)
    @test manager.size_of_local == sizeof(x)

    y = Dummy(1000)
    CachedArrays.registerlocal!(manager, y)
    @test CachedArrays.inlocal(manager, y)
    @test manager.size_of_local == sizeof(x) + sizeof(y)

    # Delete an entry
    CachedArrays.freelocal!(manager, x)
    CachedArrays.registerremote!(manager, x)

    @test CachedArrays.inlocal(manager, y)
    @test !CachedArrays.inlocal(manager, x)
    @test CachedArrays.inremote(manager, x)

    @test manager.size_of_local == sizeof(y)
    @test manager.size_of_remote == sizeof(x)

    # Free `x` again from local - make sure sizes are tracked correctly
    CachedArrays.freelocal!(manager, x)
    @test !CachedArrays.inlocal(manager, x)
    @test manager.size_of_local == sizeof(y)

    CachedArrays.freeremote!(manager, x)
    @test !CachedArrays.inlocal(manager, x)
    @test iszero(manager.size_of_remote)
end
