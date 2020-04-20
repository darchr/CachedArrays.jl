@testset "Testing Operation of Cache Manager" begin

    GC.gc(true)
    manager = CachedArrays.GlobalManager[]
    resize!(manager, 2^20)
    @test length(manager.local_objects) == 0
    @test length(manager.remote_objects) == 0

    A = CachedArray{UInt8}(undef, (500000,), manager)
    @test CachedArrays.localsize(manager) == sizeof(A)
    @test CachedArrays.islocal(A) == true
    @test_throws AssertionError CachedArrays.registerlocal!(A)

    B = CachedArray{UInt8}(undef, (300000,), manager)
    @test CachedArrays.localsize(manager) == sizeof(A) + sizeof(B)
    @test CachedArrays.remotesize(manager) == 0
    @test CachedArrays.islocal(A) == true
    @test CachedArrays.islocal(B) == true

    # When we insert C, A should be evicted.
    C = CachedArray{UInt8}(undef, (500000,), manager)
    @test CachedArrays.localsize(manager) == sizeof(B) + sizeof(C)
    @test CachedArrays.remotesize(manager) == sizeof(A)
    @test CachedArrays.islocal(A) == false
    @test CachedArrays.islocal(B) == true
    @test CachedArrays.islocal(C) == true
    @test_throws AssertionError CachedArrays.registerremote!(A)

    # Prefetch A, should kick out both B.
    # C should stay in the cache.
    CachedArrays.prefetch!(A)
    @test pointer(A) != pointer(C)
    @test CachedArrays.localsize(manager) == sizeof(A) + sizeof(C)
    @test CachedArrays.remotesize(manager) == sizeof(B) + sizeof(A)
    @test CachedArrays.islocal(A) == true
    @test CachedArrays.islocal(B) == false
    @test CachedArrays.islocal(C) == true
end
