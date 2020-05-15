@testset "Testing Operation of Cache Manager" begin
    DRAM = CachedArrays.DRAM
    PMM = CachedArrays.PMM
    PoolType = CachedArrays.PoolType

    GC.gc(true)
    manager = CachedArrays.CacheManager(
        @__DIR__;
        localsize = 2^20,
        remotesize = 2^20,
        minallocation = 12
    )

    @test length(manager.local_objects) == 0
    @test length(manager.remote_objects) == 0

    A = CachedArray{UInt8}(undef, manager, (500000,))
    @test CachedArrays.localsize(manager) == sizeof(A)
    @test CachedArrays.pool(A) == DRAM
    if CachedArrays.DEBUG
        @test_throws AssertionError CachedArrays.register!(PoolType{DRAM}(), manager, A)
    end

    B = CachedArray{UInt8}(undef, manager, (300000,))
    @test CachedArrays.localsize(manager) == sizeof(A) + sizeof(B)
    @test CachedArrays.remotesize(manager) == 0
    @test CachedArrays.pool(A) == DRAM
    @test CachedArrays.pool(B) == DRAM

    # When we insert C, A should be evicted.
    C = CachedArray{UInt8}(undef, manager, (500000,))
    @test CachedArrays.localsize(manager) == sizeof(B) + sizeof(C)
    @test CachedArrays.remotesize(manager) == sizeof(A)
    @test CachedArrays.pool(A) == PMM
    @test CachedArrays.pool(B) == DRAM
    @test CachedArrays.pool(C) == DRAM
    if CachedArrays.DEBUG
        @test_throws AssertionError CachedArrays.register!(PoolType{PMM}(), manager, A)
    end

    # Prefetch A, should kick out both B.
    # C should stay in the cache.
    CachedArrays.prefetch!(A)
    @test pointer(A) != pointer(C)
    @test CachedArrays.localsize(manager) == sizeof(A) + sizeof(C)
    @test CachedArrays.remotesize(manager) == sizeof(B) + sizeof(A)
    @test CachedArrays.pool(A) == DRAM
    @test CachedArrays.pool(B) == PMM
    @test CachedArrays.pool(C) == DRAM
end

@testset "Testing Manager Cleanup" begin
    v = CachedArrays.gc_managers()
    @test v == 1
end
