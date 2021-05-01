@testset "Testing Operation of Cache Manager" begin
    DRAM = CachedArrays.DRAM
    PMM = CachedArrays.PMM
    PoolType = CachedArrays.PoolType

    GC.gc(true)
    minallocation = 12
    manager = CachedArrays.CacheManager(
        @__DIR__;
        localsize = 2^20,
        remotesize = 2^22,
        minallocation = minallocation,
    )

    @test length(manager.local_objects) == 0
    @test length(manager.remote_objects) == 0

    A = CachedArray{UInt8}(undef, manager, (500000,))
    # Actual allocation should be within 2^minallocation of the actual size of A
    @test CachedArrays.inlocal(manager, A)
    @test CachedArrays.pool(A) == DRAM
    # TODO: Acquire Lock
    # if CachedArrays.DEBUG
    #     @test_throws AssertionError CachedArrays.unsafe_register!(
    #         PoolType{DRAM}(),
    #         manager,
    #         CachedArrays.metadata(A),
    #         CachedArrays._datapointer(A),
    #     )
    # end

    B = CachedArray{UInt8}(undef, manager, (300000,))
    @test CachedArrays.inlocal(manager, A)
    @test CachedArrays.inlocal(manager, B)

    @test isempty(manager.remote_objects) == true
    @test CachedArrays.pool(A) == DRAM
    @test CachedArrays.pool(B) == DRAM

    # When we insert C, A should be evicted.
    C = CachedArray{UInt8}(undef, manager, (500000,))

    @test CachedArrays.inlocal(manager, B)
    @test CachedArrays.inlocal(manager, C)
    @test !CachedArrays.inlocal(manager, A)
    @test CachedArrays.inremote(manager, A)

    @test CachedArrays.pool(A) == PMM
    @test CachedArrays.pool(B) == DRAM
    @test CachedArrays.pool(C) == DRAM
    # TODO: Acquire Lock
    # if CachedArrays.DEBUG
    #     @test_throws AssertionError CachedArrays.unsafe_register!(
    #         PoolType{PMM}(),
    #         manager,
    #         CachedArrays.metadata(A),
    #         CachedArrays._datapointer(A),
    #     )
    # end

    # Prefetch A, should kick out both B.
    # C should stay in the cache.
    CachedArrays.prefetch!(A)
    @test pointer(A) != pointer(C)
    @test CachedArrays.inlocal(manager, A)
    @test !CachedArrays.inlocal(manager, B)
    @test CachedArrays.inlocal(manager, C)

    @test CachedArrays.inremote(manager, A)
    @test CachedArrays.inremote(manager, B)
    @test !CachedArrays.inremote(manager, C)

    @test CachedArrays.pool(A) == DRAM
    @test CachedArrays.pool(B) == PMM
    @test CachedArrays.pool(C) == DRAM

    # Now - test that locking movement works correctly.
    # If we disable movement and then allocate a new array, it should be allocated in PMM
    CachedArrays.disable_movement!(manager)
    D = similar(B)

    @test CachedArrays.pool(A) == DRAM
    @test CachedArrays.pool(B) == PMM
    @test CachedArrays.pool(C) == DRAM
    @test CachedArrays.pool(D) == PMM

    # If we renable movement - then a new allocation should happen in DRAM with an evicion.
    CachedArrays.enable_movement!(manager)
    E = similar(B)

    @test CachedArrays.pool(A) == DRAM
    @test CachedArrays.pool(B) == PMM
    @test CachedArrays.pool(C) == PMM
    @test CachedArrays.pool(D) == PMM
    @test CachedArrays.pool(E) == DRAM
end

@testset "Testing Manager Cleanup" begin
    v = CachedArrays.gc_managers()
    @test v == 1
end
