@testset "Testing Operation of Cache Manager" begin
    Local = CachedArrays.Local
    Remote = CachedArrays.Remote
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

    A = CachedArray{UInt8}(undef, manager, 500000; priority = CachedArrays.ForceLocal)
    # Actual allocation should be within 2^minallocation of the actual size of A
    @test CachedArrays.inlocal(manager, A)
    @test CachedArrays.pool(A) == Local
    # TODO: Acquire Lock
    # if CachedArrays.DEBUG
    #     @test_throws AssertionError CachedArrays.unsafe_register!(
    #         PoolType{Local}(),
    #         manager,
    #         CachedArrays.metadata(A),
    #         CachedArrays._datapointer(A),
    #     )
    # end

    B = CachedArray{UInt8}(undef, manager, 300000; priority = CachedArrays.ForceLocal)
    @test CachedArrays.inlocal(manager, A)
    @test CachedArrays.inlocal(manager, B)

    @test isempty(manager.remote_objects) == true
    @test CachedArrays.pool(A) == Local
    @test CachedArrays.pool(B) == Local

    # When we insert C, A should be evicted.
    C = CachedArray{UInt8}(undef, manager, 500000; priority = CachedArrays.ForceLocal)

    @test CachedArrays.inlocal(manager, B)
    @test CachedArrays.inlocal(manager, C)
    @test !CachedArrays.inlocal(manager, A)
    @test CachedArrays.inremote(manager, A)

    @test CachedArrays.pool(A) == Remote
    @test CachedArrays.pool(B) == Local
    @test CachedArrays.pool(C) == Local
    # TODO: Acquire Lock
    # if CachedArrays.DEBUG
    #     @test_throws AssertionError CachedArrays.unsafe_register!(
    #         PoolType{Remote}(),
    #         manager,
    #         CachedArrays.metadata(A),
    #         CachedArrays._datapointer(A),
    #     )
    # end

    # Prefetch A, should kick out both B.
    # C should stay in the cache.
    CachedArrays.prefetch!(A)
    unsafe_pointer(x) = CachedArrays.unsafe_pointer(x)
    @test unsafe_pointer(A) != unsafe_pointer(C)
    @test CachedArrays.inlocal(manager, A)
    @test !CachedArrays.inlocal(manager, B)
    @test CachedArrays.inlocal(manager, C)

    @test CachedArrays.inremote(manager, A)
    @test CachedArrays.inremote(manager, B)
    @test !CachedArrays.inremote(manager, C)

    @test CachedArrays.pool(A) == Local
    @test CachedArrays.pool(B) == Remote
    @test CachedArrays.pool(C) == Local

    # Now - test that locking movement works correctly.
    # If we disable movement and then allocate a new array, it should be allocated in Remote
    CachedArrays.disable_movement!(manager)
    D = similar(B; priority = CachedArrays.ForceLocal)

    @test CachedArrays.pool(A) == Local
    @test CachedArrays.pool(B) == Remote
    @test CachedArrays.pool(C) == Local
    @test CachedArrays.pool(D) == Remote

    # If we renable movement - then a new allocation should happen in Local with an evicion.
    CachedArrays.enable_movement!(manager)
    E = similar(B; priority = CachedArrays.ForceLocal)

    @test CachedArrays.pool(A) == Local
    @test CachedArrays.pool(B) == Remote
    @test CachedArrays.pool(C) == Remote
    @test CachedArrays.pool(D) == Remote
    @test CachedArrays.pool(E) == Local
end

@testset "Testing Manager Cleanup" begin
    v = CachedArrays.gc_managers()
    @test v == 1
end
