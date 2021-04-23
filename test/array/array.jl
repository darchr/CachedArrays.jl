function gctest(manager)
    DRAM = CachedArrays.DRAM
    PMM = CachedArrays.PMM

    len = 1024
    B = rand(Float32, len)
    A = CachedArray{Float32}(undef, manager, len)

    @test isa(A, CachedArray)
    # Test assignment works
    A .= B
    @test A == B
    @test length(A) == len

    @test CachedArrays.pool(A) == DRAM

    # Make sure the global manager is updated correctly.
    @test CachedArrays.inlocal(manager, A)
    @test !CachedArrays.inremote(manager, A)

    # Evict this object.
    CachedArrays.evict!(A)

    # Make sure our query functions work
    @test CachedArrays.pool(A) == PMM
    @test A == B

    # Make sure the cache gets updated.
    @test !CachedArrays.inlocal(manager, A)

    @test CachedArrays.inremote(manager, A)

    # Now prefetch the array back
    CachedArrays.prefetch!(A)
    @test CachedArrays.pool(A) == DRAM
    @test A == B

    # The cache maanger should now have this array stored at both locations.
    @test CachedArrays.inlocal(manager, A)

    @test CachedArrays.inremote(manager, A)
    return nothing
end

@testset "Testing Array Correctness" begin
    # Grab ahold of the default manager so we can make sure it gets updated correctly.
    GC.gc(true)
    manager = CachedArrays.CacheManager(
        @__DIR__;
        localsize = 1_000_000_000,
        remotesize = 1_000_000_000,
        minallocation = 12,
    )

    # Run the GC test, then run garbage collection.
    gctest(manager)
    GC.gc(true)
    CachedArrays._cleanup(manager)

    # Wrap this body of code in a "let" block to ensure that GC will clean up any temporary
    # arrays created in the body.
    let
        @test length(manager.local_objects) == 0
        @test length(manager.remote_objects) == 0
        @test CachedArrays.localsize(manager) == 0
        @test CachedArrays.remotesize(manager) == 0

        # Okay, now start doing some operations on arrays.
        len = 2_000_000
        A = CachedArray{Float32}(undef, manager, len)
        B = CachedArray{Float32}(undef, manager, len)

        vA = rand(Float32, len)
        vB = rand(Float32, len)

        A .= vA
        B .= vB

        @test CachedArrays.inlocal(manager, A)
        @test CachedArrays.inlocal(manager, B)
        @test CachedArrays.remotesize(manager) == 0

        # Do a broadcasting add - make sure the newly created object is a CachedArray
        # and that it gets registered with the cache.
        C = A .+ B
        @test isa(C, CachedArray)
        @test length(manager.local_objects) == 3
        @test CachedArrays.inlocal(manager, A)
        @test CachedArrays.inlocal(manager, B)
        @test CachedArrays.inlocal(manager, C)

        # Now do some timing to make sure the overhead of CachedArrays isn't stupid.
        f!(C,A,B) = C .= A .+ B
        f!(C,A,B)
        @test (@allocated f!(C,A,B)) == 0
    end
end

@testset "Testing Cleanup" begin
    @test CachedArrays.gc_managers() == 1
end


