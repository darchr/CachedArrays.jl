function gctest(manager)
    Local = CachedArrays.Local
    Remote = CachedArrays.Remote
    @show manager

    len = 1024
    B = rand(Float32, len)
    A = CachedArray{Float32}(undef, manager, len; status = CachedArrays.ReadWrite())

    @test isa(A, CachedArray)
    # Test assignment works
    A .= B
    @test A == B
    @test length(A) == len

    @test CachedArrays.pool(A) == Local

    # Make sure the global manager is updated correctly.
    @test inlocal(manager, A)
    @test !inremote(manager, A)

    # Evict this object.
    CachedArrays.evict!(A)

    # Make sure our query functions work
    @test CachedArrays.pool(A) == Remote
    @test A == B

    # Make sure the cache gets updated.
    @test !inlocal(manager, A)

    @test inremote(manager, A)

    # Now prefetch the array back
    CachedArrays.prefetch!(A)
    @test CachedArrays.pool(A) == Local
    @test A == B

    # The cache maanger should now have this array stored at both locations.
    @test inlocal(manager, A)
    @test inremote(manager, A; primary_only = false)
    return nothing
end

@testset "Testing Array Correctness" begin
    # Grab ahold of the default manager so we can make sure it gets updated correctly.
    GC.gc(true)
    manager = CachedArrays.CacheManager(
        CachedArrays.AlignedAllocator(),
        CachedArrays.AlignedAllocator();
        localsize = 1_000_000_000,
        remotesize = 1_000_000_000,
        minallocation = 12,
    )

    # Run the GC test, then run garbage collection.
    gctest(manager)
    @test CachedArrays.check(manager)
    GC.gc(true)
    foreach(x -> CachedArrays.unsafe_free(manager, x), values(manager.map))
    @test CachedArrays.check(manager)

    # Wrap this body of code in a "let" block to ensure that GC will clean up any temporary
    # arrays created in the body.
    let
        Local, Remote = CachedArrays.Local, CachedArrays.Remote
        @test length(CachedArrays.visible_ids(manager, Local; primary_only = true)) == 0
        @test length(CachedArrays.visible_ids(manager, Remote; primary_only = true)) == 0
        # Okay, now start doing some operations on arrays.
        len = 2_000_000
        A = CachedArray{Float32}(undef, manager, len; status = CachedArrays.ReadWrite())
        B = CachedArray{Float32}(undef, manager, len; status = CachedArrays.ReadWrite())

        vA = rand(Float32, len)
        vB = rand(Float32, len)

        A .= vA
        B .= vB

        @test inlocal(manager, A)
        @test inlocal(manager, B)
        #@test CachedArrays.getsize(manager.remotemap) == 0

        # Do a broadcasting add - make sure the newly created object is a CachedArray
        # and that it gets registered with the cache.
        C = A .+ B
        @test isa(C, CachedArray)
        @test length(CachedArrays.visible_ids(manager, Local; primary_only = true)) == 3

        @test inlocal(manager, A)
        @test inlocal(manager, B)
        @test inlocal(manager, C)

        # Now do some timing to make sure the overhead of CachedArrays isn't stupid.
        f!(C, A, B) = C .= A .+ B
        f!(C, A, B)
        @test (@allocated f!(C, A, B)) == 0
    end
    foreach(x -> CachedArrays.unsafe_free(manager, x), values(manager.map))
end

@testset "Testing Cleanup" begin
    @test CachedArrays.gc_managers() == 1
end

