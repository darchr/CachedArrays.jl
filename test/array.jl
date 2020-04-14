function gctest(manager)
    len = 1024
    B = rand(Float32, len)
    A = CachedArray{Float32}(undef, len)

    @test isa(A, CachedArray)
    # Test assignment works
    A .= B
    @test A == B
    @test length(A) == len

    # Internal detail
    @test CachedArrays.parent(A) == nothing
    @test !CachedArrays.isparent(A)
    @test !CachedArrays.hasparent(A)
    @test CachedArrays.islocal(A)

    # Make sure the global manager is updated correctly.
    @test CachedArrays.inlocal(manager, A)
    @test CachedArrays.localsize(manager) == sizeof(A)

    @test !CachedArrays.inremote(manager, A)
    @test CachedArrays.remotesize(manager) == 0

    # Evict this object.
    CachedArrays.evict!(A)

    # Make sure our query functions work
    @test CachedArrays.isparent(A)
    @test CachedArrays.hasparent(A)
    @test !CachedArrays.islocal(A)

    # Make sure the cache gets updated.
    @test !CachedArrays.inlocal(manager, A)
    @test CachedArrays.localsize(manager) == 0

    @test CachedArrays.inremote(manager, A)
    @test CachedArrays.remotesize(manager) ==  sizeof(A)

    # Now prefetch the array back
    CachedArrays.prefetch!(A)
    @test !CachedArrays.isparent(A)
    @test CachedArrays.hasparent(A)
    @test CachedArrays.islocal(A)

    # The cache maanger should now have this array stored at both locations.
    @test CachedArrays.inlocal(manager, A)
    @test CachedArrays.localsize(manager) == sizeof(A)

    @test CachedArrays.inremote(manager, A)
    @test CachedArrays.remotesize(manager) == sizeof(A)

    return nothing
end

@testset "Testing Array Correctness" begin
    # Grab ahold of the default manager so we can make sure it gets updated correctly.
    manager = CachedArrays.GlobalManager[]

    # Run the GC test, then run garbage collection.
    gctest(manager)
    GC.gc()

    @test length(manager.local_objects) == 0
    @test length(manager.remote_objects) == 0
    @test CachedArrays.localsize(manager) == 0
    @test CachedArrays.remotesize(manager) == 0

    # Okay, now start doing some operations on arrays.
    len = 2_000_000
    A = CachedArray{Float32}(undef, len)
    B = CachedArray{Float32}(undef, len)

    vA = rand(Float32, len)
    vB = rand(Float32, len)

    A .= vA
    B .= vB

    @test CachedArrays.localsize(manager) == sizeof(A) + sizeof(B)
    @test CachedArrays.remotesize(manager) == 0

    # Do a broadcasting add - make sure the newly created object is a CachedArray
    # and that it gets registered with the cache.
    C = A .+ B
    @test isa(C, CachedArray)
    @test length(manager.local_objects) == 3
    @test CachedArrays.localsize(manager) == sizeof(A) + sizeof(B) + sizeof(C)

    # Now do some timing to make sure the overhead of CachedArrays isn't stupid.
    f!(C,A,B) = C .= A .+ B
    f!(C,A,B)
    @test (@allocated f!(C,A,B)) == 0
end

