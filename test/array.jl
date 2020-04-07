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
    @test manager.size_of_local == sizeof(A)

    @test !CachedArrays.inremote(manager, A)
    @test manager.size_of_remote == 0

    # Evict this object.
    CachedArrays.evict!(A)

    # Make sure our query functions work
    @test CachedArrays.isparent(A)
    @test CachedArrays.hasparent(A)
    @test !CachedArrays.islocal(A)

    # Make sure the cache gets updated.
    @test !CachedArrays.inlocal(manager, A)
    @test manager.size_of_local == 0

    @test CachedArrays.inremote(manager, A)
    @test manager.size_of_remote == sizeof(A)

    # Now prefetch the array back
    CachedArrays.prefetch!(A)
    @test !CachedArrays.isparent(A)
    @test CachedArrays.hasparent(A)
    @test CachedArrays.islocal(A)

    # The cache maanger should now have this array stored at both locations.
    @test CachedArrays.inlocal(manager, A)
    @test manager.size_of_local == sizeof(A)

    @test CachedArrays.inremote(manager, A)
    @test manager.size_of_remote == sizeof(A)

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
    @test manager.size_of_local == 0
    @test manager.size_of_remote == 0
end

