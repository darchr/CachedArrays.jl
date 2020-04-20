@testset "Testing LockedArrays" begin
    # Create a new manager so old ones don't collide.
    manager = CachedArrays.CacheManager{CachedArrays.LRU{UInt}}(@__DIR__)

    # Make sure everything has been cleaned up from previous runs.
    @test CachedArrays.localsize(manager) == 0
    @test CachedArrays.remotesize(manager) == 0

    # Resize the manager pool to something reasonable for testing.
    resize!(manager, 1_000_000_000)
    len = 2_000_000

    # Wrap in an `let` block for GC purposes
    let
        A = LockedCachedArray{Float32}(undef, len)
        B = LockedCachedArray{Float32}(undef, len)

        vA = rand(Float32, len)
        vB = rand(Float32, len)

        A .= vA
        B .= vB

        @test A == vA
        @test B == vB

        # Now, A should be dirty.
        @test CachedArrays.isdirty(A)

        # Evict A and prefetch it.
        # A should then be clean.
        @test CachedArrays.islocal(A)
        @test !CachedArrays.hasparent(A)
        CachedArrays.evict!(A)
        @test !CachedArrays.islocal(A)
        @test CachedArrays.hasparent(A)
        CachedArrays.prefetch!(A)
        @test CachedArrays.islocal(A)
        @test CachedArrays.hasparent(A)
        @test !CachedArrays.isdirty(A)

        # Various flavors of "setindex!" should cause an error.
        @test_throws ErrorException A[1] = 10
        @test_throws ErrorException A[:] = vA

        # See if taking a view and then calling setindex causes a problem.
        v = view(A, 1:10)
        @test_throws ErrorException v[1] = 10

        # Make sure the array is still clean.
        @test !CachedArrays.isdirty(A)

        # Now, copy something to it. The act of copying should make it dirty.
        copyto!(A, vA)
        @test CachedArrays.isdirty(A)

        # Clean the array
        CachedArrays.evict!(A)
        CachedArrays.prefetch!(A)

        @test !CachedArrays.isdirty(A)
        copyto!(A, 1, vA, 1, 4)
        @test CachedArrays.isdirty(A)
        CachedArrays.evict!(A)
        CachedArrays.prefetch!(A)

        # Check for the case when doing a mutating broadcast.
        @test !CachedArrays.isdirty(A)
        A .= A + B
        @test CachedArrays.isdirty(A)

        # Make sure that mixing and matching broadcasting still results in a LockedCachedArray.
        C = A .+ B
        @test isa(C, LockedCachedArray)
        C = A .+ vA .+ 1
        @test isa(C, LockedCachedArray)
    end
end
