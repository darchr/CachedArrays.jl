@testset "Testing Policy" begin
    @testset "Testing OptaneTracker Macro" begin
        @test CachedArrays.@OptaneTracker() == CachedArrays.OptaneTracker
        @test CachedArrays.@OptaneTracker(2) == CachedArrays.OptaneTracker{2}
        # Test the keyword arguments
        @test CachedArrays.@OptaneTracker(AllowsPrefetch = true) ==
              CachedArrays.OptaneTracker{<:Any,true}
        @test CachedArrays.@OptaneTracker(2, AllowsPrefetch = true) ==
              CachedArrays.OptaneTracker{2,true}
        @test CachedArrays.@OptaneTracker(AllowsUnlinked = true) ==
              CachedArrays.OptaneTracker{<:Any,<:Any,true}
        @test CachedArrays.@OptaneTracker(AllowsNoescape = true) ==
              CachedArrays.OptaneTracker{<:Any,<:Any,<:Any,true}
        @test CachedArrays.@OptaneTracker(AllowsCleanup = true) ==
              CachedArrays.OptaneTracker{<:Any,<:Any,<:Any,<:Any,true}

        @test CachedArrays.@OptaneTracker(
            AllowsPrefetch = false,
            AllowsUnlinked = true,
            AllowsNoescape = false,
            AllowsCleanup = true
        ) == CachedArrays.OptaneTracker{<:Any,false,true,false,true}

        # Make sure function dispatch works correctly.
        test_ndims(::CachedArrays.@OptaneTracker(N)) where {N} = N
        function test_ndims(
            ::CachedArrays.@OptaneTracker(N, AllowsCleanup = false)
        ) where {N}
            return N + 1
        end
        tracker = CachedArrays.OptaneTracker((1, 2, 3))
        @test test_ndims(tracker) == 3
        tracker = CachedArrays.OptaneTracker((1, 2, 3); allows_cleanup = false)
        @test test_ndims(tracker) == 4
    end

    @testset "Testing \"AllowsPrefetch\"" begin
        # The idea here is to use two managers.
        # * `manager_prefetch` allows prefetching.
        # * `manager_noprefetch` does not allow prefetching.
        # Note: BOTH should allows the low level `forceprefetch!`.
        manager_prefetch = CachedArrays.CacheManager(
            CachedArrays.AlignedAllocator(),
            CachedArrays.AlignedAllocator();
            localsize = 100_000_000,
            remotesize = 200_000_000,
            minallocation = 10,
            policy = CachedArrays.OptaneTracker((2^10,)),
        )

        manager_noprefetch = CachedArrays.CacheManager(
            CachedArrays.AlignedAllocator(),
            CachedArrays.AlignedAllocator();
            localsize = 100_000_000,
            remotesize = 200_000_000,
            minallocation = 10,
            policy = CachedArrays.OptaneTracker((2^10,); allows_prefetch = false),
        )

        x_prefetch = CachedArrays.CachedArray{Float32}(
            undef, manager_prefetch, 10000; priority = CachedArrays.ForceRemote
        )
        x_noprefetch = CachedArrays.CachedArray{Float32}(
            undef, manager_noprefetch, 10000; priority = CachedArrays.ForceRemote
        )

        @test CachedArrays.metadata(x_prefetch).pool == CachedArrays.Remote
        @test CachedArrays.metadata(x_noprefetch).pool == CachedArrays.Remote
        CachedArrays.prefetch!(x_prefetch)
        CachedArrays.prefetch!(x_noprefetch)

        @test CachedArrays.metadata(x_prefetch).pool == CachedArrays.Local
        @test CachedArrays.metadata(x_prefetch).sibling.pool == CachedArrays.Remote

        @test CachedArrays.metadata(x_noprefetch).pool == CachedArrays.Remote
        @test CachedArrays.isnull(CachedArrays.metadata(x_noprefetch).sibling)

        # Make sure eviction still works when prefetching is disabled.
        x_noprefetch_2 = similar(x_noprefetch)
        @test CachedArrays.metadata(x_noprefetch_2).pool == CachedArrays.Local
        CachedArrays.evict!(x_noprefetch_2)
        @test CachedArrays.metadata(x_noprefetch_2).pool == CachedArrays.Remote
        CachedArrays.prefetch!(x_noprefetch_2)
        @test CachedArrays.metadata(x_noprefetch_2).pool == CachedArrays.Remote
    end
    CachedArrays.gc_managers()

    @testset "Testing \"AllowsUnlinked\"" begin
        # The idea here is to use two managers.
        # One manager (manager_unlinked) allows for regions in local memory to exist on
        # their own.
        # The other (manager_linked) *should* always have a linked region in remote memory,
        # even when explicitly requested to be local.
        manager_unlinked = CachedArrays.CacheManager(
            CachedArrays.AlignedAllocator(),
            CachedArrays.AlignedAllocator();
            localsize = 100_000_000,
            remotesize = 200_000_000,
            minallocation = 10,
            policy = CachedArrays.OptaneTracker((2^10,)),
        )

        manager_linked = CachedArrays.CacheManager(
            CachedArrays.AlignedAllocator(),
            CachedArrays.AlignedAllocator();
            localsize = 100_000_000,
            remotesize = 200_000_000,
            minallocation = 10,
            policy = CachedArrays.OptaneTracker((2^10,); allows_unlinked = false),
        )

        x_unlinked = CachedArrays.CachedArray{Float32}(
            undef, manager_unlinked, 10000; priority = CachedArrays.ForceLocal
        )
        x_linked = CachedArrays.CachedArray{Float32}(
            undef, manager_linked, 10000; priority = CachedArrays.ForceLocal
        )

        meta_unlinked = CachedArrays.metadata(x_unlinked)
        meta_linked = CachedArrays.metadata(x_linked)

        # Both arrays should be in the local pool.
        # However, the unlinked array shouldn't have a sibling in remote memory while
        # the linked array should.
        @test meta_unlinked.pool == CachedArrays.Local
        @test meta_linked.pool == CachedArrays.Local

        @test CachedArrays.isnull(meta_unlinked.sibling)
        @test isa(meta_linked.sibling, CachedArrays.Block)
        @test meta_linked.sibling.pool == CachedArrays.Remote
        @test meta_linked.id == meta_linked.sibling.id

        # When using something like "similar", we should get similar results.
        x_unlinked_2 = similar(x_unlinked)
        x_linked_2 = similar(x_linked)

        meta_unlinked_2 = CachedArrays.metadata(x_unlinked_2)
        meta_linked_2 = CachedArrays.metadata(x_linked_2)
        @test meta_unlinked_2.pool == CachedArrays.Local
        @test meta_linked_2.pool == CachedArrays.Local

        @test CachedArrays.isnull(meta_unlinked_2.sibling)
        @test isa(meta_linked_2.sibling, CachedArrays.Block)
        @test meta_linked_2.sibling.pool == CachedArrays.Remote
        @test meta_linked_2.id == meta_linked_2.sibling.id
    end
    CachedArrays.gc_managers()

    @testset "Testing \"AllowsNoescape\"" begin
        manager_with_noescape = CachedArrays.CacheManager(
            CachedArrays.AlignedAllocator(),
            CachedArrays.AlignedAllocator();
            localsize = 100_000_000,
            remotesize = 200_000_000,
            minallocation = 10,
            policy = CachedArrays.OptaneTracker((2^10,)),
        )

        manager_without_noescape = CachedArrays.CacheManager(
            CachedArrays.AlignedAllocator(),
            CachedArrays.AlignedAllocator();
            localsize = 100_000_000,
            remotesize = 200_000_000,
            minallocation = 10,
            policy = CachedArrays.OptaneTracker((2^10,); allows_noescape = false),
        )

        function allocates_many_intermediates(x)
            for _ = 1:100
                x = similar(x)
            end
            return x
        end

        x_with_noescape = CachedArrays.CachedArray{Float32}(
            undef, manager_with_noescape, 10000; priority = CachedArrays.ForceLocal, status = CachedArrays.ReadWrite(),
        )
        x_without_noescape = CachedArrays.CachedArray{Float32}(
            undef, manager_without_noescape, 10000; priority = CachedArrays.ForceLocal, status = CachedArrays.ReadWrite(),
        )

        y_with_noescape = CachedArrays.@noescape(
            manager_with_noescape, allocates_many_intermediates(x_with_noescape)
        )
        @test typeof(y_with_noescape) == typeof(x_with_noescape)
        y_without_noescape = CachedArrays.@noescape(
            manager_without_noescape, allocates_many_intermediates(x_without_noescape)
        )
        @test typeof(y_without_noescape) == typeof(x_without_noescape)

        # Run `cangc` to clean-up any pending frees in each cache manager.
        @test CachedArrays.cangc(manager_with_noescape) == false
        @test CachedArrays.cangc(manager_without_noescape) == false

        # The manager that allows @noescape should have only two objects live.
        @test length(CachedArrays.getmap(manager_with_noescape)) == 2
        @test length(CachedArrays.getmap(manager_without_noescape)) == 101
    end

    @testset "Testing \"AllowsCleanup\"" begin
        manager_with_cleanup = CachedArrays.CacheManager(
            CachedArrays.AlignedAllocator(),
            CachedArrays.AlignedAllocator();
            localsize = 100_000_000,
            remotesize = 200_000_000,
            minallocation = 10,
            policy = CachedArrays.OptaneTracker((2^10,)),
        )

        manager_without_cleanup = CachedArrays.CacheManager(
            CachedArrays.AlignedAllocator(),
            CachedArrays.AlignedAllocator();
            localsize = 100_000_000,
            remotesize = 200_000_000,
            minallocation = 10,
            policy = CachedArrays.OptaneTracker((2^10,); allows_cleanup = false),
        )

        x_with_cleanup = CachedArrays.CachedArray{Float32}(
            undef, manager_with_cleanup, 10000; priority = CachedArrays.ForceLocal, status = CachedArrays.ReadWrite(),
        )
        x_without_cleanup = CachedArrays.CachedArray{Float32}(
            undef, manager_without_cleanup, 10000; priority = CachedArrays.ForceLocal, status = CachedArrays.ReadWrite(),
        )

        # Create hierarchical structs out of each
        x = x_with_cleanup
        struct_with_cleanup = (copy(x), (copy(x), copy(x)), x, 10)

        x = x_without_cleanup
        struct_without_cleanup = (copy(x), (copy(x), copy(x)), x, 10)

        CachedArrays.cleanup!(struct_with_cleanup)
        CachedArrays.cleanup!(struct_without_cleanup)

        # Run `cangc` to clean-up any pending frees in each cache manager.
        @test CachedArrays.cangc(manager_with_cleanup) == true
        @test CachedArrays.cangc(manager_without_cleanup) == false

        # The manager that allows @noescape should have only two objects live.
        @test length(CachedArrays.getmap(manager_with_cleanup)) == 0
        @test length(CachedArrays.getmap(manager_without_cleanup)) == 4
    end
end
