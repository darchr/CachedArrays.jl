@testset "Testing Corner Cases" begin
    Local, Remote = CachedArrays.Local, CachedArrays.Remote
    manager = CachedArrays.CacheManager(
        CachedArrays.AlignedAllocator(),
        CachedArrays.AlignedAllocator();
        localsize = 100_000_000,
        remotesize = 500_000_000,
        minallocation = 12, # 4096 Bytes
    )

    # Case 1: Requesting movement of a Region that is in the free buffer.
    let
        _a = CachedArrays.CachedArray{Float32}(undef, manager, 10_000)
        ### Unsafe operation.
        # Run finalizers on the `Region` backing `_a`.
        # This will put it on the manager's freebuffer.
        # Don't use `_a` after this operation.
        finalize(_a.object)

        # When we manually invoke an eviction, the manager will discover during allocation
        # that this object is queued to be freed.
        #
        # As such, it will not even move the array and simple return.
        CachedArrays.@spinlock CachedArrays.alloc_lock(manager) begin
            CachedArrays.evict!(_a, manager.policy, manager)
        end

        @test length(CachedArrays.visible_ids(manager, Local)) == 0
        @test length(CachedArrays.visible_ids(manager, Remote)) == 0

        @test CachedArrays.check(manager.remote_heap)
        @test CachedArrays.check(manager.local_heap)
        @test CachedArrays.check(manager)
    end

    # Case 2: Try the same movement corner case, but going the other direction.
    let
        _a = CachedArrays.CachedArray{Float32}(undef, manager, 10_000)

        # Move this block into the remote heap.
        # Make sure that it actually resides in the remote heap and that it doesn't
        # have any siblings in the local heap for some reason.
        CachedArrays.evict!(_a)
        @test CachedArrays.getpool(CachedArrays.metadata(_a)) == CachedArrays.Remote
        @test CachedArrays.getsibling(CachedArrays.metadata(_a)) === nothing

        ### Unsafe operation.
        # Run finalizers on the `Region` backing `_a`.
        # This will put it on the manager's freebuffer.
        # Don't use `_a` after this operation.
        finalize(_a.object)

        # When we manually invoke an eviction, the manager will discover during allocation
        # that this object is queued to be freed.
        #
        # As such, it will not even move the array and simple return.
        CachedArrays.prefetch!(_a)

        @test length(CachedArrays.visible_ids(manager, Local)) == 0
        @test length(CachedArrays.visible_ids(manager, Remote)) == 0

        @test CachedArrays.check(manager.remote_heap)
        @test CachedArrays.check(manager.local_heap)
        @test CachedArrays.check(manager)
    end
end
