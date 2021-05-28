# Lots of corner cases live in eviction.
# Particularly when early aborts are triggered.
# Here, we try to test the corner cases.
@testset "Testing Eviction Corner Cases" begin
    # Scenarios to try.
    # 1. Block right before the eviction is freed during callback.
    # 2. Block right after current eviction frontier is freed during callback.
    # 3. Abort called during forward eviction frontier expansion. Block is NOT orphaned.
    # 4. Abort called during forward eviction frontier expansion. Block IS orphaned.
    # 5. Abort called during backward eviction frontier expansion. Block is NOT orphaned.
    # 6. Abort called during backward eviction frontier expansion. Block IS orphaned.c

    #####
    ##### Case 1.
    #####

    let
        allocator = CachedArrays.AlignedAllocator()
        heap = CachedArrays.CompactHeap(allocator, 6 * 4096; minallocation = 12)

        A = CachedArrays.unsafe_block(CachedArrays.alloc(heap, 256, 10))
        B = CachedArrays.unsafe_block(CachedArrays.alloc(heap, 256, 0))
        C = CachedArrays.unsafe_block(CachedArrays.alloc(heap, 256, 20))

        @test CachedArrays.getid(A) == 10
        @test CachedArrays.getid(B) == 0
        @test CachedArrays.getid(C) == 20

        # Callback function will free "A".
        # This will occur while "B" is being evicted.
        cb = function(block)
            if CachedArrays.getid(block) == CachedArrays.getid(C)
                CachedArrays.free(heap, CachedArrays.datapointer(A))
            end
        end

        CachedArrays.evictfrom!(heap, B, 2 * 4096; cb = cb)
        @test CachedArrays.check(heap)
        @test length(heap) == 1
    end

    #####
    ##### Case 2.
    #####

    let
        allocator = CachedArrays.AlignedAllocator()
        heap = CachedArrays.CompactHeap(allocator, 6 * 4096; minallocation = 12)

        A = CachedArrays.unsafe_block(CachedArrays.alloc(heap, 256, 10))
        B = CachedArrays.unsafe_block(CachedArrays.alloc(heap, 256, 0))
        C = CachedArrays.unsafe_block(CachedArrays.alloc(heap, 256, 20))

        @test CachedArrays.getid(A) == 10
        @test CachedArrays.getid(B) == 0
        @test CachedArrays.getid(C) == 20

        # Callback function will free "C".
        # This will occur while "B" is being evicted.
        cb = function(block)
            if CachedArrays.getid(block) == CachedArrays.getid(B)
                CachedArrays.free(heap, CachedArrays.datapointer(C))
            end
        end

        CachedArrays.evictfrom!(heap, B, 2 * 4096; cb = cb)
        @test CachedArrays.check(heap)
        @test length(heap) == 2
        CachedArrays.free(heap, CachedArrays.datapointer(A))
        @test length(heap) == 1
    end

    #####
    ##### Case 3.
    #####

    let
        allocator = CachedArrays.AlignedAllocator()
        heap = CachedArrays.CompactHeap(allocator, 6 * 4096; minallocation = 12)

        A = CachedArrays.unsafe_block(CachedArrays.alloc(heap, 256, 10))
        B = CachedArrays.unsafe_block(CachedArrays.alloc(heap, 256, 0))
        C = CachedArrays.unsafe_block(CachedArrays.alloc(heap, 256, 20))
        D = CachedArrays.unsafe_block(CachedArrays.alloc(heap, 256, 30))

        @test CachedArrays.getid(A) == 10
        @test CachedArrays.getid(B) == 0
        @test CachedArrays.getid(C) == 20
        @test CachedArrays.getid(D) == 30

        # Callback function will free "C".
        # This will occur while "B" is being evicted.
        cb = function(block)
            return CachedArrays.getid(block) == CachedArrays.getid(C)
        end

        # Evict from B, abort at C.
        # C is still taken and was not freed during the eviction process.
        # Make sure C is still in the heap.
        CachedArrays.evictfrom!(heap, B, 2 * 4096; cb = cb)
        @test CachedArrays.isfree(A) == false
        @test CachedArrays.isfree(B) == true
        @test CachedArrays.isfree(C) == false
        @test CachedArrays.isfree(D) == false
        @test CachedArrays.check(heap)

        @test length(heap) == 5
    end

    #####
    ##### Case 4.
    #####

    let
        allocator = CachedArrays.AlignedAllocator()
        heap = CachedArrays.CompactHeap(allocator, 6 * 4096; minallocation = 12)

        A = CachedArrays.unsafe_block(CachedArrays.alloc(heap, 256, 10))
        B = CachedArrays.unsafe_block(CachedArrays.alloc(heap, 256, 0))
        C = CachedArrays.unsafe_block(CachedArrays.alloc(heap, 256, 20))
        D = CachedArrays.unsafe_block(CachedArrays.alloc(heap, 256, 30))

        @test CachedArrays.getid(A) == 10
        @test CachedArrays.getid(B) == 0
        @test CachedArrays.getid(C) == 20
        @test CachedArrays.getid(D) == 30

        # Callback function will free "C".
        # This will occur while "B" is being evicted.
        cb = function(block)
            if CachedArrays.getid(block) == 20
                CachedArrays.free(heap, CachedArrays.datapointer(C))
                @test C.orphaned == true
                return true
            end
            return false
        end

        # Evict from B, abort at C.
        # C is still freed while "B" is being evicted.
        # We then abort when trying to evict C.
        # Make sure that C gets reclaimed during the cleanup process.
        CachedArrays.evictfrom!(heap, B, 2 * 4096; cb = cb)
        @test CachedArrays.isfree(A) == false
        @test CachedArrays.isfree(B) == true
        # B was merged with C
        @test B.size == 2 * 4096
        @test CachedArrays.isfree(D) == false
        @test CachedArrays.check(heap)

        @test length(heap) == 4 # A, B, D, tail
    end

    #####
    ##### Case 6
    #####
end
