@testset "CompactHeap" begin
    @testset "Eviction" begin
        # Make a small-granularity heap so we can wrap our heads around what's going on.
        # This heap will only have 4 blocks, each of size 4096.
        allocator = CachedArrays.AlignedAllocator()
        heap = CachedArrays.CompactHeap(allocator, 4 * 4096; minallocation = 12)
        @test sizeof(heap) == 4 * 4096

        # Get the base pointer for verification purposes.
        base = CachedArrays.basepointer(heap)

        # The first allocation should go into the bottom-most block
        ptrA = CachedArrays.alloc(heap, 10, 1)
        @test ptrA == base + CachedArrays.headersize()

        # Next allocation should go to the next block.
        ptrB = CachedArrays.alloc(heap, 4096 + 10, 2)
        @test ptrB == base + 4096 + CachedArrays.headersize()

        # Last allocation should take the the last available slot.
        ptrC = CachedArrays.alloc(heap, 10, 3)
        @test ptrC == base + 3 * 4096 + CachedArrays.headersize()

        # By now, the cache should be full.
        @test isnothing(CachedArrays.alloc(heap, 10, 4))

        # Now, when we evice ptrA, should get a CB for id 1
        ids = Int[]
        cb = x -> push!(ids, CachedArrays.getid(x))
        CachedArrays.evictfrom!(heap, ptrA, 10; cb = cb)
        @test ids == [1]
        @test CachedArrays.check(heap)

        # Okay, get this pointer back, lets try evicting something larger.
        ptrA = CachedArrays.alloc(heap, 10, 1)
        @test ptrA == base + CachedArrays.headersize()

        empty!(ids)
        CachedArrays.evictfrom!(heap, ptrA, 4096 + 10; cb = cb)
        @test ids == [1,2]

        # Repopulate - lets try to force allocation to go backwards.
        CachedArrays.free(heap, ptrC)

        #####
        ##### New set of tests for easier copy-paste debugging
        #####

        allocator = CachedArrays.AlignedAllocator()
        heap = CachedArrays.CompactHeap(allocator, 4 * 4096; minallocation = 12)
        base = CachedArrays.basepointer(heap)
        ids = Int[]
        cb = x -> push!(ids, CachedArrays.getid(x))

        ptrA = CachedArrays.alloc(heap, 10, 1)
        ptrB = CachedArrays.alloc(heap, 10, 2)
        ptrC = CachedArrays.alloc(heap, 10, 3)
        ptrD = CachedArrays.alloc(heap, 10, 4)

        @test ptrA == base + CachedArrays.headersize()
        @test ptrB == base + 4096 + CachedArrays.headersize()
        @test ptrC == base + 2 * 4096 + CachedArrays.headersize()
        @test ptrD == base + 3 * 4096 + CachedArrays.headersize()

        empty!(ids)
        CachedArrays.evictfrom!(heap, ptrC, 2 * 4096 + 10; cb = cb)
        @test ids == [3, 4, 2]

        ptrD = CachedArrays.alloc(heap, 2 * 4096 + 10)
        @test ptrD == base + 4096 + CachedArrays.headersize()
    end

    @testset "Scoping" begin
        #len = 100_000_000
        allocator = CachedArrays.AlignedAllocator()
        len = 2^27
        heap = CachedArrays.CompactHeap(allocator, len)
        numtests = 10000
        pointers = Set{Ptr{Nothing}}()
        Random.seed!(123)

        timer = TimerOutput()

        @timeit timer "Test Running" for test in 1:3
            for _ in 1:numtests
                # CHange ratios of allocations to frees.
                if test == 1
                    num_allocs = rand(1:20)
                elseif test == 2
                    num_allocs = rand(10:20)
                else
                    num_allocs = rand(1:10)
                end

                for _ in 1:num_allocs
                    # Make a random allocation.
                    sz = rand(0:len - 256)
                    @timeit timer "Allocating" ptr = CachedArrays.alloc(heap, sz)

                    # Throw an error if it doesn't pass.
                    # That way, if it actually doesn't pass, we don't get SPAMMED in the
                    # test window.
                    @timeit timer "Checking 1" passed = CachedArrays.check(heap)
                    !passed && error()

                    if !isnothing(ptr)
                        @test !in(ptr, pointers)
                        push!(pointers, ptr)
                    end
                end

                if test == 1
                    num_frees = rand(1:10)
                elseif test == 2
                    num_frees = rand(10:20)
                else
                    num_frees = rand(1:11)
                end

                for _ in 1:num_frees
                    isempty(pointers) && break
                    ptr = rand(pointers)
                    delete!(pointers, ptr)
                    @timeit timer "Freeing" CachedArrays.free(heap, ptr)
                    @timeit timer "Checking 2" passed = CachedArrays.check(heap)
                    !passed && error()
                end
            end
        end
        printstyled(stdout, "Timer Results for Compact Heap\n"; color = :cyan, bold = true)
        println(stdout, timer)

        for ptr in pointers
            CachedArrays.free(heap, ptr)
            @test CachedArrays.check(heap)
        end
    end

    @testset "Defragmentation" begin
        # The idea here is to get the heap into a known state with an expected
        # post-defragmentation result, then try defragmentation with an appropriate
        # callback and make sure the right things happen.
        #
        # First, we will do the easy case with no blocks queued for being returned
        # to the heap, then we'll test that case.
        minallocation = CachedArrays.PowerOfTwo(12)
        minallocation_bytes = 2 ^ (minallocation.val)
        allocation_size = 10 * 2 ^ (minallocation.val)

        heap = CachedArrays.CompactHeap(
            CachedArrays.AlignedAllocator(),
            allocation_size;
            minallocation = minallocation,
        )

        oneblock = (2 ^ (minallocation.val) - CachedArrays.headersize())
        twoblock = (2 * 2 ^ (minallocation.val) - CachedArrays.headersize())

        idmap = Dict{UInt64,CachedArrays.Block}()

        # The target state pre-defragmentation is shown below, where each block
        # in the upper region represents a region of size "minallocation"
        #
        # F = Free
        # A = Allocated
        #
        # +---+---+---+---+---+---+---+---+---+---+
        # |   | 1 |   | 2 |   |   | 3 | 4 | 5 |   | Heap Block
        # +---+---+---+---+---+---+---+---+---+---+
        # | F |   A1  | A2|   F   | A3| A4|   A5  | Logical Block
        # +---+-------+---+-------+---+---+-------+
        #         |     |               |     |
        #         |     +---------------+     |
        #         |          siblings         |
        #         +---------------------------+

        ptr_f1 = CachedArrays.alloc(heap, oneblock, UInt(0))
        ptr_a1 = CachedArrays.alloc(heap, twoblock, UInt(1))
        ptr_a2 = CachedArrays.alloc(heap, oneblock, UInt(2))

        ptr_f2 = CachedArrays.alloc(heap, twoblock, UInt(0))
        ptr_a3 = CachedArrays.alloc(heap, oneblock, UInt(3))
        ptr_a4 = CachedArrays.alloc(heap, oneblock, UInt(4))
        ptr_a5 = CachedArrays.alloc(heap, twoblock, UInt(5))

        basepointer = CachedArrays.basepointer(heap)
        headersize = CachedArrays.headersize()
        @test ptr_f1 == basepointer + headersize
        @test ptr_a1 == basepointer + minallocation_bytes + headersize
        @test ptr_a2 == basepointer + 3 * minallocation_bytes + headersize
        @test ptr_f2 == basepointer + 4 * minallocation_bytes + headersize
        @test ptr_a3 == basepointer + 6 * minallocation_bytes + headersize
        @test ptr_a4 == basepointer + 7 * minallocation_bytes + headersize
        @test ptr_a5 == basepointer + 8 * minallocation_bytes + headersize

        CachedArrays.free(heap, ptr_f1)
        CachedArrays.free(heap, ptr_f2)

        # Set siblings
        block_a1 = CachedArrays.unsafe_block(ptr_a1)
        block_a5 = CachedArrays.unsafe_block(ptr_a5)
        @test block_a1.id == 1
        @test block_a5.id == 5
        block_a1.sibling = block_a5
        block_a5.sibling = block_a1
        @test block_a1.id == 1
        @test block_a5.id == 5

        block_a2 = CachedArrays.unsafe_block(ptr_a2)
        block_a4 = CachedArrays.unsafe_block(ptr_a4)
        @test block_a2.id == 2
        @test block_a4.id == 4
        block_a2.sibling = block_a4
        block_a4.sibling = block_a2
        @test block_a2.id == 2
        @test block_a4.id == 4

        block_a3 = CachedArrays.unsafe_block(ptr_a3)
        @test block_a3.id == 3

        # Set block mapping
        for block in (block_a1, block_a2, block_a3, block_a4, block_a5)
            idmap[block.id] = block
        end

        # Time to defrag!
        count = 1
        CachedArrays.defrag!(heap) do id, newblock, _
            @test id == count
            count += 1

            @test haskey(idmap, id)
            idmap[id] = newblock
        end

        # Time to see how we did!
        offset = 0
        for i in 1:length(idmap)
            block = idmap[i]
            @test pointer(block) == CachedArrays.basepointer(heap) + offset
            offset += sizeof(block)
        end

        # Check siblings.
        @test idmap[1].sibling === idmap[5]
        @test idmap[5].sibling === idmap[1]

        @test idmap[2].sibling === idmap[4]
        @test idmap[4].sibling === idmap[2]

        # Number of blocks should be 6 - 5 allocated, one free at the end.
        @test length(heap) == 6
        @test CachedArrays.check(heap)
    end

    @testset "Defragmentation - Queueing" begin
        # The idea here is to get the heap into a known state with an expected
        # post-defragmentation result, then try defragmentation with an appropriate
        # callback and make sure the right things happen.
        #
        # First, we will do the easy case with no blocks queued for being returned
        # to the heap, then we'll test that case.
        minallocation = CachedArrays.PowerOfTwo(12)
        minallocation_bytes = 2 ^ (minallocation.val)
        allocation_size = 10 * 2 ^ (minallocation.val)

        heap = CachedArrays.CompactHeap(
            CachedArrays.AlignedAllocator(),
            allocation_size;
            minallocation = minallocation,
        )

        oneblock = (2 ^ (minallocation.val) - CachedArrays.headersize())
        twoblock = (2 * 2 ^ (minallocation.val) - CachedArrays.headersize())

        idmap = Dict{UInt64,CachedArrays.Block}()

        # The target state pre-defragmentation is shown below, where each block
        # in the upper region represents a region of size "minallocation"
        #
        # F = Free
        # A = Allocated
        #
        #               +- Marked As Queued. Cannot be moved.
        #               |
        #               v
        # +---+---+---+---+---+---+---+---+---+---+
        # |   | 1 |   | 2 |   |   | 3 | 4 | 5 |   | Heap Block
        # +---+---+---+---+---+---+---+---+---+---+
        # | F |   A1  | A2|   F   | A3| A4|   A5  | Logical Block
        # +---+-------+---+-------+---+---+-------+
        #         |     |               |     |
        #         |     +---------------+     |
        #         |          siblings         |
        #         +---------------------------+

        ptr_f1 = CachedArrays.alloc(heap, oneblock, UInt(0))
        ptr_a1 = CachedArrays.alloc(heap, twoblock, UInt(1))
        ptr_a2 = CachedArrays.alloc(heap, oneblock, UInt(2))

        ptr_f2 = CachedArrays.alloc(heap, twoblock, UInt(0))
        ptr_a3 = CachedArrays.alloc(heap, oneblock, UInt(3))
        ptr_a4 = CachedArrays.alloc(heap, oneblock, UInt(4))
        ptr_a5 = CachedArrays.alloc(heap, twoblock, UInt(5))

        basepointer = CachedArrays.basepointer(heap)
        headersize = CachedArrays.headersize()
        @test ptr_f1 == basepointer + headersize
        @test ptr_a1 == basepointer + minallocation_bytes + headersize
        @test ptr_a2 == basepointer + 3 * minallocation_bytes + headersize
        @test ptr_f2 == basepointer + 4 * minallocation_bytes + headersize
        @test ptr_a3 == basepointer + 6 * minallocation_bytes + headersize
        @test ptr_a4 == basepointer + 7 * minallocation_bytes + headersize
        @test ptr_a5 == basepointer + 8 * minallocation_bytes + headersize

        CachedArrays.free(heap, ptr_f1)
        CachedArrays.free(heap, ptr_f2)

        # Set siblings
        block_a1 = CachedArrays.unsafe_block(ptr_a1)
        block_a5 = CachedArrays.unsafe_block(ptr_a5)
        @test block_a1.id == 1
        @test block_a5.id == 5
        block_a1.sibling = block_a5
        block_a5.sibling = block_a1
        @test block_a1.id == 1
        @test block_a5.id == 5

        block_a2 = CachedArrays.unsafe_block(ptr_a2)
        block_a4 = CachedArrays.unsafe_block(ptr_a4)
        @test block_a2.id == 2
        @test block_a4.id == 4
        block_a2.sibling = block_a4
        block_a4.sibling = block_a2
        @test block_a2.id == 2
        @test block_a4.id == 4

        block_a3 = CachedArrays.unsafe_block(ptr_a3)
        @test block_a3.id == 3

        # Set block mapping
        for block in (block_a1, block_a2, block_a3, block_a4, block_a5)
            idmap[block.id] = block
        end

        # Time to defrag!
        block_a2.queued = true
        CachedArrays.defrag!(heap) do id, newblock, _
            @test haskey(idmap, id)
            idmap[id] = newblock
        end

        # Time to see how we did!
        offset = 0
        for i in 1:length(idmap)
            if i == 2
                offset += minallocation_bytes
            end

            block = idmap[i]
            @test pointer(block) == CachedArrays.basepointer(heap) + offset
            offset += sizeof(block)
        end

        # Check siblings.
        @test idmap[1].sibling === idmap[5]
        @test idmap[5].sibling === idmap[1]

        @test idmap[2].sibling === idmap[4]
        @test idmap[4].sibling === idmap[2]

        # Number of blocks should be 7
        # 1 allocated at the beginning.
        # 1 free after it.
        # 4 allocated.
        # 1 free for the remainder.
        @test length(heap) == 7
        @test CachedArrays.check(heap)
    end
end

@testset "Testing Eviction Corner Cases" begin
    # This tests the case where we have the following scenario:
    #
    #                 Evicted      Evicting
    #                    |             |
    #                    V             V
    # +------------+-------------+-----------+
    # |     A      |     B       |     C     |
    # +------------+-------------+-----------+
    #       ∧
    #       |
    # Freed during eviction callback
    #
    # In other words, block B is being evicted and during the eviction process, Block A
    # is freed.
    #
    # Even though block B may be next to block A in the heap, we want to prevent block A and
    # B from merging since that breaks the eviction logic.
    #
    # I.E. Eviction assumes that all blocks actively touched during the eviction process
    # do not belong to any freelist.

    # Small heap - enough for 6 allocations.
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
