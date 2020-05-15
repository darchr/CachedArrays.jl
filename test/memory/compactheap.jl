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

    @testset "Scoping Block" begin
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
end
