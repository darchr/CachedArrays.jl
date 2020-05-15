@testset "CompactHeap" begin
    allocator = CachedArrays.AlignedAllocator()
    @testset "Scoping Block" begin
        #len = 100_000_000
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
            CachedArarys.free(heap, ptr)
            @test CachedArrays.check(heap)
        end
    end
end
