@testset "Testing Custom BuddyHeap Manager" begin
    # Start with a small heap for our experiments.
    allocator = CachedArrays.AlignedAllocator()
    len = 2^20
    heap = CachedArrays.BuddyHeap(allocator, len)

    # We should have 1 bin that is 2^20 bytes large.
    num_bins = length(heap.freelists)
    @test CachedArrays.binsize(num_bins) == len
    @test CachedArrays.check(heap)
    for i in 1:num_bins-1
        @test CachedArrays.freelist_length(heap, i) == 0
    end
    @test CachedArrays.freelist_length(heap, num_bins) == 1
    @test length(heap) == 1

    # Make sure that this block does not have a buddy
    block = first(heap)
    @test isnothing(CachedArrays.getbuddy(heap, block))

    # Now, split this bin
    a, b = CachedArrays.split!(heap, pop!(heap.freelists[num_bins]))
    CachedArrays.push_freelist!(heap, b)
    CachedArrays.push_freelist!(heap, a)

    @test CachedArrays.getbuddy(heap, a) == b
    @test CachedArrays.getbuddy(heap, b) == a

    @test CachedArrays.freelist_length(heap, num_bins) == 0
    @test CachedArrays.freelist_length(heap, num_bins - 1) == 2
    @test length(heap) == 2

    # This should fail the buddy check since we have two buddy blocks that are both free.
    @test !CachedArrays.buddycheck(heap)

    # Test merging logic
    block = CachedArrays.pop_freelist!(heap, num_bins - 1)
    @test CachedArrays.freelist_length(heap, num_bins - 1) == 1
    @test length(heap) == 2

    CachedArrays.putback!(heap, block)
    @test CachedArrays.freelist_length(heap, num_bins - 1) == 0
    @test CachedArrays.freelist_length(heap, num_bins) == 1
    @test length(heap) == 1

    # Now, for something tricky, try to grab an item from the lowest bin.
    block = CachedArrays.pop_freelist!(heap, 1)
    @show length(heap)
    @test length(heap) == num_bins
    for i in 1:num_bins - 1
        @test CachedArrays.freelist_length(heap, i) == 1
    end
    block.free = false

    # Run the large check
    @test CachedArrays.check(heap)

    # Make sure the check fails if we accidentally mark this block as `free`.
    block.free = true
    @test CachedArrays.countcheck(heap) == false

    # If we return this block back, we should end up with just a single slab again.
    CachedArrays.putback!(heap, block)
    @test length(heap) == 1
    @test first(heap).size == heap.len
    @test CachedArrays.check(heap)

    # Try a pretty gnalry allocation test.
    len = 2^20
    heap = CachedArrays.BuddyHeap(allocator, len)
    @show length(heap)

    numtests = 10000
    pointers = Set{Ptr{Nothing}}()
    Random.seed!(123)

    for test in 1:3
        @time for _ in 1:numtests
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
                ptr = CachedArrays.alloc(heap, sz)
                CachedArrays.zerocheck(heap)

                # Throw an error if it doesn't pass.
                # That way, if it actually doesn't pass, we don't get SPAMMED in the
                # test window.
                passed = CachedArrays.check(heap)
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
                CachedArrays.free(heap, ptr)
                passed = CachedArrays.check(heap)
                !passed && error()
            end
        end
    end

    # Free all pointers
    for ptr in pointers
        CachedArrays.free(heap, ptr)
    end
    @test CachedArrays.check(heap)
    @test length(heap) == 1
end

@testset "Testing Eviciton" begin
    # Create a pretty small heap.
    #
    #                   32768
    #        16384                 16384
    #   8192       8192       8192       8192
    # 4096 4096  4096 4096  4096 4096  4096 4096
    allocator = CachedArrays.AlignedAllocator()
    heap = CachedArrays.BuddyHeap(allocator, 4096 * 8)

    # Allocate a 4096 byte chunk and a 8192 byte chunk.
    function doallocation(heap)
        @test length(heap) == 1
        p0 = CachedArrays.alloc(heap, 3000, 0)
        p1 = CachedArrays.alloc(heap, 6000, 1)
        return (p0, p1)
    end

    p0, p1 = doallocation(heap)
    b0 = CachedArrays.Block(p0 - CachedArrays.headersize())
    b1 = CachedArrays.Block(p1 - CachedArrays.headersize())

    @test b0.id == 0
    @test b0.size == 4096
    @test b1.id == 1
    @test b1.size == 8192

    @test CachedArrays.canallocfrom(heap, b0, 3000)
    @test CachedArrays.canallocfrom(heap, b0, 6000)
    @test CachedArrays.canallocfrom(heap, b0, 10000)
    @test CachedArrays.canallocfrom(heap, b0, 20000)
    @test !CachedArrays.canallocfrom(heap, b0, 40000)

    @test CachedArrays.canallocfrom(heap, b1, 3000)
    @test CachedArrays.canallocfrom(heap, b1, 6000)
    @test CachedArrays.canallocfrom(heap, b1, 10000)
    @test CachedArrays.canallocfrom(heap, b1, 20000)
    @test !CachedArrays.canallocfrom(heap, b1, 40000)

    # Get the free buddy for b0
    b0_buddy = CachedArrays.Block(CachedArrays.address(b0) + 4096)
    @test CachedArrays.canallocfrom(heap, b0_buddy, 3000)
    @test CachedArrays.canallocfrom(heap, b0_buddy, 6000)
    @test CachedArrays.canallocfrom(heap, b0_buddy, 10000)
    @test CachedArrays.canallocfrom(heap, b0_buddy, 20000)
    @test !CachedArrays.canallocfrom(heap, b0_buddy, 40000)

    # Next step, try this eviction free schenanigans.
    @test CachedArrays.check(heap)
    ids = Int[]
    CachedArrays.evictfrom!(heap, b0, 6000; cb = x -> push!(ids, CachedArrays.getid(x)))
    @test CachedArrays.check(heap)
    # Should have evicted block zero
    @test ids == [0]

    # Pop the block from the free list - should be the block we just evicted.
    b = CachedArrays.alloc(heap, 6000)
    @test CachedArrays.check(heap)
    @test b == pointer(b0) + CachedArrays.headersize()
    CachedArrays.free(heap, b)
    @test CachedArrays.check(heap)

    # Okay, now put back start again.
    CachedArrays.free(heap, p1)
    @test CachedArrays.check(heap)
    p0, p1 = doallocation(heap)

    ids = Int[]
    b0 = CachedArrays.Block(p0 - CachedArrays.headersize())
    CachedArrays.evictfrom!(heap, b0, 10000; cb = x -> push!(ids, CachedArrays.getid(x)))

    # We've eviction should maintain the status of the cache.
    @test CachedArrays.check(heap)

    p2 = CachedArrays.alloc(heap, 10000, 3)
    @test CachedArrays.check(heap)
    @test ids == [0, 1]
    @test CachedArrays.check(heap)

    # Free everything again
    CachedArrays.free(heap, p2)
    @test CachedArrays.check(heap)
    p0, p1 = doallocation(heap)

    # Now, evict from p1
    ids = Int[]
    b1 = CachedArrays.Block(p1 - CachedArrays.headersize())
    CachedArrays.evictfrom!(heap, b1, 10000; cb = x -> push!(ids, CachedArrays.getid(x)))
    @test ids == [0, 1]
    p2 = CachedArrays.alloc(heap, 10000, 3)
    @test CachedArrays.check(heap)
    CachedArrays.free(heap, p2)
end
