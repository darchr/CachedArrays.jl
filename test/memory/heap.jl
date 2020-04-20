@testset "Testing Custom Heap Manager" begin
    # Test "Block"
    #
    # Make a 64 byte vector that our "block" will modify
    allocator = CachedArrays.AlignedAllocator()
    V = unsafe_wrap(
        Vector{UInt8},
        convert(Ptr{UInt8}, CachedArrays.allocate(allocator, 64)),
        64
    )
    finalizer(V) do x
        CachedArrays.free(allocator, convert(Ptr{Nothing}, pointer(x)))
    end
    V .= 0

    block = CachedArrays.Block(convert(Ptr{Nothing}, pointer(V)))

    @test pointer(block) == convert(Ptr{Nothing}, pointer(V))

    # Since we've started this from an array of all zeros, the size, next, and previous
    # blocks should all be zer.
    @test block.size == 0
    @show block.next
    @show block.previous
    @test CachedArrays.isnull(block.next)
    @test CachedArrays.isnull(block.previous)
    @test block.free == false

    # Try changing the size
    block.size = 2^20
    @test block.size == 2^20
    block.free = true

    @test block.free
    @test CachedArrays.isfree(block)
    block.free = false
    @test !block.free

    x = Ptr{UInt64}(rand(UInt64))
    b = CachedArrays.Block(x)
    block.next = b
    @test block.next == b
    @test pointer(block.next) == x

    # Start with a small heap for our experiments.
    len = 2^20
    heap = CachedArrays.Heap(allocator, len)

    # We should have 1 bin that is 2^20 bytes large.
    num_bins = length(heap.freelists)
    @test CachedArrays.binsize(num_bins) == len
    @test CachedArrays.check(heap)
    for i in 1:num_bins-1
        @test CachedArrays.freelist_length(heap, i) == 0
    end
    @test CachedArrays.freelist_length(heap, num_bins) == 1
    @test CachedArrays.slowlength(heap) == 1

    # Make sure that this block does not have a buddy
    block = first(heap)
    @test isnothing(CachedArrays.getbuddy(heap, block))

    # Now, split this bin
    a, b = CachedArrays.split!(heap, heap.freelists[num_bins])
    CachedArrays.push_freelist!(heap, b)
    CachedArrays.push_freelist!(heap, a)

    @test CachedArrays.getbuddy(heap, a) == b
    @test CachedArrays.getbuddy(heap, b) == a

    @test CachedArrays.freelist_length(heap, num_bins) == 0
    @test CachedArrays.freelist_length(heap, num_bins - 1) == 2
    @test CachedArrays.slowlength(heap) == 2

    # This should fail the buddy check since we have two buddy blocks that are both free.
    @test !CachedArrays.buddycheck(heap)

    # Test merging logic
    block = CachedArrays.pop_freelist!(heap, num_bins - 1)
    @test CachedArrays.freelist_length(heap, num_bins - 1) == 1
    @test CachedArrays.slowlength(heap) == 2

    CachedArrays.putback!(heap, block)
    @test CachedArrays.freelist_length(heap, num_bins - 1) == 0
    @test CachedArrays.freelist_length(heap, num_bins) == 1
    @test CachedArrays.slowlength(heap) == 1

    # Now, for something tricky, try to grab an item from the lowest bin.
    block = CachedArrays.pop_freelist!(heap, 1)
    @show CachedArrays.slowlength(heap)
    @test CachedArrays.slowlength(heap) == num_bins
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
    @test CachedArrays.slowlength(heap) == 1
    @test first(heap).size == heap.len
    @test CachedArrays.check(heap)

    # Try a pretty gnalry allocation test.
    len = 2^20
    heap = CachedArrays.Heap(allocator, len)
    @show CachedArrays.slowlength(heap)

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
    @test CachedArrays.slowlength(heap) == 1
end
