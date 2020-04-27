@testset "Testing Allocators" begin
    #####
    ##### MemKind Allocator
    #####

    maxsize = 2^26
    kind = CachedArrays.MemKind.create_pmem(@__DIR__, maxsize)

    allocator = CachedArrays.MemKindAllocator(kind)

    verify_alignment(p) = @test iszero(mod(convert(UInt, p), 64))

    # Verify alignment
    for _ in 1:10000
        ptr = CachedArrays.allocate(allocator, rand(1:1_000_000))
        verify_alignment(ptr)
        CachedArrays.free(allocator, ptr)
    end

    #####
    ##### Aligned Allocator
    #####

    allocator = CachedArrays.AlignedAllocator()

    for _ in 1:10000
        ptr = CachedArrays.allocate(allocator, rand(1:1_000_000))
        verify_alignment(ptr)
        CachedArrays.free(allocator, ptr)
    end
end
