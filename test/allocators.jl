@testset "Testing Allocators" begin
    verify_alignment(p) = @test iszero(mod(convert(UInt, p), 64))

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
