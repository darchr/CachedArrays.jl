@testset "FastHeap" begin
    allocator = CachedArrays.AlignedAllocator()
    @testset "Scoping Block" begin
        heap = CachedArays.FastHeap(allocator, 100_000_000)
    end
end
