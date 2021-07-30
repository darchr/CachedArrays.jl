@testset "Testing LRU Cache" begin
    # Test `Priority`
    x = CachedArrays.Priority(0, 10)
    y = CachedArrays.Priority(1, 20)
    z = CachedArrays.Priority(0, 30)

    @test (CachedArrays.unwrap).((x, y, z)) == (10, 20, 30)
    @test x < y
    @test z < y
    @test !(z < x)
    @test !(x < z)
    @test z == x
    @test z != y
    @test hash(z) == hash(x)
    @test hash(z) != hash(y)

    # Test `LRU`
    lru = CachedArrays.LRU{Int}()
    @test isempty(lru)

    # LRU is a minheap, so should always return the item with the lowest priority.
    push!(lru, 1, 10)
    push!(lru, 2, 20)
    @test length(lru) == 2
    @test pop!(lru) == 1
    @test length(lru) == 1
    @test first(lru) == 2
    push!(lru, 3, 10)
    @test first(lru) == 3
    @test length(lru) == 2

    # Now, update "3" to have a higher priority.
    CachedArrays.update!(lru, 3, 30)
    @test length(lru) == 2
    @test first(lru) == 2
    @test pop!(lru) == 2
    @test pop!(lru) == 3
    @test isempty(lru)

    # Finally, test that deletion works.
    push!(lru, 3, 30)
    push!(lru, 1, 10)
    push!(lru, 2, 20)
    @test length(lru) == 3
    delete!(lru, 2)
    @test length(lru) == 2
    @test pop!(lru) == 1
    @test pop!(lru) == 3
    @test isempty(lru)
end
