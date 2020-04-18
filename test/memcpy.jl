@testset "Testing memcpy" begin
    # Test error checking first.
    x = Vector{Vector{Int}}()
    y = Vector{Vector{Int}}()

    @test_throws ArgumentError CachedArrays.memcpy!(x, y)

    # Source/Dest length mismatch
    x = rand(Float32, 1024)
    y = rand(Float32, 1025)
    @test_throws ArgumentError CachedArrays.memcpy!(x, y)

    # Test alignment
    popfirst!(y)
    #@test_throws ArgumentError CachedArrays.memcpy!(x, y)
    #@test_throws ArgumentError CachedArrays.memcpy!(y, x)

    # Now, see if the memcpy works correctly.
    x = rand(Float32, 1024 * 10)
    y = rand(Float32, 1024 * 10)
    @test x != y
    CachedArrays.memcpy!(x, y)
    @test x == y

    # Make this kind of a wierd number of bytes.
    x = rand(Float32, 1024 * 10 + 5)
    y = rand(Float32, 1024 * 10 + 5)
    @test x != y
    CachedArrays.memcpy!(x, y)
    @test x == y
end
