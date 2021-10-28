struct TestStruct{A,B}
    a::A
    b::B
    c
end

struct TestBlocked{T}
    x::T
end
@blockobjects TestBlocked

@testset "Testing Annotation Utilities" begin
    manager = CachedArrays.CacheManager(
        CachedArrays.AlignedAllocator(),
        CachedArrays.AlignedAllocator();
        localsize = 100_000_000,
        remotesize = 100_000_000,
        minallocation = 10,
    )

    x = CachedArray(rand(Float32, 10), manager)
    y = similar(x)
    xo = x.object
    yo = y.object

    # Test "findobjects"
    @test findobjects(x) === (xo,)
    @inferred findobjects(x)
    @test findobjects((1,x,2,y)) == (xo, yo)
    @inferred findobjects((1,x,2,y))

    v = (a = 1, c = (TestStruct(5, x, 1), "hello"), d = y)
    @test findobjects(v) == (xo, yo)
    @inferred findobjects(v)

    # For now - check that uninferred fields are skipped.
    # Probably need to revisit this at some point though ...
    v = TestStruct(5, x, y)
    @test findobjects(v) == (xo,)

    # Test "onobjects"
    function collector(x)
        objects = []
        onobjects(x) do o
            push!(objects, o)
        end
        return objects
    end

    @test collector(x) == [xo]
    @test collector([x,y]) == [xo, yo]
    @test collector(TestStruct(x, x, x)) == [xo, xo, xo]
    @test collector(TestStruct(x, x, 10)) == [xo, xo]
    @test collector(TestStruct(1, 2, TestStruct(5, x, y))) == [xo, yo]
    @test collector((a = 1, b = TestBlocked(x), c = TestStruct(y, 0, x))) == [yo, xo]
end
