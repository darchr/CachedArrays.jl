@testset "Testing Utils" begin
    @testset "Testing Always" begin
        f = CachedArrays.Always(10)
        @test f() == 10
        @test f(1) == 10
        @test f(1, "hello") == 10
        @test f(; g = 5) == 10
        @test f(1; h = "hello") == 10

        @test CachedArrays.donothing() === nothing
        @test CachedArrays.alwaysfalse() == false
    end

    # @spinlock
    @testset "Testing @spinlock" begin
        a = Threads.SpinLock()
        b = Threads.SpinLock()
        c = Threads.SpinLock()

        # Test that locks are acquired.
        @test islocked(a) == false
        @test islocked(b) == false
        @test islocked(c) == false
        CachedArrays.@spinlock a b c begin
            @test islocked(a) == true
            @test islocked(b) == true
            @test islocked(c) == true
        end
        @test islocked(a) == false
        @test islocked(b) == false
        @test islocked(c) == false

        # Throw an error and make sure we recover
        try
            CachedArrays.@spinlock a b c begin
                @test islocked(a) == true
                @test islocked(b) == true
                @test islocked(c) == true
                throw(AssertionError())
            end
        catch e
            !isa(e, AssertionError) && rethrow(e)
        end
        @test islocked(a) == false
        @test islocked(b) == false
        @test islocked(c) == false

        # nofail versions
        CachedArrays.@spinlock_nofail a b c begin
            @test islocked(a) == true
            @test islocked(b) == true
            @test islocked(c) == true
        end
        @test islocked(a) == false
        @test islocked(b) == false
        @test islocked(c) == false

        try
            CachedArrays.@spinlock_nofail a b c begin
                @test islocked(a) == true
                @test islocked(b) == true
                @test islocked(c) == true
                throw(AssertionError())
            end
        catch e
            !isa(e, AssertionError) && rethrow(e)
        end
        @test islocked(a) == true
        @test islocked(b) == true
        @test islocked(c) == true
    end

    @testset "Testing @checknothing" begin
        # This is the "nothing" case
        x = 100
        v = CachedArrays.@checknothing CachedArrays.donothing() (x = 5)
        @test v === nothing
        @test x == 5

        # This is the "not nothing" case
        x = 100
        v = CachedArrays.@checknothing 10 (x = 5)
        @test v === 10
        @test x == 100
    end

    @testset "Testing findT" begin
        x = Int(1)
        y = Float32(10)
        findT = CachedArrays.findT
        @test findT(Int, (x, y)) == 1
        @test findT(Int, (y, x)) == 1
        @test findT(Float32, (x, y)) == 10
        @test findT(Float32, (y, x)) == 10
        @test findT(Float64, (y, x)) === nothing

        @test findT(Integer, 1) == 1
        @test findT(Integer, Float32(10)) === nothing
        @test findT(Integer, ("nothing", 1)) == 1
        @test findT(Int, Base.Broadcast.Broadcasted(+, (x, y))) == 1

        # Make sure this works on arrays
        x = [1,2,3]
        @test findT(Vector{Int}, x) == x
        @test findT(Vector{Int}, view(x, 1:2)) == x
    end

    @testset "Testing `atomic_ptr_xchg`" begin
        a = Ref(Ptr{Int}())
        @test Int(a[]) == 0

        CachedArrays.atomic_ptr_xchg!(Base.unsafe_convert(Ptr{Ptr{Int}}, a), Ptr{Int}(1))
        @test Int(a[]) == 1
    end
end
