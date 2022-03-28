# Dummy Module for testing macro expansion.
module TestModule
import CachedArrays
end

# The general idea here is that we manually perform macroexpansion on expressions passed
# to `CachedArrays.@annotate`. This will do all the the things like symbol and function
# resolution.
#
# We can then use the `@expected` macro to manually specify what we expect the macro
# to expand to for testing.
#
# This is a little brittle and requires a bit of care for the following reasons:
#
# 1. Gensyms need to be replaced in a deterministic manner. See `alias_gensyms` below.
macro annotatetest(expr)
    return :(
        alias_gensyms(MacroTools.prettify(
            macroexpand(TestModule, $(QuoteNode(expr)); recursive = true); alias = false
        ))
    )
end

macro expected(expr)
    expanded = QuoteNode(macroexpand(TestModule, expr; recursive = true))
    return quote
        alias_gensyms(MacroTools.prettify($expanded; alias = false))
    end
end

# Deterministically replace gensyms with things that are easier to read.
# Good for testing, not for general usage.
const gensyms = [:animal, :butterfly, :caracal, :dingo]

function alias_gensyms(expr)
    map = Dict{Symbol,Symbol}()
    return MacroTools.postwalk(expr) do ex
        if MacroTools.isgensym(ex)
            return get!(map, ex, gensyms[length(map) + 1])
        end
        return ex
    end
end

@testset "Testing Library Utilities" begin
    @testset "Testing @wrapper" begin
        manager = CachedArrays.CacheManager(
            CachedArrays.AlignedAllocator(),
            CachedArrays.AlignedAllocator();
            localsize = 1_000_000_000,
            remotesize = 1_000_000_000,
            minallocation = 12,
        )

        x = CachedArray{Float32}(undef, manager, 10, 20)
        @test size(x) == (10, 20)
        rx = CachedArrays.writable(x)
        rx .= zero(Float32)

        y = transpose(x)
        @test size(y) == (20, 10)

        # Cannot index because the wrapped array is locked.
        @test_throws ErrorException y[1]
        @test_throws ErrorException y[1] = 2

        # Convert to a readable form.
        # `getindex` works but `setindex!` should not.
        readable_y = CachedArrays.readable(y)
        @test all(iszero, readable_y)
        @test_throws ErrorException readable_y[1] = 10

        # Convert to a writable form.
        # Changes to the writable form should be visible in the original array.
        writable_y = CachedArrays.writable(y)
        @test all(iszero, writable_y)
        writable_y .+= 1
        @test all(isequal(1), writable_y)
        @test all(isequal(1), rx)

        # Make sure we can release wrapper types as well.
        released_y = CachedArrays.release(y)
        @test_throws ErrorException released_y[1]
        @test_throws ErrorException released_y[1] = 10

        # Now - does this work on nested structs.
        z = reshape(transpose(x), 5, :)
        @test_throws ErrorException z[1]
        @test_throws ErrorException z[1] = 10

        readable_z = CachedArrays.readable(z)
        @test all(isequal(1), readable_z)
        @test_throws ErrorException readable_z[1] = 10

        writable_z = CachedArrays.writable(z)
        @test all(isequal(1), writable_z)
        writable_z .+= 1
        @test all(isequal(2), writable_z)
        @test all(isequal(2), rx)

        released_z = CachedArrays.release(z)
        @test_throws ErrorException released_z[1]
        @test_throws ErrorException released_z[1] = 10

        # Nest in the opposite order
        w = transpose(reshape(x, 5, :))
        @test_throws ErrorException w[1]
        @test_throws ErrorException w[1] = 10

        readable_w = CachedArrays.readable(w)
        @test all(isequal(2), readable_w)
        @test_throws ErrorException readable_w[1] = 10

        writable_w = CachedArrays.writable(w)
        @test all(isequal(2), writable_w)
        writable_w .+= 1
        @test all(isequal(3), writable_w)
        @test all(isequal(3), rx)

        released_w = CachedArrays.release(w)
        @test_throws ErrorException released_w[1]
        @test_throws ErrorException released_w[1] = 10
    end

    @testset "Testing @annotate" begin
        # # Test "maybe_process_call" works on keywords and ignores nonkeywords.
        # for keyword in CachedArrays.KEYWORDS
        #     sym = Symbol("__$(keyword)__")
        #     @test CachedArrays.maybe_process_call(sym) == :(CachedArrays.$(Symbol(keyword)))
        # end

        # @test CachedArrays.maybe_process_call(:__invoke__) == :__invoke__
        # @test CachedArrays.maybe_process_call(:__recurse__) == :__recurse__
        # @test CachedArrays.maybe_process_call(:__notakeyword__) == :__notakeyword__
        # @test CachedArrays.maybe_process_call(:release) == :release

        #####
        ##### Test Cases
        #####

        # # Detect if MacroTools gets updated and add the `alias` keyword.
        # @test_throws MethodError MacroTools.prettify(:(myexpr); alias = false)

        ### Test 1
        fn = @annotatetest CachedArrays.@annotate function Mod.fn(
            x::MaybeTranspose{<:UnreadableCachedArray}, desc
        )
            return __recurse__(readable(x), desc)
        end
        ex = @expected function Mod.fn(x::MaybeTranspose{<:UnreadableCachedArray}, desc;)
            return Mod.fn(readable(x), desc)
        end
        @test fn == ex

        ### Test 2
        fn = @annotatetest CachedArrays.@annotate function Mod.another_fn(
            f, data::CachedArray
        )
            return __invoke__(f, writable(data))
        end

        # Test replacement order
        @test gensyms[1] == :animal
        ex = @expected function Mod.another_fn(f, data::CachedArray;)
            return begin
                animal = (f, CachedArrays.writable(data))
                invoke(
                    Mod.another_fn,
                    CachedArrays.Tuple{CachedArrays.maybesuper.(animal)...},
                    animal...;
                )
            end
        end

        ### Test 3
        fn = @annotatetest CachedArrays.@annotate function (dot::SomeModule.Callable)(
            x::UnreadableCachedArray, ys::ReadableCachedArray; kw...
        )
            return dot(readable(x), ys; kw...)
        end
        ex = @expected function (dot::SomeModule.Callable)(
            x::UnreadableCachedArray, ys::ReadableCachedArray; kw...
        )
            return dot(readable(x), ys; kw...)
        end
        @test fn == ex

        ### Test 4
        # Ensure that functions scoped outside the body of the macro are called correctly.
        fn = @annotatetest CachedArrays.@annotate function (dot::SomeModule.Callable)(
            x::UnreadableCachedArray
        )
            y = CachedArrays.@noescape manager(x) __recurse__(readable(x))
            CachedArrays.onobjects(dot) do o
                somefunction!(o)
            end
            return release(y)
        end

        ex = @expected function (dot::SomeModule.Callable)(x::UnreadableCachedArray;)
            y = CachedArrays.@noescape manager(x) (dot::SomeModule.Callable)(
                readable(x)
            )
            CachedArrays.onobjects(dot) do o
                somefunction!(o)
            end
            return release(y)
        end
        @test fn == ex

        ### Add corner cases as they are encountered.
    end
end
