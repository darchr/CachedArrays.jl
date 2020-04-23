# Macro for defining our extensions.
macro prefetch(expr)
    # Add a "=nothing" to the end of the expression to make it a valid function definition.
    # This makes using the MacroTools built in methods easier to deal with.
    prefetch_impl(:($expr = nothing))
end

maybesuper(::AbstractCachedArray{T,N}) where {T,N} = DenseArray{T,N}
maybesuper(::T) where {T} = T

# Define prefetching to do nothing for non-cached arrays.
maybeprefetch(x) = nothing
maybeprefetch(x::AbstractCachedArray) = prefetch!(x)
_esc(x) = :($(esc(x)))

function prefetch_impl(expr::Expr)
    def = MacroTools.splitdef(expr)
    names = first.(MacroTools.splitarg.(def[:args]))
    def[:name] = esc(def[:name])
    def[:args] = map(_esc, def[:args])

    prefetch = map(names) do arg
        :(maybeprefetch($(esc(arg))))
    end

    tupletype = map(names) do arg
        :(maybesuper($(esc(arg))))
    end

    # Here, we force julia to call the next most specific definition.
    # We just use direct overloading as a trick to get the prefetch calls in.
    def[:body] = quote
        $(prefetch...)
        return Base.invoke(
            $(def[:name]),
            Tuple{$(tupletype...)},
            $(esc.(names)...);
            $(def[:kwargs]...)
        )
    end

    return MacroTools.combinedef(def)
end

#####
##### Prefetch Definitions
#####

# Prefetch before *
@prefetch Base.:*(A::AbstractCachedArray, B::AbstractCachedArray)

