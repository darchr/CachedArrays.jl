# Macro for defining our extensions.
macro decorate(expr)
    # Add a "=nothing" to the end of the expression to make it a valid function definition.
    # This makes using the MacroTools built in methods easier to deal with.
    _decorate(:($expr = nothing))
end

maybesuper(::AbstractCachedArray{T,N}) where {T,N} = DenseArray{T,N}
maybesuper(::T) where {T} = T

maybeprefetch(x) = nothing
maybeprefetch(x::AbstractCachedArray) = prefetch!(x)

getname(ex::Expr) = first(ex.args)
getname(ex::Symbol) = ex

function _decorate(expr::Expr)
    def = splitdef(expr)
    def[:args] = map(x -> :($(esc(x))), def[:args])
    names = getname.(def[:args])

    prefetch = map(names) do arg
        :(maybeprefetch($(esc(arg))))
    end

    tupletype = map(names) do arg
        :(maybesuper($(esc(arg))))
    end

    def[:body] = quote
        $(prefetch...)
        Base.invoke($(def[:name]), Tuple{$(tupletype...)}, $(esc.(names)...); $(def[:kwargs]...))
    end

    return MacroTools.combinedef(def)
end

#####
##### Prefetch Definitions
#####

# Prefetch before *
@decorate Base.:*(A::CachedArray, B::CachedArray)

