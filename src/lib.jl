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

# Process arguments - see if any want to be unlocked.
function process(args)
    processed = []
    annotations = []
    for arg in args
        p = MacroTools.postwalk(arg) do x
            # Strip off calls to 'unlock'
            if MacroTools.@capture(x, unlock(xs_))
                push!(annotations, :(unlock($xs)))
                return xs
            else
                return x
            end
        end
        push!(processed, p)
    end
    return processed, annotations
end

function prefetch_impl(expr::Expr)
    def = MacroTools.splitdef(expr)
    def[:name] = esc(def[:name])

    def[:args], annotations = process(def[:args])
    names = first.(MacroTools.splitarg.(def[:args]))

    prefetch = map(names) do arg
        :(maybeprefetch($(arg)))
    end

    tupletype = map(names) do arg
        :(maybesuper($(arg)))
    end

    # Here, we force julia to call the next most specific definition.
    # We just use direct overloading as a trick to get the prefetch calls in.
    def[:body] = quote
        $(prefetch...)
        $(annotations...)
        return $(esc(Base.invoke))(
            $(def[:name]),
            Tuple{$(tupletype...)},
            $(names...);
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

