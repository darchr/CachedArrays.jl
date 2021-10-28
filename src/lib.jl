#####
##### Wrapper Types
#####

const RECONSTRUCTING_KEYWORDS = [:readable, :writable, :release]
const FORWARDING_KEYWORDS =
    [:prefetch!, :softevict!, :evict!, :unsafe_free, :_unsafe_track!, :_unsafe_untrack!]
const SINGLE_FORWARDING_KEYWORDS = [:manager]

const KEYWORDS =
    vcat(RECONSTRUCTING_KEYWORDS, FORWARDING_KEYWORDS, SINGLE_FORWARDING_KEYWORDS)

for keyword in vcat(RECONSTRUCTING_KEYWORDS, FORWARDING_KEYWORDS)
    @eval $keyword(x::AbstractArray; kw...) = nothing
    @eval $keyword(x::CachedArray; kw...) = $keyword(Cacheable(), x; kw...)
    @eval $keyword(x, y, z...; kw...) = foreach(a -> $keyword(a; kw...), (x, y, z...))
end

children(x) = ()
children(x::Union{Tuple,AbstractArray}) = x

# Helper functions for when we need to recurse using "Base.invoke"
maybesuper(::CachedArray{T,N}) where {T,N} = DenseArray{T,N}
maybesuper(::T) where {T} = T

# Helper function for dealing with wrapper types.
# The wrapper type's `maybesuper` method gets defined by the `@wrapper` macro below.
_maybesuper(x::T) where {T} = T
_maybesuper(x::T, ::Any, y...) where {T} = _maybesuper(x, y...)
_maybesuper(x::T, ::CachedArray, y...) where {T} = supertype(T)

makecall(f::Symbol) = esc(:(CachedArrays.$f))

# TODO: Make no-op in case of no required state change.
macro wrapper(typ, fields...)
    typ = esc(typ)
    if isa(fields[1], Expr)
        expr = fields[1]
        fields = fields[2:end]

        # Validate applicable methods
        if expr.head != :tuple
            error("If applying a subset of methods, please provide list as a tuple.")
        end

        fns = expr.args
        @assert all(in(KEYWORDS), fns)
    else
        fns = KEYWORDS
    end

    # Construct a builder for our generated expressions.
    fields = map(QuoteNode, fields)
    function builder(f)
        call = makecall(f)
        if in(f, RECONSTRUCTING_KEYWORDS)
            # Reconstruction for State Changes
            accessors = [:($call(getproperty(x, $field))) for field in fields]
            nt = :(NamedTuple{($(fields...),)}(($(accessors...),)))
            return quote
                function $call(x::$typ)
                    return ConstructionBase.setproperties(x, $nt)
                end
            end
        elseif in(f, FORWARDING_KEYWORDS)
            # Forwarding methods for policy hints
            forwards = [:($call(getproperty(x, $field); kw...)) for field in fields]
            return quote
                function $call(x::$typ; kw...)
                    $(forwards...)
                    return nothing
                end
            end
        elseif in(f, SINGLE_FORWARDING_KEYWORDS)
            forward = :($call(getproperty(x, $(first(fields)))))
            return quote
                function $call(x::$typ; kw...)
                    return $forward
                end
            end
        else
            error("Unknown Keyword: $f")
        end
    end

    overloads = builder.(fns)

    # Define "maybesuper" for this type as well.
    accessors = [:(getproperty(x, $field)) for field in fields]
    _f = makecall(:maybesuper)
    maybesuper = quote
        $_f(x::$typ) = _maybesuper(x, $(accessors...))
    end

    # Define accessor method for children array fields
    _f = makecall(:children)
    children = quote
        $_f(x::$typ) = ($(accessors...),)
    end

    return quote
        $(overloads...)
        $maybesuper
        $children
        # $_expand
    end
end

@wrapper LinearAlgebra.Transpose parent
@wrapper Base.SubArray parent
@wrapper Base.ReshapedArray parent

#####
##### @annotate
#####

# Macro for defining our extensions.
macro annotate(fn)
    return annotate_impl(fn)
end

function maybe_process_call(sym::Symbol)
    symstring = String(sym)
    # Our special keywords begin and end with "__"
    if startswith(symstring, "__") && endswith(symstring, "__")
        # Grab the chunk sandwiched between the "__" and see if it's a registered keyword.
        substr = Symbol(symstring[3:(end - 2)])
        if in(substr, KEYWORDS)
            return substr
        end
    end
    return sym
end

function annotate_impl(fn)
    # Get the split version of the function.
    def, oldargs = prepare_function(fn)

    function process_item(x)
        # We need to escape all arguments in the function signature in order to allow
        # type annotations to be scoped correctly.
        #
        # As a result, every time we see one of the original argument symbols in the
        # body of the function, we must escape it so that internal uses match the
        # modified function signature.
        if isa(x, Symbol) && in(x, oldargs)
            return esc(x)
        end

        # Next, we handle inner function calls by looking for our magic keyhwords.
        # If we find one of out magic keyword functions, replace it with a call to
        # the corresponding `CachedArrays` function.
        #
        # If we come across the magic `__invoke__` keyword, then we must replace the
        # callsite with the correct `Base.invoke` call.
        if MacroTools.@capture(x, f_(args__))
            # Only interested in our keywords, which will always be symbols in the
            # processed code (hopefully).
            if isa(f, Symbol) && f == :__invoke__
                # Deal with keywords.
                # Keywords get stored at the start of a function call as a "parameters"
                # expression.
                #
                # Don't need to do this for the other function calls because they
                # already get treated correctly.
                if !isempty(args) && isa(args[1], Expr) && args[1].head == :parameters
                    # Kind of a hack - remove the "parameters" head so we can splat
                    # out the keywords in the correct position.
                    kwargs = args[1].args
                    popfirst!(args)
                else
                    kwargs = []
                end
                return quote
                    tup = ($(args...),)
                    $(Base.invoke)(
                        $(def[:name]),
                        Tuple{map(maybesuper, tup)...},
                        tup...;
                        $(kwargs...),
                    )
                end
            end
        end

        # Deal with magic keywords
        if isa(x, Symbol)
            if x == :__recurse__
                return def[:name]
            else
                return maybe_process_call(x)
            end
        end

        # Default - no modification.
        return x
    end

    # Process the body of the function, converting keywords into CachedArray calls
    # and splicing in the created "invoke" statement above.
    def[:body] = MacroTools.postwalk(process_item, def[:body])
    return MacroTools.combinedef(def)
end

argname(x) = first(MacroTools.splitarg(x))
function prepare_function(expr::Expr)
    # Make the function definition complete for MacroTool's sake.
    def = MacroTools.splitdef(expr)

    # Record the symbols of the arguments to ensure we escape the symbols where necessary
    # in the body of the macro.
    #
    # Also, capture the function name as well since it could belong to a callable struct.
    oldargs = []
    for arg in def[:args]
        push!(oldargs, argname(arg))
    end
    push!(oldargs, argname(def[:name]))
    for arg in def[:kwargs]
        push!(oldargs, argname(arg))
    end

    # Get scoping of function definition correct.
    def[:name] = esc(def[:name])
    def[:args] = esc.(def[:args])
    def[:whereparams] = esc.(def[:whereparams])
    def[:kwargs] = esc.(def[:kwargs])

    return def, oldargs
end

#####
##### For analyzing arbitrary structs.
#####

onobjects(f::F, x::AbstractArray{<:AbstractArray}) where {F} =
    foreach(x -> onobjects(f, x), x)
onobjects(f::F, x::Union{NamedTuple,Tuple}) where {F} = foreach(x -> onobjects(f, x), x)
onobjects(f::F, x::CachedArray) where {F} = f(x.object)

@generated function onobjects(f::F, x::T) where {F,T}
    (isbitstype(T) || iszero(fieldcount(T))) && return :()
    exprs = [:(onobjects(f, (x.$fieldname))) for fieldname in fieldnames(T)]
    return quote
        $(exprs...)
    end
end

macro blockobjects(type)
    return :(onobjects(f::F, _::$(esc(type))) where {F} = nothing)
end

#####
##### Recursively gather all objects
#####

function findhelper(::Type{T}) where {T}
    q = :()
    tags = Symbol[]
    findobjects!(q, tags, :x, T)
    return q, tags
end

function findobjects!(q::Expr, tags, sym, ::Type{T}) where {T}
    gf = GlobalRef(Core, :getfield)
    for f in Base.OneTo(fieldcount(T))
        TF = fieldtype(T, f)
        skip =
            !Base.isconcretetype(TF) ||
            Base.isbitstype(TF) ||
            Base.isprimitivetype(TF) ||
            iszero(fieldcount(TF))
        skip && continue

        gfcall = Expr(:call, gf, sym, f)
        newsym = gensym(sym)
        if TF <: Object
            push!(q.args, Expr(:(=), newsym, gfcall))
            push!(tags, newsym)
        else
            newtags = []
            newq = :()
            findobjects!(newq, newtags, newsym, TF)
            if !isempty(newtags)
                push!(q.args, Expr(:(=), newsym, gfcall))
                append!(q.args, newq.args)
                append!(tags, newtags)
            end
        end
    end
end

@generated function slurp(x::T) where {T}
    body = Expr(:block)
    tags = Symbol[]
    findobjects!(body, tags, :x, T)
    tuple = :(($(tags...),))
    push!(body.args, tuple)
    return body
end

#####
##### No Escape
#####

function noescape(manager::CacheManager, f::F, args::Vararg{Any,N}; kw...) where {F,N}
    start = readid(manager)
    result = f(args...; kw...)
    objects = slurp(result)
    saveids = map(x -> getid(metadata(x)), objects)
    stop = readid(manager) - 1

    @spinlock alloc_lock(manager) begin
        backedges = manager.map.dict
        for id = start:stop
            if !in(id, saveids)
                backedge = get(backedges, id, nothing)
                backedge === nothing && continue

                # Back edge exists - free the result
                unsafe_free(manager, backedge)
            end
        end
    end
    return result
end

macro noescape(manager, fn)
    fn.head == :call || error("Can only handle function calls!")
    args = fn.args
    if args[2] isa Expr && args[2].head == :parameters
        kw = args[2].args
        deleteat!(args, 2)
    else
        kw = Any[]
    end
    args = map(esc, args)
    kw = map(esc, kw)
    return :(noescape($(esc(manager)), $(args...); $(kw...)))
end
