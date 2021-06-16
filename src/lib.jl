#####
##### Wrapper Types
#####

const STATE_CHANGES = [
    :readable,
    :writable,
    :release
]

# TODO: Make no-op in case of no required state change.
macro wrapper(typ, fields...)
    if isa(fields[1], Expr)
        expr = fields[1]
        fields = fields[2:end]

        # Validate applicable methods
        if expr.head != :tuple
            error("If applying a subset of methods, please provide list as a tuple.")
        end

        fns = expr.args
        @assert all(in(STATE_CHANGES), fns)
    else
        fns = STATE_CHANGES
    end

    # Construct a builder for our generated expressions.
    fns = [esc(:(CachedArrays.$f)) for f in fns]
    fields = map(QuoteNode, fields)
    function builder(f)
        accessors = [:($f(getproperty(x, $field))) for field in fields]
        nt = :(NamedTuple{($(fields...),)}(($(accessors...),)))
        return quote
            function $f(x::$(esc(typ)))
                return ConstructionBase.setproperties(x, $nt)
            end
        end
    end

    overloads = [builder(f) for f in fns]
    return quote
        $(overloads...)
    end
end

@wrapper LinearAlgebra.Transpose parent

#####
##### @annotate
#####

# Macro for defining our extensions.
macro annotate(fn)
    return annotate_impl(fn)
end

# TODO: More generic treatment of wrapper types.
maybesuper(::T) where {T <: LinearAlgebra.Transpose{<:Any,<:CachedArray}} = supertype(T)
maybesuper(::CachedArray{T,N}) where {T,N} = DenseArray{T,N}
maybesuper(::T) where {T} = T

const CACHEDARRAY_KEYWORDS = [
    "prefetch!",
    "evict!",
    "readable",
    "writable",
    "release",
]

function maybe_process_call(sym::Symbol)
    symstring = String(sym)
    # Our special keywords begin with
    if startswith(symstring, "__") && endswith(symstring, "__")
        # Grab the chunk sandwiched between the "__" and see if it's a registered keyword.
        substr = symstring[3:end-2]
        if in(substr, CACHEDARRAY_KEYWORDS)
            return :($(Symbol(substr)))
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
            if isa(f, Symbol)
                if f == :__invoke__
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
                            Tuple{maybesuper.(tup)...},
                            tup...;
                            $(kwargs...),
                        )
                    end
                elseif f == :__recurse__
                    return :($(def[:name])($(args...)))
                else
                    newf = maybe_process_call(f)
                    return :($newf($(args...)))
                end
            end
        end

        # Default behavior - no modification.
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

