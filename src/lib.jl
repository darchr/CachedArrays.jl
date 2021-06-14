# Macro for defining our extensions.
macro hint(fn)
    return hint_impl(fn)
end

# TODO: More generic treatment of wrapper types.
maybesuper(::T) where {T <: LinearAlgebra.Transpose{<:Any,<:CachedArray}} = supertype(T)
maybesuper(::CachedArray{T,N}) where {T,N} = DenseArray{T,N}
maybesuper(::T) where {T} = T

function hint_impl(fn)
    # Get the split version of the function.
    def, oldargs = prepare_function(fn)

    function handle_invoke(x)
        if isa(x, Symbol)
            if in(x, oldargs)
                return :($(esc(x)))
            end
            return x
        end

        if MacroTools.@capture(x, f_(args__))
            if isa(f, Symbol) && f == :__invoke__
                return quote
                    names = ($(args...),)
                    $(Base.invoke)(
                        $(def[:name]),
                        Tuple{maybesuper.(names)...},
                        names...,
                    )
                end
            end
        end
        return x
    end

    # Process the body of the function, converting keywords into CachedArray calls
    # and splicing in the created "invoke" statement above.
    def[:body] = process_body(def[:body], handle_invoke)
    return MacroTools.combinedef(def)
end

function prepare_function(expr::Expr)
    # Make the function definition complete for MacroTool's sake.
    def = MacroTools.splitdef(expr)
    def[:name] = esc(def[:name])
    oldargs = first.(MacroTools.splitarg.(def[:args]))
    def[:args] .= esc.(def[:args])

    return def, oldargs
end

#####
##### Postwalk pipeline
#####

# If we see the magic "__invoke__" keyword, splice in the appropriate invocation of
# the base function we're actually trying to call.
process_statement(x, handle_invoke) = handle_invoke(x)
function process_statement(x::Expr, handle_invoke)
    # Maybe modify function calls if they are not scoped by a module prefix.
    if x.head == :call && isa(x.args[1], Symbol)
        x.args[1] = maybe_process_call(x.args[1])
    end
    return handle_invoke(x)
end

const KEYWORDS = [
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
        if in(substr, KEYWORDS)
            return :($(Symbol(substr)))
        end
    end
    return sym
end

function process_body(body::Expr, handle_invoke)
    # Clean up line numbers
    body = MacroTools.postwalk(MacroTools.rmlines, body)
    _isempty = (body == :())

    if !_isempty && body.head != :block
        error("Expression must be a block. Instead, it is $(body.head)")
    end

    if !_isempty
        body = MacroTools.postwalk(body) do expr
            return process_statement(expr, handle_invoke)
        end
    end

    return MacroTools.prettify(body)
end

