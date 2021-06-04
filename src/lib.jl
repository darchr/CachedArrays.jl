# Macro for defining our extensions.
macro hint(fn, annotations = :())
    # Add a "=nothing" to the end of the expression to make it a valid function definition.
    # This makes using the MacroTools built in methods easier to deal with.
    hint_impl(fn, annotations)
end

maybesuper(::CachedArray{T,N}) where {T,N} = DenseArray{T,N}
maybesuper(::T) where {T} = T

function hint_impl(fn, _annotations)
    # Gather performance annotations.
    annotations = process_body(_annotations)

    # Get the split version of the
    def = prepare_function(fn)
    body = def[:body]

    # Get function names to create types for `Base.invoke`
    names = first.(MacroTools.splitarg.(def[:args]))
    tupletype = [:(maybesuper($(arg))) for arg in names]

    body = quote
        # Prefetch
        $(annotations.prefetches...)

        # Inner invoke
        val = $(esc(Base.invoke))(
            $(def[:name]),
            Tuple{$(tupletype...)},
            $(names...);
            $(def[:kwargs]...),
        )

        # Cleanup
        $(annotations.evicts...)
        return val
    end
    def[:body] = body

    return MacroTools.combinedef(def)
end

function prepare_function(expr::Expr)
    # Make the function definition complete for MacroTool's sake.
    def = MacroTools.splitdef(:($expr = nothing))
    def[:name] = esc(def[:name])
    return def
end

function process_body(body::Expr)
    # Clean up line numbers
    body = MacroTools.postwalk(MacroTools.rmlines, body)
    _isempty = (body == :())

    if !_isempty && body.head != :block
        error("Expression must be a block. Instead, it is $(body.head)")
    end

    prefetches = []
    evicts = []

    if !_isempty
        for line in body.args
            # Prefetch
            if MacroTools.@capture(line, prefetch(x_))
                push!(prefetches, line)
            elseif MacroTools.@capture(line, evict(x_))
                push!(evicts, line)
            else
                error("Unrecognized expression: $line")
            end
        end
    end

    return (; prefetches, evicts)
end

