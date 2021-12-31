######
##### Default Definitions
#####

const CachedTypes = Union{Object,CachedArray}

function setdirty!(x::CachedTypes, flag = true)
    meta = metadata(x)
    setdirty!(manager(x), meta, flag)
    setdirty!(meta, flag)
    return nothing
end

update!(x::CachedTypes) = update!(manager(x), metadata(x))
isdirty(x::CachedTypes) = isdirty(metadata(x))
id(x::CachedTypes) = getid(metadata(x))
pool(x::CachedTypes) = getpool(metadata(x))

function prefetch!(x::CachedTypes; kw...)
    _manager = manager(x)
    @spinlock alloc_lock(_manager) begin
        block = metadata(x)
        # Possibility that `maybe_cleanup!` cleans up this block.
        # Presumeably, this can't happen since we have `x` and `x` is holding onto the
        # object that owns this block ... so it should never have been garbage collected
        # in the first place.
        if maybe_cleanup!(_manager, getid(block)) == false
            prefetch!(block, getpolicy(_manager), _manager; kw...)
        end
    end
end

function evict!(x::CachedTypes; kw...)
    _manager = manager(x)
    @spinlock alloc_lock(_manager) begin
        maybe_cleanup!(_manager)
        evict!(x, getpolicy(_manager), _manager)
    end
    return nothing
end

function softevict!(x::CachedTypes)
    _manager = manager(x)
    @spinlock alloc_lock(_manager) softevict!(getpolicy(_manager), _manager, metadata(x))
end

#####
##### Annotation API
#####

function onobjects(f::F, x::Array{T}) where {F,T}
    isbitstype(T) || foreach(x -> onobjects(f, x), x)
    return nothing
end

onobjects(f::F, x::Object) where {F} = isnull(unsafe_pointer(x)) || f(x)
onobjects(f::F, x::CachedArray) where {F} = onobjects(f, x.object)
onobjects(f::F, x::Union{NamedTuple,Tuple}) where {F} = foreach(x -> onobjects(f, x), x)

"""
    onobjects(f, x)

Call function `f` on all [`CachedArrays.Objects`] that are reachable from `x`.
If `f` is a composite type, recursively calls `onobjects` on each member of `x`.
If `f` is some iterable like an `AbstractArrays` or `Tuple`, then `onobjects` is called on
each member of that array.

Behavior can be customized by extending `onobjects` for your custom types.
If you would like to opt out your type from `onobjects` altogether, the convenience macro
[`@blockobjects`] may be used.
"""
@generated function onobjects(f::F, x::T) where {F,T}
    (isbitstype(T) || iszero(fieldcount(T))) && return :()
    exprs = map(fieldnames(T)) do fieldname
        return :(onobjects(f, Core.getfield(x, $(QuoteNode(fieldname)))))
    end
    return quote
        $(exprs...)
    end
end

"""
    @blockobjects MyType

Restrict [`onobjects`](@ref) from recursing into the fields of `MyType`.
"""
macro blockobjects(type)
    return :(onobjects(f::F, _::$(esc(type))) where {F} = nothing)
end

#####
##### Recursively gather all objects
#####

function findobjects_builder!(q::Expr, tags::Vector{Any}, sym::Symbol, ::Type{T}) where {T}
    gf = GlobalRef(Core, :getfield)
    for f in Base.OneTo(fieldcount(T))
        TF = fieldtype(T, f)
        # Note: Objects are mutable structs.
        # Therefore, no `isbitstype` can contain a reference to an Object, allowing us
        # to skip all these types.
        skip = !Base.isconcretetype(TF) || Base.isbitstype(TF) || iszero(fieldcount(TF))
        skip && continue
        gfcall = Expr(:call, gf, sym, f)
        newsym = gensym(sym)
        # If this field type is an object, we're done.
        # Emit the expression to make the appropriate assignment to `newsym` and
        # record `newsym` as one of the symbols to use to build the final tuple.
        if TF <: Object
            push!(q.args, Expr(:(=), newsym, gfcall))
            push!(tags, newsym)
        else
            # Otherwise, there's the possiblity that THIS member is also a composite
            # struct that might contain an Object, so we need to recurse.
            # However, we don't want to needlessly bloat our generated function.
            #
            # SO, only append the sub-expressions if at least one Object is found during
            # this recursive call (i.e., `newtags` is not empty)
            newtags = []
            newq = :()
            findobjects_builder!(newq, newtags, newsym, TF)
            if !isempty(newtags)
                push!(q.args, Expr(:(=), newsym, gfcall))
                append!(q.args, newq.args)
                append!(tags, newtags)
            end
        end
    end
end

function findobjects_builder(::Type{T}) where {T}
    q = :()
    tags = []
    findobjects_builder!(q, tags, :x, T)
    return q, tags
end

"""
    findobjects(x) -> (Objects...,)
    findobjects_builder!(q::Expr, tags::Vector{Symbol}, sym::Symbol, ::Type{T})

Recursive compile-time function builder expression.
The goal of this function is to assist in building an expression that:

1. Builds up `getfield` calls on nested structs.
2. Records the LHS symbols of the `getfield` forest that correspond to `CachedArrays.Object`s
3. Removes misc. and unnecessary `getfield` calls to reduce the work required for the
    downstream compiler.

It achieves by reflecting on the type `T` and selectively expanding `q` and `tags`.

** WARNING **: A current limitations are:

    1. All recursively nested types should be concrete. If they aren't, they will be
       skipped, which may or may not be what you want.
    2. Arrays will not be recursed over.
"""
@generated function findobjects(x::T) where {T}
    body = Expr(:block)
    tags = []
    findobjects_builder!(body, tags, :x, T)
    tuple = :(($(tags...),))
    push!(body.args, tuple)
    return body
end

#####
##### noescape
#####

function noescape(
    manager::CacheManager, ::Val{Large}, f::F, args::Vararg{Any,N}; kw...
) where {Large,F,N}
    # Record the allocation ID in the manager before and after the call to `f`.
    # We the use `findobjects` on the returned result to gather any `Objects` that
    # might be present, extracting the `id` for these objects.
    start = readid(manager)
    result = f(args...; kw...)
    if Large
        saveids = Set{UInt}()
        onobjects(result) do o
            isnull(unsafe_pointer(o)) || push!(saveids, getid(metadata(o)))
        end
    else
        objects = findobjects(result)
        saveids = map(objects) do o
            # If an allocation happens to be for zero bytes, then CachedArrays
            # will return a null pointer.
            #
            # Thus, we need to perform a null check here in order to avoid a segfault in
            # these corner cases.
            return isnull(unsafe_pointer(o)) ? zero(UInt) : getid(metadata(o))
        end
    end
    stop = readid(manager)

    # For all allocations that were made between `start` and `stop`, perform
    # `unsafe_free` on those objects (if not present in the returned objects).
    @spinlock alloc_lock(manager) begin
        backedges = manager.map.dict
        for id in Iterators.filter(!in(saveids), start:stop)
            backedge = get(backedges, id, nothing)
            backedge === nothing && continue

            # Back edge exists - free the result
            unsafe_free(manager, backedge)
        end
    end
    return result
end

# So we can handle calls to our generated "__invoke__", the top level expression needs to
# either:
#
# 1. Just be a straight up function call.
# 2. Contain a call to `Base.invoke`.
#
# In the case of the latter, we only modify the outermost invoke.
function find_invoke(manager, annotations, expr::Expr)
    # Use a `Ref` for `success` to make the linter happy.
    processed_invoke = Ref(false)
    processed_tuple = Ref(false)
    newexpr = MacroTools.prewalk(expr) do subexpr
        processed_invoke[] && return subexpr
        if isa(subexpr, Expr) && subexpr.head == :tuple && processed_tuple[] == false
            processed_tuple[] = true
            return esc(subexpr)
        elseif isa(subexpr, Expr) && subexpr.head == :call && subexpr.args[1] == invoke
            args, kw = extract_kw(subexpr.args[2:end], 1)

            # Perform the proper nested escaping because dealing with nested macros
            # is SUPER annoying.
            #
            # In all honesty, maybe we should teach `@annotate` how to correctly deal with
            # calls to `@noescape` to simplify the whole implementation.
            args[1] = esc(args[1])
            kw = map(esc, kw)

            processed_invoke[] = true
            return :(noescape($manager, $(annotations...), invoke, $(args...); $(kw...)))
        end
        return subexpr
    end
    if processed_invoke[] == false || processed_tuple[] == false
        error("Could not find a suitable subexpression!")
    end
    return newexpr
end

function extract_kw(args, pos = 2)
    if args[pos] isa Expr && args[pos].head == :parameters
        kw = args[pos].args
        deleteat!(args, pos)
    else
        kw = Any[]
    end
    return args, kw
end

function noescape_impl(manager, params, fn)
    large = params[:large]
    annotations = (Val(large),)
    manager = esc(manager)
    # Simple case
    if fn.head == :call
        args, kw = extract_kw(fn.args)
        args = map(esc, args)
        kw = map(esc, kw)
        return :(noescape($manager, $(annotations...), $(args...); $(kw...)))
    else
        # More complicated case.
        return find_invoke(manager, annotations, fn)
    end
end

default_noescape_kw() = Dict(:large => false)

"""
    @noescape manager [args=false] [large=false] f(args...; kw...)

Inform the CacheManager `manager` that intermediate allocations made during the call
to `f` that are not present in the returned value are save to be reclaimed.

A trailing `tuple` argument `others` can provide a further list of types to inspect in order
to keep otherwise escaping types from being freed.
"""
macro noescape(manager, fn)
    return noescape_impl(manager, default_noescape_kw(), fn)
end

macro noescape(manager, args...)
    fn = last(args)

    # Default keyword arguments
    kw = default_noescape_kw()
    kw_type_map = Dict(k => typeof(v) for (k, v) in kw)
    for i in Base.OneTo(lastindex(args) - 1)
        arg = args[i]
        if arg.head != :(=)
            error("Expected keyword arguments to be in the form `a=b`!")
        end

        # Match implemented keywords
        name = arg.args[1]
        value = arg.args[2]
        expected_type = get(kw_type_map, name, nothing)
        expected_type === nothing && error("Unknown keyword argument $(name)!")
        if !isa(value, expected_type)
            msg = """
            Expected keyword argument $name to have type $expected_type.
            Instead, it has type $(typeof(value))!
            """
            error(msg)
        end
        kw[name] = value
    end
    # Process keywords
    return noescape_impl(manager, kw, fn)
end

