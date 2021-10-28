#####
##### Annotation API
#####

function onobjects(f::F, x::AbstractArray{T}) where {F,T}
    isbitstype(T) || foreach(x -> onobjects(f, x), x)
    return nothing
end

onobjects(f::F, x::Union{NamedTuple,Tuple}) where {F} = foreach(x -> onobjects(f, x), x)
onobjects(f::F, x::CachedArray) where {F} = f(x.object)
onobjects(f::F, x::Object) where {F} = f(x)

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
    exprs = [:(onobjects(f, (x.$fieldname))) for fieldname in fieldnames(T)]
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

function noescape(manager::CacheManager, f::F, args::Vararg{Any,N}; kw...) where {F,N}
    # Record the allocation ID in the manager before and after the call to `f`.
    # We the use `findobjects` on the returned result to gather any `Objects` that
    # might be present, extracting the `id` for these objects.
    start = readid(manager)
    result = f(args...; kw...)
    objects = findobjects(result)
    saveids = map(x -> getid(metadata(x)), objects)
    stop = readid(manager) - 1

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

"""
    @noescape manager f(args...; kw...)

Inform the CacheManager `manager` that intermediate allocations made during the call
to `f` that are not present in the returned value are save to be reclaimed.

**TODO**: In the future, we should be able to provide additional hooks to record
potentially escaped allocations, in the case that one of the arguments is holding
onto an array that gets reassigned.
"""
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

