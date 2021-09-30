# Print your message with out worrying about task switching.
# Suitable for running from withing finalizers.
function safeprint(message; force = false)
    if (force || VERBOSE)
        ccall(
            :jl_safe_printf,
            Cvoid,
            (Cstring,),
            "$message\n",
        )
    end
    return nothing
end

# a safe way to acquire locks from finalizers, where we can't wait (which switches tasks)
# Copied from CUDA.jl and Julia's "locks-mt.jl"
macro spinlock(l...)
    expr = esc(l[end])
    locks = esc.(l[1:end-1])

    # Build expression from the inner most to the outermost to ensure locks are acquired
    # in the order given at the macro invocation.
    for lock in reverse(locks)
        v = gensym("lock")
        expr = quote
            $v = $lock
            while !trylock($v)
                ccall(:jl_cpu_pause, Cvoid, ())
                # Temporary solution before we have gc transition support in codegen.
                ccall(:jl_gc_safepoint, Cvoid, ())
                # we can't yield here
            end
            try
                $(expr)
            finally
                unlock($v)
            end
        end
    end
    return expr
end

macro spinlock_nofail(l...)
    expr = esc(l[end])
    locks = esc.(l[1:end-1])

    # Build expression from the inner most to the outermost to ensure locks are acquired
    # in the order given at the macro invocation.
    for lock in reverse(locks)
        v = gensym("lock")
        expr = quote
            $v = $lock
            while !trylock($v)
                ccall(:jl_cpu_pause, Cvoid, ())
                # Temporary solution before we have gc transition support in codegen.
                ccall(:jl_gc_safepoint, Cvoid, ())
                # we can't yield here
            end
            $(expr)
            unlock($v)
        end
    end
    return expr
end

macro requires(locks...)
    lock_exprs = [:(Base.assert_havelock($(esc(lock)))) for lock in locks]

    return quote
        if DEBUG
            $(lock_exprs...)
        end
    end
end

macro checknothing(expr, action = :(return nothing))
    return quote
        x = $(esc(expr))
        x === nothing && $(esc(action))
        x
    end
end

"""
    @return_if_exists expr

If the result of `expr` is not `nothing`, then return that result.
Otherwise, continue execution.
"""
macro return_if_exists(expr)
    return quote
        x = $(esc(expr))
        x === nothing || return x
        x
    end
end


# We need to search through the `bc` object for the first instance of T
# Once we find it, return it.
findT(::Type{T}, bc::Base.Broadcast.Broadcasted) where {T} = findT(T, bc.args)
findT(::Type{T}, x::Tuple) where {T} = findT(T, findT(T, first(x)), Base.tail(x))
findT(::Type{T}, x::U) where {T,U<:T} = x
findT(::Type{T}, x::SubArray{<:Any,<:Any,U}) where {T,U<:T} = findT(T, parent(x))
findT(::Type{T}, x) where {T} = nothing
findT(::Type{T}, ::Tuple{}) where {T} = nothing
findT(::Type{T}, x::U, rest) where {T,U<:T} = x
findT(::Type{T}, ::Any, rest) where {T} = findT(T, rest)

# We hit this case when there's Float32/Float64 confusion ...
findT(::Type{T}, x::Base.Broadcast.Extruded{U}) where {T,U<:T} = x.x

#####
##### ObjectCache
#####

struct ObjectCache{T,Args}
    cache::Vector{T}
    args::Args
end

ObjectCache{T}() where {T} = ObjectCache{T,Tuple{}}(Vector{T}(), ())

function Base.getindex(cache::ObjectCache{T}) where {T}
    isempty(cache.cache) || return pop!(cache.cache)
    return T(cache.args...)
end

return!(cache::ObjectCache{T}, x::T) where {T} = push!(cache.cache, x)

#####
##### Exceptions
#####

struct AllocationException <: Exception end

#####
##### Object
#####

# Not to be confused with the "objects" in the `ObjectCache` above.
mutable struct Object{T}
    ptr::Ptr{Nothing}
    manager::T

    # Inner constructor to ensure finalizers are attached>
    function Object(ptr::Ptr{Nothing}, manager::T) where {T}
        object = new{T}(ptr, manager)
        if !isnull(ptr)
            unsafe_register!(manager, object)
            finalizer(free, object)
        end
        return object
    end
end

Base.pointer(object::Object) = object.ptr
unsafe_pointer(object::Object) = object.ptr
blockpointer(object::Object) = pointer_from_objref(object)

free(object::Object) = free(manager(object), unsafe_pointer(object))
metastyle(::Object) = BlockMeta()
manager(object::Object) = object.manager

"""
$(TYPEDSIGNATURES)

Allocate `bytes` from `objects`'s manager.
If `id` is not given, it will be selected automatically.
"""
function alloc(
    object::Object,
    bytes::Integer,
    priority::AllocationPriority = PreferLocal,
    id = getid(object.manager),
)
    return alloc(manager(object), bytes, priority, id)
end

# #####
# ##### Backedges
# #####
#
# # Back edges from the manager to the GC managed object holding a block.
# # Kept as a raw Ptr{Ptr} to avoid GC cycles.
# const Backedge = Ptr{Ptr{Nothing}}
# unsafe_block(backedge::Backedge) = unsafe_block(unsafe_load(backedge))
#
# backedge(x::Backedge) = x
# backedge(x::Ptr) = convert(Backedge, x)
# backedge(x) = backedge(blockpointer(x))
#
# mutable struct BackedgeMap
#     dict::Dict{UInt,Backedge}
#     size::Int
# end
#
# BackedgeMap() = BackedgeMap(Dict{UInt,Backedge}(), 0)
#
# Base.getindex(map::BackedgeMap, id) = map.dict[id]
# function set!(map::BackedgeMap, backedge::Backedge, id, sz::Integer)
#     @check !in(id, map)
#     map.dict[id] = backedge
#     map.size += sz
#     return backedge
# end
#
# function Base.delete!(map::BackedgeMap, id::UInt, sz::Integer)
#     delete!(map.dict, id)
#     @check map.size >= sz
#     map.size -= sz
#     return nothing
# end
#
# getsize(map::BackedgeMap) = map.size
# Base.in(id, map::BackedgeMap) = haskey(map.dict, id)
# Base.haskey(map::BackedgeMap, id) = haskey(map.dict, id)
# Base.length(map::BackedgeMap) = length(map.dict)
# Base.isempty(map::BackedgeMap) = isempty(map.dict)
# Base.keys(map::BackedgeMap) = keys(map.dict)
#
# Base.iterate(map::BackedgeMap, s...) = iterate(map.dict, s...)

# #####
# ##### GC Callbacks
# #####
#
# # We can use GC Extensions to register a callback that will helpfully inform us
# # when the garbage collector runs.
# struct PostGCCallback
#     flag::Threads.Atomic{Int}
# end
#
# PostGCCallback() = PostGCCallback(Threads.Atomic{Int}(0))
#
# setflag(x::PostGCCallback) = Threads.atomic_xchg!(x.flag, 1)
# clearflag(x::PostGCCallback) = Threads.atomic_xchg!(x.flag, 0)
#
# function (cb::PostGCCallback)(_::Cint)
#     setflag(cb)
#     return nothing
# end
#
# """
#     create_post_gc_callback() -> (PostGCCallback, CFunction)
#
# Create and register a Garbage Collector callback that will run after the garbage collector
# completes. The callback will set the atomic integer stored in `PostGCCallback` when
# invoked.
#
# Be sure to hold onto the returned `CFunction` as well, since the callback will be removed
# when the `CFunction` is garbage collected.
# """
# function create_post_gc_callback()
#     cb = PostGCCallback()
#     cfunc = @cfunction($_cb, Cvoid, (Cint,))
#     @ccall jl_gc_set_post_gc(cfunc::Ptr{Cvoid}, 1::Cint)::Cvoid
#     finalizer(cfunc) do _cfunc
#         @ccall jl_gc_set_post_gc(_cfunc::Ptr{Cvoid}, 0::Cint)::Cvoid
#     end
#     return cb, cfunc
# end

"""
$(TYPEDSIGNATURES)

Atomically store `v` to the location pointed to by `ptr` and return the old values
stored at `ptr`.
"""
function atomic_ptr_xchg!(ptr::Ptr{Ptr{T}}, v::Ptr{T}) where {T}
    return Base.llvmcall("""
        %ptr = inttoptr i64 %0 to i64*
        %rv = atomicrmw xchg i64* %ptr, i64 %1 acq_rel
        ret i64 %rv
        """,
        Ptr{T},
        Tuple{Ptr{Ptr{T}},Ptr{T}},
        ptr,
        v,
    )
end

# Convenience Constructors
struct ToCached{M,S}
    manager::M
    status::S
    priority::CachedArrays.AllocationPriority
end

function tocached(m::M, s::S = CachedArrays.NotBusy(); priority = CachedArrays.ForceLocal) where {M,S}
    return ToCached{M,S}(m, s, priority)
end

function (f::ToCached)(::Type{T}, x...) where {T}
    return CachedArrays.CachedArray{T}(undef, f.manager, x; f.status, f.priority)
end

