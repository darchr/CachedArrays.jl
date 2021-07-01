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

macro requires(locks...)
    lock_exprs = [:(Base.assert_havelock($(esc(lock)))) for lock in locks]

    return quote
        if DEBUG
            $(lock_exprs...)
        end
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
##### GC Callbacks
#####

# We can use GC Extensions to register a callback that will helpfully inform us
# when the garbage collector runs.
struct PostGCCallback
    flag::Threads.Atomic{Int}
end

PostGCCallback() = PostGCCallback(Threads.Atomic{Int}(0))

setflag(x::PostGCCallback) = Threads.atomic_xchg!(x.flag, 1)
clearflag(x::PostGCCallback) = Threads.atomic_xchg!(x.flag, 0)

function (cb::PostGCCallback)(_::Cint)
    setflag(cb)
    return nothing
end

"""
    create_post_gc_callback() -> (PostGCCallback, CFunction)

Create and register a Garbage Collector callback that will run after the garbage collector
completes. The callback will set the atomic integer stored in `PostGCCallback` when
invoked.

Be sure to hold onto the returned `CFunction` as well, since the callback will be removed
when the `CFunction` is garbage collected.
"""
function create_post_gc_callback()
    cb = PostGCCallback()
    cfunc = @cfunction($_cb, Cvoid, (Cint,))
    @ccall jl_gc_set_post_gc(cfunc::Ptr{Cvoid}, 1::Cint)::Cvoid
    finalizer(cfunc) do _cfunc
        @ccall jl_gc_set_post_gc(_cfunc::Ptr{Cvoid}, 0::Cint)::Cvoid
    end
    return cb, cfunc
end

