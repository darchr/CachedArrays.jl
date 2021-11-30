#####
##### Callback placeholders
#####

"""
    Always{T}(val)

Functor that allways returns `val` when called.
"""
struct Always{T}
    val::T
end
(f::Always)(x...; kw...) = f.val

const donothing = Always(nothing)
const alwaysfalse = Always(false)

#####
##### printing
#####

# Print your message with out worrying about task switching.
# Suitable for running from withing finalizers.
"""
    safeprint(message; [force])

Print `message` in a way that is safe to do inside finalizers (can't trigger a task switch).
If keyword `force = false`, than message will only printed if `VERBOSE = true`.
"""
function safeprint(message; force = false)
    if (force || VERBOSE)
        ccall(:jl_safe_printf, Cvoid, (Cstring,), "$message\n")
    end
    return nothing
end

#####
##### internal macros
#####

cpu_pause() = ccall(:jl_cpu_pause, Cvoid, ())
safepoint() = ccall(:jl_gc_safepoint, Cvoid, ())

function _spinlock_impl(expr, canfail::Bool, locks)
    # If the inner expression can fail, then we need to wrap inner calls in a try-finally
    # block. If it is stated that `expr` cannot fail, then we can get some slightly better
    # code generation by removing these try-finally blocks.
    function inner(x, lock)
        if canfail
            return quote
                try
                    $x
                finally
                    unlock($lock)
                end
            end
        else
            return quote
                $x
                unlock($lock)
            end
        end
    end
    expr = esc(expr)
    locks = map(esc, locks)

    # Build expression from the inner most to the outermost to ensure locks are acquired
    # in the order given at the macro invocation.
    for lock in reverse(locks)
        v = gensym("lock")
        expr = quote
            $v = $lock
            while !trylock($v)
                cpu_pause()
                safepoint()
            end
            $(inner(expr, v))
        end
    end
    return expr
end

# a safe way to acquire locks from finalizers, where we can't wait (which switches tasks)
# Copied from CUDA.jl and Julia's "locks-mt.jl"
"""
    @spinlock lock1 lock2 ... lockN expr

Acquire locks in order than execute `expr`.
Acquiring the locks will not trigger a task switch.
"""
macro spinlock(l...)
    return _spinlock_impl(l[end], true, l[1:end-1])
end

"""
    @spinlock_nofail lock1 lock2 ... lockN expr

Acquire `locks` in order than execute `expr`.
Acquiring the locks will not trigger a task switch.
If `expr` throws an error, locks will not be released.
"""
macro spinlock_nofail(l...)
    return _spinlock_impl(l[end], false, l[1:end-1])
end

"""
    @requires lock1 lock2 ...

Assert that all locks are held. Only valid in debug mode.
"""
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

#####
##### Broadcast Destructuring
#####

# We need to search through the `bc` object for the first instance of T
# Once we find it, return it.
findT(::Type{T}, bc::Base.Broadcast.Broadcasted) where {T} = findT(T, bc.args)
findT(::Type{T}, x::Tuple) where {T} = findT(T, findT(T, first(x)), Base.tail(x))
findT(::Type{T}, x::U) where {T,U<:T} = x
findT(::Type{T}, x::SubArray{<:Any,<:Any,U}) where {T<:AbstractArray,U<:T} = findT(T, parent(x))
findT(::Type{T}, x) where {T} = nothing
findT(::Type{T}, ::Tuple{}) where {T} = nothing
findT(::Type{T}, x::U, rest) where {T,U<:T} = x
findT(::Type{T}, ::Any, rest) where {T} = findT(T, rest)

#####
##### ObjectCache
#####

mutable struct ObjectCache{T,Args}
    cache::Vector{T}
    head::Int
    args::Args
end

ObjectCache{T}() where {T} = ObjectCache{T,Tuple{}}(Vector{T}(), 0, ())

function Base.getindex(cache::ObjectCache{T}) where {T}
    head = cache.head
    if head > 0
        object = @inbounds(cache.cache[head])
        cache.head = head - 1
        return object
    end
    return T(cache.args...)
end

function return!(cache::ObjectCache{T}, x::T) where {T}
    head = cache.head
    if head < length(cache.cache)
        head += 1
        cache.cache[head] = x
    else
        push!(cache.cache, x)
    end
    return nothing
end

#####
##### Exceptions
#####

struct AllocationException <: Exception end

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

#####
##### ToCached
#####

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
    return CachedArray{T}(undef, f.manager, x; f.status, f.priority)
end

