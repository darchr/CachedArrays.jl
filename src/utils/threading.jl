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
