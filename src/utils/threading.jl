# a safe way to acquire locks from finalizers, where we can't wait (which switches tasks)
# Copied from CUDA.jl and Julia's "locks-mt.jl"
macro spinlock(l, ex)
    quote
        temp = $(esc(l))
        while !trylock(temp)
            ccall(:jl_cpu_pause, Cvoid, ())
            # Temporary solution before we have gc transition support in codegen.
            ccall(:jl_gc_safepoint, Cvoid, ())
            # we can't yield here
        end
        try
            $(esc(ex))
        finally
            unlock(temp)
        end
    end
end

macro requires(locks...)
    lock_exprs = [:(Base.assert_havelock($(esc(lock)))) for lock in locks]

    return quote
        if DEBUG
            $(lock_exprs...)
        end
    end
end
