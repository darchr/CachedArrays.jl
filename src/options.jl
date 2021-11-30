#####
##### DEBUG
#####

# If we're not in DEBUG mode, the @check macro will become a nop.
# Otherwise, it will simply forward to `@assert`.
@static if DEBUG
    # Copy code from `error.jl` because calling a macro in a macro leads to kind of
    # ugly messages.
    macro check(ex, msgs...)
        msg = isempty(msgs) ? ex : msgs[1]
        if isa(msg, AbstractString)
            msg = msg
        elseif !isempty(msgs) && (isa(msg, Expr) || isa(msg, Symbol))
            msg = :(string($(esc(msg))))
        else
            msg = string(msg)
        end
        return :($(esc(ex)) ? $(nothing) : throw(AssertionError($msg)))
    end

    macro lock(ex...)
        return :(Base.@lock($(esc.(ex...))))
    end
else
    macro check(ex...)
        return :()
    end

    macro lock(ex...)
        return :(Base.@lock_nofail($(esc.(ex...))))
    end
end

#####
##### ENABLETIMING
#####

@static if ENABLETIMING
    const GLOBAL_TIMER = TimerOutputs.TimerOutput()
    macro timeit(label, expr)
        return :(TimerOutputs.@timeit $(esc(GLOBAL_TIMER)) $(esc(label)) $(esc(expr)))
    end

    # Timing Functions
    reset_timer!() = TimerOutputs.reset_timer!(GLOBAL_TIMER)
    gettimer() = GLOBAL_TIMER
else
    macro timeit(label, expr)
        return :($(esc(expr)))
    end

    reset_timer!() = nothing
    gettimer() = nothing
end

