module CachedArrays

export CachedArray

# base
import Base: @lock

# stdlib
import Dates
import Random

# Dependencies
import ArrayInterface: ArrayInterface
import VectorizationBase
import DataStructures
import SIMD
import MacroTools
import TimerOutputs

# Control whether asserts are active
# Default to `true` for now because of development
const DEBUG = true
const VERBOSE = false
const ENABLETIMING = false

# Check ALL array updates for correctness.
# const PEDANTIC = false      # TODO: Currently Broken

# Flag to indicate if we're in 2LM.
# If we are, configure the system to error if we ever try to allocate remote memory.
_boolparse(x::Bool) = x
_boolparse(x::String) = parse(Bool, x)

const IS_2LM = false

# If we're not in DEBUG mode, the @check macro will become a nop.
# Otherwise, it will simply forward to `@assert`.
@static if DEBUG
    macro check(ex...)
        return :(@assert($(esc.(ex)...)))
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
##### Optional Timing
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

# This turns out to be surprisingly useful ...
donothing(x...) = nothing

# Low level timing macro
macro timens(name, expr)
    return quote
        before = time_ns()
        res = $(esc(expr))
        after = time_ns()
        println($name, ": ", Int(after - before), " ns")
        res
    end
end


#####
##### includes
#####

# Bootstrap Utilities
include("utils/findnexttree.jl")
include("utils/threading.jl")
include("utils/freebuffer.jl")

include("memkind.jl")
include("allocators.jl")
include("metadata.jl")

# Heap implementations
include("memory/memory.jl")

# Cache eviction policies
include("policy/policy.jl")

# Implementation of the arrays and cache manager
include("manager.jl")

# Array Implementstions
include("llvm.jl")
using .LoadStore: LoadStore

include("array.jl")

# Fast "memcpy"
include("memcpy.jl")
include("lib.jl")

#####
##### Keep track of Managers
#####

# When Managers get created - hold them in a global array to keep them from getting
# GC'd before the arrays they track.
#
# We can periodically GC the mangers to see if they are no longer holding onto anything,
# at which point we're free to reclaim their resources.
const GlobalManagers = CacheManager[]

function gc_managers()
    # Find all the managers that can be garbage collected.
    #
    # Trigger a full garbage collection before to clean up anything that may be holding
    # onto a manager
    GC.gc(true)

    # Now, find all the managers that have been completely cleaned up.
    i = findall(cangc, GlobalManagers)
    i === nothing && return 0

    # Remove the managers from this list and then run a full garbage collection to make sure
    # they're well can completely gone.
    deleteat!(GlobalManagers, i)
    GC.gc(true)

    # Return the number of managers we removed
    return length(i)
end

end # module
