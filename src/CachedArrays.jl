module CachedArrays

export CachedArray, LockedCachedArray, AbstractCachedArray

# stdlib
import Dates

# Dependencies
import DataStructures
import SIMD
import MacroTools

# Control whether asserts are active
# Default to `true` for now because of development
const DEBUG = get(ENV, "JULIA_CACHEDARRAYS_DEBUG", true)

# Check ALL array updates for correctness.
const PEDANTIC = false      # TODO: Currently Broken
const THREADED_COPY = true

# Flag to indicate if we're in 2LM.
# If we are, configure the system to error if we ever try to allocate remote memory.
_boolparse(x::Bool) = x
_boolparse(x::String) = parse(Bool, x)
const IS_2LM = _boolparse(get(ENV, "JULIA_IS_2LM", false))
#const IS_2LM = true

# If we're not in DEBUG mode, the @check macro will become a nop.
# Otherwise, it will simply forward to `@assert`.
@static if DEBUG
    macro check(ex...)
        return :(@assert($(esc.(ex)...)))
    end
else
    macro check(ex...)
        return :()
    end
end

# This turns out to be surpritingly useful ...
donothing(x...) = nothing

# Bootstrap Utilities
include("memkind.jl")
include("allocators.jl")
include("metadata.jl")
include("loadstore.jl")

# Heap implementations
include("memory/memory.jl")

# Cache eviction policies
include("policy/policy.jl")

# Implementation of the arrays and cache manager
include("manager.jl")

# Array Implementstions
include("array/cachedarray.jl")
include("array/locked.jl")

# Fast "memcpy"
include("memcpy.jl")
include("lib.jl")

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
    isnothing(i) && return 0

    # Remove the managers from this list and then run a full garbage collection to make sure
    # they're well can completely gone.
    deleteat!(GlobalManagers, i)
    GC.gc(true)

    # Return the number of managers we removed
    return length(i)
end

end # module

