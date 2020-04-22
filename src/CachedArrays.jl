module CachedArrays

export CachedArray, LockedCachedArray, AbstractCachedArray

# stdlib
import Dates

# Dependencies
import DataStructures
import SIMD
using MacroTools

# Control whether asserts are active
# Default to `true` for now because of development
const DEBUG = get(ENV, "JULIA_CACHEDARRAYS_DEBUG", true)
const THREADED_COPY = true

# Flag to indicate if we're in 2LM.
# If we are, configure the system to error if we ever try to allocate remote memory.
_boolparse(x::Bool) = x
_boolparse(x::String) = parse(Bool, x)
const IS_2LM = _boolparse(get(ENV, "JULIA_IS_2LM", false))

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

include("memory/block.jl")
include("memory/heap.jl")

# Cache eviction policies
include("policy/lru.jl")

# Implementation of the arrays and cache manager
include("manager.jl")

# Array Implementstions
include("array/cachedarray.jl")
include("array/locked.jl")

# Fast "memcpy"
include("memcpy.jl")
include("lib.jl")

# Global manager for the set of CachedArrays.
# It's important to keep this concretely typed.
const ManagerType = CacheManager{LRU{UInt},BuddyHeap{MemKindAllocator},BuddyHeap{AlignedAllocator}}
const GlobalManager = Ref{ManagerType}()


function __init__()
    # Create the global manager.
    path = get(ENV, "JULIA_PMEM_PATH", @__DIR__)
    if (path == @__DIR__) && !IS_2LM
        @warn """
            Please define the environment variable "JULIA_PMEM_PATH" to point to
            the location where the PMM file should be located.

            Otherwise, the file will be created in $(@__DIR__) which is probably not what
            you want to do, but is fine for testing.
        """
    end
    GlobalManager[] = CacheManager{LRU{UInt}}(path)
end


end # module
