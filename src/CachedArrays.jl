module CachedArrays

export CachedArray, LockedCachedArray

# Dependencies
import DataStructures
import SIMD
#import ThreadPools
using MacroTools

# Control whether asserts are active
# Default to `true` for now because of development
const DEBUG = get(ENV, "JULIA_CACHEDARRAYS_DEBUG", true)
const THREADED_COPY = true

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

# Bootstrap Utilities
include("memkind.jl")
include("memcpy.jl")

# Cache eviction policies
include("policy/lru.jl")

# Object Pools
include("pool/pool.jl")

# Implementation of the arrays and cache manager
include("cache/cache.jl")

# Array Implementstions
include("array/array.jl")
include("array/locked.jl")

include("lib.jl")


# Global manager for the set of CachedArrays.
# It's important to keep this concretely typed.
const ManagerType = CacheManager{LRUCache{UInt},SimplePool{MemKindAllocator},SimplePool{AlignedAllocator}}
const GlobalManager = Ref{ManagerType}()

function __init__()
    # Create the global manager.
    path = get(ENV, "JULIA_PMEM_PATH", @__DIR__)
    if path == @__DIR__
        @warn """
            Please define the environment variable "JULIA_PMEM_PATH" to point to
            the location where the PMM file should be located.

            Otherwise, the file will be created in $(@__DIR__) which is probably not what
            you want to do, but is fine for testing.
        """
    end
    GlobalManager[] = CacheManager{LRUCache{UInt}}(path)
end


end # module
