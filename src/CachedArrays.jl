module CachedArrays

export CachedArray

include("memkind.jl")
include("cache.jl")
include("array.jl")

# Global manager for the set of CachedArrays
const GlobalManager = Ref{CacheManager{CachedArray}}()

function __init__()
    # Create the global manager.
    path = get(ENV, "JULIA_PMEM_PATH", @__DIR__)
    if isnothing(path)
        @warn """
            Please define the environment variable "JULIA_PMEM_PATH" to point to
            the location where the PMM file should be located.

            Otherwise, the file will be created in $(@__DIR__) which is probably not what
            you want to do, but is fine for testing.
        """
    end
    GlobalManager[] = CacheManager{CachedArray}(path)
end


end # module
