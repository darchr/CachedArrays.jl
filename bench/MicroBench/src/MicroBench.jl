module MicroBench

# The main reason for this module's existence
using CachedArrays

# stdlib
using Random

# "internal" dependencies
using MaxLFSR
using KernelBenchmarks  # for fast initialization of arrays.

# external deps
using ProgressMeter

# debug mode enables much smaller kernels that don't exceed the size of the DRAM
const DEBUG = false

# Will throw an error if this is not defined
const IS_2LM = parse(Bool, ENV["JULIA_IS_2LM"])
const IS_1LM = !IS_2LM

const ARRAYSIZE = DEBUG ? 1_000_000_000 : parse(Int, replace(ENV["JULIA_MICROBENCH_ARRAYSIZE"], "_"=>""))
const TOTALSIZE = DEBUG ? 20_000_000_000 : parse(Int, replace(ENV["JULIA_MICROBENCH_TOTALSIZE"], "_"=>""))

include("1d/top_1d.jl")
include("2d/top_2d.jl")

# For now, only run this once at startup.
function setup()
    # if in 2LM mode, all of the temporary arrays should go into local memory
    if IS_2LM
        # Round up for headroom
        localsize = round(Int, TOTALSIZE * 1.1)
        remotesize = 4096
    # Size local mamory to be a little smaller than the DRAM cache to allow for other
    # program activities that don't belong to our heap.
    else
        localsize = DEBUG ? TOTALSIZE >> 1 : 180_000_000_000
        remotesize = DEBUG ? 2 * TOTALSIZE : 1_000_000_000_000
    end

    # Resize the CacheManager
    manager = CachedArrays.GlobalManager[]
    manager.gc_before_evict = false

    # Allocate 180 GB for local memory, using 2GiB for the maximum allocation.
    resize!(manager, localsize; maxallocation = 2 * ARRAYSIZE)

    # Allocate 1T for remote memory ... mostly just because we can haha.
    CachedArrays.resize_remote!(manager, remotesize; maxallocation = 2 * ARRAYSIZE)
end

end # module
