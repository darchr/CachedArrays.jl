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

include("benchmarks/copy.jl")

end # module
