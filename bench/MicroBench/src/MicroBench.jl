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

const ARRAYSIZE = parse(Int, replace(ENV["JULIA_MICROBENCH_ARRAYSIZE"], "_"=>""))

include("benchmarks/copy.jl")

end # module
