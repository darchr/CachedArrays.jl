__precompile__(false)
module MicroBench

# The main reason for this module's existence
using CachedArrays

# stdlib
using Dates
using LinearAlgebra
using Random
using Statistics

# "internal" dependencies
using MaxLFSR
using KernelBenchmarks  # for fast initialization of arrays.
using SystemSnoop
using CounterTools
using MattDaemon
using ExperimentsDB

# external deps
using DataStructures
using ProgressMeter
using StructArrays

# debug mode enables much smaller kernels that don't exceed the size of the DRAM
const DEBUG = false

# Will throw an error if this is not defined
const IS_2LM = parse(Bool, ENV["JULIA_IS_2LM"])
const IS_1LM = !IS_2LM

# More Prefetch definitions
CachedArrays.@prefetch LinearAlgebramul!(
        unlock(Y)::AbstractCachedArray,
        A::AbstractCachedArray,
        B::AbstractCachedArray
    )

include("counters.jl")
include("1d/top_1d.jl")
include("2d/top_2d.jl")

end # module
