using CachedArrays
using Test
using Random
using BenchmarkTools
using TimerOutputs
using Distributions
import ProgressMeter

include("utils/findnexttree.jl")

include("memkind.jl")
include("allocators.jl")

include("memory/block.jl")
include("memory/freelist.jl")
include("memory/compactheap.jl")

include("memcpy.jl")

include("policy/lru.jl")

include("manager.jl")
include("array/array.jl")

include("integration/corner.jl")
include("integration/stress.jl")

