using CachedArrays
using Test
using Random
using BenchmarkTools
using TimerOutputs
import MacroTools
using Distributions
import ProgressMeter

# Helper Functions
inlocal(manager, A) = in(CachedArrays.getid(CachedArrays.metadata(A)), manager.localmap)
inremote(manager, A) = in(CachedArrays.getid(CachedArrays.metadata(A)), manager.remotemap)

include("utils/findnexttree.jl")
include("allocators.jl")

include("memory/block.jl")
include("memory/freelist.jl")
include("memory/compactheap.jl")
include("memory/eviction.jl")
include("memcpy.jl")

include("policy/lru.jl")

include("manager.jl")
include("array/array.jl")
include("lib.jl")

include("integration/corner.jl")
include("integration/stress.jl")
