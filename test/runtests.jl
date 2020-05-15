using CachedArrays
using Test
using Random
using BenchmarkTools
using TimerOutputs

include("utils/findnexttree.jl")

include("memkind.jl")
include("allocators.jl")

include("memory/block.jl")
include("memory/freelist.jl")
include("memory/buddyheap.jl")
include("memory/compactheap.jl")

include("memcpy.jl")

include("policy/lru.jl")

include("manager.jl")
include("array/array.jl")
include("array/locked.jl")

