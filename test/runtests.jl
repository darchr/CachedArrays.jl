using CachedArrays
using Test
using Random
using BenchmarkTools

include("memkind.jl")
include("memory/block.jl")
include("memory/buddyheap.jl")

include("memcpy.jl")

include("policy/lru.jl")

include("manager.jl")
include("array/array.jl")
include("array/locked.jl")

