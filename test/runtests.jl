using CachedArrays
using Test
using BenchmarkTools

include("policy/lru.jl")

include("memkind.jl")
include("cache.jl")
include("array.jl")

