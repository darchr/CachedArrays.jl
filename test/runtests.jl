using CachedArrays
using Test
using Random
using BenchmarkTools

include("memory/buddyheap.jl")

include("memkind.jl")
include("memcpy.jl")

include("policy/lru.jl")

include("cache.jl")
include("array.jl")
include("locked.jl")

