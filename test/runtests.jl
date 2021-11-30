using CachedArrays
using Test
using Random
using BenchmarkTools
using TimerOutputs
import MacroTools
using Distributions
import ProgressMeter

# Helper Functions
inlocal(manager, A; kw...) = inpool(manager, A, CachedArrays.Local; kw...)
inremote(manager, A; kw...) = inpool(manager, A, CachedArrays.Remote; kw...)

function inpool(manager, A, pool; primary_only = true)
    block = CachedArrays.metadata(A)
    id = CachedArrays.getid(block)
    return in(id, CachedArrays.visible_ids(manager, pool; primary_only))
end

include("utils/utils.jl")
include("utils/findnexttree.jl")
include("utils/lru.jl")
include("utils/freebuffer.jl")

include("allocators.jl")

include("memory/block.jl")
include("memory/freelist.jl")
include("memory/compactheap.jl")
include("memory/eviction.jl")
include("memcpy.jl")

include("manager.jl")
include("array/array.jl")
# include("lib.jl")
# include("annotation.jl")

include("integration/corner.jl")
include("integration/stress.jl")
