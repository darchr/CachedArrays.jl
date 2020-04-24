# Things to think about
#
# Allow arbitrary hooks for:
#     Usage Updates
#     Marking Dirty
#     Other hints
#
# How do we track state?
# Should the element types of the eviction policies be metadata?
getval(::Type{T}, x::T) where {T} = x

# Policy Hints
setdirty!(x, meta) = nothing
cheapevict(x, meta) = nothing

include("lru.jl")
include("random.jl")
