abstract type AbstractHeap end

include("block.jl")
include("freelist.jl")

# Heap implementations
include("buddyheap.jl")
