# Utils
include("block.jl")
include("freelist.jl")

# A buddy-based heap allocator.
# All allocations supplied by this allocator are aligned to 64B
# We use a 64B header and footer to store metadata.
isnull(x::Ptr) = isnull(convert(UInt, x))
isnull(x::UInt) = iszero(x)

#####
##### Abtract Heap Methods
#####

abstract type AbstractHeap end

# Heap implementations
include("compactheap.jl")

# API
# basepointer - return the pointer to the base of the memory region managed by the heap
# sizeof - lenght of the memory region in bytes
# alloc(heap::AbstractHeap, bytes::Integer, id::UInt)
# free(heap::AbstractHeap, ptr::Ptr{Nothing})

# Evicting
# canallocfrom(heap::AbstractHeap, block::Block, sz)
# evictfrom!(heap::AbstractHeap, block::Block, sz; [cb = donothing])

alloc(heap::AbstractHeap, bytes::Integer) = alloc(heap, bytes, zero(UInt))
alloc(heap::AbstractHeap, bytes::Integer, id::Integer) = alloc(heap, bytes, UInt(id))

free(heap::AbstractHeap, ptr::Ptr) = free(heap::AbstractHeap, convert(Ptr{Nothing}, ptr))

endof(heap::AbstractHeap) = basepointer(heap) + sizeof(heap)

function basepointer end
baseblock(heap::AbstractHeap) = Block(basepointer(heap))

# Iteration
function Base.iterate(heap::AbstractHeap)
    block = baseblock(heap)
    return (block, block)
end

function Base.iterate(heap::AbstractHeap, block)
    if pointer(block) + block.size >= basepointer(heap) + sizeof(heap)
        return nothing
    end
    block = Block(pointer(block) + block.size)
    return (block, block)
end

function Base.length(heap::AbstractHeap)
    count = 0
    for _ in heap
        count += 1
    end
    return count
end

# AbstractHeaps should keep the `block.size` feature.
function walknext(heap::AbstractHeap, block::Block)
    ptr = pointer(block) + block.size
    if ptr >= endof(heap)
        return nothing
    else
        return Block(ptr)
    end
end


