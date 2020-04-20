# Generic block of memory
struct PoolBlock
    ptr::Ptr{Nothing}
    sz::Int
end

Base.pointer(block::PoolBlock) = block.ptr
Base.sizeof(block::PoolBlock) =  block.sz

# In general, parameterize object pools by allocators, which are ultimately responsible
# for providing memory.
include("allocators.jl")

abstract type AbstractPool{T <: AbstractAllocator} end

# Default fallback
allocator(pool::AbstractPool) = pool.allocator

function __alloc(pool::AbstractPool, sz)
    ptr = allocate(allocator(pool), sz)
    block = PoolBlock(ptr, sz)
    return block
end

__free(pool::AbstractPool, block) = __free(pool, pointer(block))
__free(pool::AbstractPool, ptr::Ptr) = __free(pool, convert(Ptr{Nothing}, ptr))
__free(pool::AbstractPool, ptr::Ptr{Nothing}) = free(allocator(pool), ptr)

include("simple.jl")
#include("binned.jl")

