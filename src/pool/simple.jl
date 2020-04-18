# Returning large object to libmemkind causes them to potentially be returned
# to the OS. This, in turn, results in them not being initialized when reallocated, which
# can really hurt performance.
#
# Instead, we follow a strategy like CuArrays.jl and keep excess blocks of memory around
# that have been previously freed.
#
# These are already initialized.
struct Block
    ptr::Ptr{Nothing}
    sz::Int
end

Base.pointer(block::Block) = block.ptr
Base.sizeof(block::Block) =  block.sz

#####
##### Simple Pool
#####

# Simple Pool
mutable struct SimplePool{T <: AbstractAllocator}
    allocator::T

    # Keep track of the blocks in use.
    allocated::Dict{Ptr{Nothing},Block}
    blocks_available::Set{Block}

    # Keep track of the number of times we've failed to deliver a block from our local
    # pool.
    # Use this to trigger GC.
    num_failures::Int
    num_failures_before_gc::Int
end

function SimplePool(allocator) where {M,F}
    return SimplePool(
        allocator,
        Dict{Ptr{Nothing},Block}(),
        Set{Block}(),
        0,
        # TODO: Tune this parameter
        10000,
    )
end

# Bottom level malloc and free
function actual_alloc(pool::SimplePool, sz)
    ptr = alloc(pool.allocator, sz)
    block = Block(ptr, sz)
    return block
end

function actual_free(pool::SimplePool, block::Block)
    free(pool.allocator, pointer(block))
    return nothing
end

# The maximum size a pooled block can be to fulfill a request.
maxoversize(sz) = sz + (sz >> 1)

function scan(pool::SimplePool, sz)
    for block in pool.blocks_available
        if sz <= sizeof(block) <= maxoversize(sz)
            delete!(pool.blocks_available, block)
            #println("Returning Requested Block")
            return block
        end
    end
    return nothing
end

# Send `sz` bytes back to the original allocator.
function reclaim(pool::SimplePool, sz::Int = typemax(Int))
    freedbytes = 0
    while freedbytes < sz && !isempty(pool.blocks_available)
        block = pop!(pool.blocks_available)
        freedbytes += sizeof(block)
        actual_free(pool, block)
    end
    return freedbytes
end

# Pool API
init(pool::SimplePool) = nothing

function _alloc(pool::SimplePool, sz)
    block = scan(pool, sz)
    isnothing(block) || return block

    # Note the failure.
    pool.num_failures += 1

    # Decide if we should trigger Garbage collection.
    if pool.num_failures >= pool.num_failures_before_gc
        # Trigger a full GC.
        GC.gc(true)
        pool.num_failures += 1
    end

    # Try to scan again.
    block = scan(pool, sz)
    isnothing(block) || return block

    # If we failed again, for now, just trigger a full reclamation.
    reclaim(pool)
    return actual_alloc(pool, sz)
end

_free(pool::SimplePool, block) = push!(pool.blocks_available, block)

function alloc(pool::SimplePool, sz)
    # Try to allocate a block from the local pool
    block = _alloc(pool, sz)
    if !isnothing(block)
        ptr = pointer(block)
        pool.allocated[ptr] = block
        return ptr
    end
    error("Out of Memory??")
end

free(pool::SimplePool, ptr::Ptr) = free(pool, convert(Ptr{Nothing}, ptr))
function free(pool::SimplePool, ptr::Ptr{Nothing})
    block = pool.allocated[ptr]
    delete!(pool.allocated, ptr)
    _free(pool, block)
    return nothing
end
