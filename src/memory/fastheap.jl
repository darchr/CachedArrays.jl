# Granularity of allocations.
const FASTHEAP_GRANULARITY = 2^24

# So we can access "binsize" before a `FastHeap` is instantiated.
struct FastHeapDispatch end

getbin(::FastHeapDispatch, sz) = div(sz - 1, FASTHEAP_GRANULARITY) + 1
binsize(::FastHeapDispatch, bin) = bin * FASTHEAP_GRANULARITY

function walknext(heap, block)
    ptr = pointer(block) + block.size
    if ptr >= basepointer(heap) + sizeof(heap)
        return nothing
    else
        return Block(ptr)
    end
end

function walkprevious(heap, block)
    # Invariant, the first block will always have a backsize of zero.
    backsize = block.backsize
    if iszero(backsize)
        return nothing
    else
        return Block(basepointer(heap) - backsize)
    end
end

# Allocation Strategy
#
# Look up one
mutable struct FastHeap{T} <: AbstractHeap
    allocator::T

    # Where is and what kind of memory do we have?
    base::Ptr{Nothing}
    len::Int
    pool::Pool

    # Maintaining freelist status
    status::MaskTree
    freelists::Dict{Int,Freelist{Block}}
end

function FastHeap(allocator::T, sz; pool = DRAM) where {T}
    # Round down size to the nearest multiple of `FASTHEAP_GRANULARITY`
    sz = FASTHEAP_GRANULARITY * div(sz, FASTHEAP_GRANULARITY)

    # Allocate the memory managed by this heap
    base = allocate(allocator, sz)
    numbins = getbin(FastHeapDispatch(), sz)
    status = MaskTree(numbins)
    freelists = Dict{Int,Freelist{Block}}()

    heap = FastHeap{T}(
        allocator,
        base,
        sz,
        pool,
        status,
        freelists
    )

    finalizer(heap) do x
        free(x.allocator, x.base)
    end

    # Add an entry for the biggest freelist.
    block = Block(base)
    block.size = sz
    block.backsize = 0
    block.free = true
    block.next = Block()
    block.previous = Block()

    pushfreelist!(heap, block)
    return heap
end

getbin(::FastHeap, sz) = getbin(FastHeapDispatch(), sz)
getbin(heap::FastHeap, block::Block) = getbin(heap, block.size)

binsize(::FastHeap, bin) = binsize(FastHeapDispatch(), bin)

basepointer(heap::FastHeap) = heap.base
Base.sizeof(heap::FastHeap) = heap.len
baseaddress(heap::FastHeap) = convert(UInt, heap.base)
numbins(heap::FastHeap) = length(heap.freelists)

function pushfreelist!(heap::FastHeap, block)
    bin = getbin(heap, block)
    list = get!(heap.freelists, bin, Freelist{Block}())

    isempty(list) && setentry!(heap.status, bin)
    push!(list, block)
    return nothing
end

# Pop a block for this bin.
# If a freelist doesn't exist for this bin, split the next largest block.
#
# Assume `sz` is a multiple of the heap granularity
function popfreelist!(heap::FastHeap, bin)
    list = get(heap.freelists, bin, nothing)
    if !isnothing(list)
        block = pop!(list)

        # If this freelist is now empty, clean up the tracker.
        if isempty(list)
            delete!(heap.freelists, bin)
            clearentry!(heap.status, bin)
        end
        return block
    end

    # Okay, we don't have a block.
    # We need to get the next highest available block and split it.
    nextbin = findnext(heap.status, bin)
    isnothing(nextbin) && return nothing

    list = heap.freelists[nextbin]
    block = pop!(list)
    if isempty(list)
        delete!(heap.freelists, nextbin)
        clearentry!(heap.status, nextbin)
    end

    # Now that we have the block, split it down to the size we want and put the remainder
    # back in the heap.
    sz = binsize(heap, bin)
    remainder = Block(pointer(block) + sz)

    remainder.free = true
    remainder.size = block.size - sz
    remainder.next = Block()
    remainder.previous = Block()

    # Since we split this block, we need to maintain the backsize pointers for efficient
    # merging.
    remainder.backsize = block.size
    next = walknext(heap, remainder)
    if !isnothing(next)
        next.backsize = remainder.size
    end

    block.size = sz
    pushfreelist!(heap, remainder)
    return block
end

function remove!(heap::FastHeap, block::Block)
    bin = getbin(heap, block)
    list = heap.freelists[bin]
    remove!(list, block)
    if isempty(list)
        delete!(heap.freelists, bin)
        clearentry!(heap.status, bin)
    end
    return nothing
end

function putback!(heap::FastHeap, block::Block)
    @check isfree(block)

    # Check the block before and after are free.
    #
    # If so, merge the blocks
    previous = walkprevious(heap, block)
    next = walknext(heap, block)

    if !isnothing(previous) && isfree(previous)
        remove!(heap, previous)
        previous.size += block.size
        block = previous
    end

    if !isnothing(next)
        if isfree(next)
            remove!(heap, next)
            block.size += next.size

            # Set the backedge for the nextnext block
            nextnext = walknext(heap, next)
            if !isnothing(nextnext)
                nextnext.backsize = block.size
            end
        else
            next.backsize = block.size
        end
    end

    pushfreelist!(heap, block)
    return nothing
end

#####
##### High level API
#####

function alloc(heap::FastHeap, bytes::Integer, id::UInt)
    iszero(bytes) && return nothing

    bin = getbin(heap, max(bytes + headersize(), FASTHEAP_GRANULARITY))
    block = popfreelist!(heap, bin)
    isnothing(block) && return nothing

    block.free = false
    block.evicting = false
    block.id = id
    block.pool = heap.pool

    ptr = pointer(block) + headersize()
    return ptr
end

function free(heap::FastHeap, ptr::Ptr{Nothing})
    block = unsafe_block(ptr)
    block.evicting == true && return nothing
    block.free = true
    putback!(heap, block)
    return nothing
end
