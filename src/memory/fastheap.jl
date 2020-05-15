# Granularity of allocations.
const FASTHEAP_GRANULARITY = 2^22

# So we can access "binsize" before a `FastHeap` is instantiated.
struct FastHeapDispatch end

getbin(::FastHeapDispatch, sz) = div(sz - 1, FASTHEAP_GRANULARITY) + 1
binsize(::FastHeapDispatch, bin) = bin * FASTHEAP_GRANULARITY

#####
##### FastHeap
#####

mutable struct FastHeap{T} <: AbstractHeap
    allocator::T

    # Where is and what kind of memory do we have?
    base::Ptr{Nothing}
    len::Int
    pool::Pool

    # Maintaining freelist status
    status::FindNextTree
    freelists::Dict{Int,Freelist{Block}}
end

function FastHeap(allocator::T, sz; pool = DRAM) where {T}
    # Round down size to the nearest multiple of `FASTHEAP_GRANULARITY`
    sz = FASTHEAP_GRANULARITY * div(sz, FASTHEAP_GRANULARITY)

    # Allocate the memory managed by this heap
    base = allocate(allocator, sz)
    numbins = getbin(FastHeapDispatch(), sz)
    status = FindNextTree(numbins)
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

    push!(heap, block)
    return heap
end

getbin(::FastHeap, sz) = getbin(FastHeapDispatch(), sz)
getbin(heap::FastHeap, block::Block) = getbin(heap, block.size)

binsize(::FastHeap, bin) = binsize(FastHeapDispatch(), bin)

basepointer(heap::FastHeap) = heap.base
Base.sizeof(heap::FastHeap) = heap.len
baseaddress(heap::FastHeap) = convert(UInt, heap.base)
numbins(heap::FastHeap) = length(heap.freelists)

# The FastHeap maintains backsizes, so we can also walk backwards.
function walkprevious(heap::FastHeap, block)
    # Invariant, the first block will always have a backsize of zero.
    backsize = block.backsize
    if iszero(backsize)
        return nothing
    else
        return Block(pointer(block) - backsize)
    end
end

"""
    sortfreelists!(heap)

Sort all of the freelists in `heap` in order of increasing address.
"""
function sortfreelists!(heap)
    for freelist in values(heap.freelists)
        sort!(freelist)
    end
end

function Base.push!(heap::FastHeap, block::Block)
    bin = getbin(heap, block)
    list = get!(heap.freelists, bin, Freelist{Block}())

    isempty(list) && setentry!(heap.status, bin)
    push!(list, block)
    return nothing
end

popfreelist!(heap::FastHeap, bin::Integer) = popfreelist!(heap, heap.freelists[bin], bin)
function popfreelist!(heap::FastHeap, list::Freelist, bin::Integer)
    block = pop!(list)
    if isempty(list)
        delete!(heap.freelists, bin)
        clearentry!(heap.status, bin)
    end
    return block
end

# Pop a block for this bin.
# If a freelist doesn't exist for this bin, split the next largest block.
#
# Assume `sz` is a multiple of the heap granularity
function Base.pop!(heap::FastHeap, bin)
    list = get(heap.freelists, bin, nothing)
    if !isnothing(list)
        return popfreelist!(heap, list, bin)
    end

    # Okay, we don't have a block.
    # We need to get the next highest available block and split it.
    nextbin = findnext(heap.status, bin)

    # No bigger block available.
    # This means we can't fulfill this request.
    # Return `nothing` to indicate this.
    isnothing(nextbin) && return nothing

    # Otherwise, pop of this
    block = popfreelist!(heap, nextbin)

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
    block.size = sz
    remainder.backsize = block.size
    next = walknext(heap, remainder)
    if !isnothing(next)
        next.backsize = remainder.size
    end

    push!(heap, remainder)
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

    push!(heap, block)
    return nothing
end

#####
##### High level API
#####

function alloc(heap::FastHeap, bytes::Integer, id::UInt)
    iszero(bytes) && return nothing

    bin = getbin(heap, max(bytes + headersize(), FASTHEAP_GRANULARITY))
    block = pop!(heap, bin)
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

#####
##### Eviction API
#####

# We don't have buddy constraints here - so as long as the request is less than the size
# of the entire heap, we can allocate it.
canallocfrom(heap::FastHeap, block::Block, sz) = (sz < sizeof(heap))

function unsafe_free!(f, block::Block)
    @check !isfree(block)

    # Mark this block as being evicted.
    # This will short-circuit any calls to `free` the result from the callback
    block.evicting = true

    # Do the callback on the block and then mark the blocked as freed.
    f(block)
    block.free = true
    return nothing
end

function evict!(f::F, heap, block) where {F}
    isfree(block) ? remove!(heap, block) : unsafe_free!(f, block)
    return nothing
end

function evictfrom!(heap::FastHeap, block::Block, sz; cb = donothing)
    bsz = binsize(getbin(heap, sz + headersize()))

    current = block
    evict!(cb, heap, block)

    sizefreed = 0
    # Walk forward until either we have freed enough space or reach the end of the heap.
    while sizefreed < sz && !isnothing(current)
        current = walknext(heap, current)
        sizefreed += current.size
        evict!(cb, heap, block)
    end

    # Walk backwards if we have to in order to free enough space.
    if sizefreed < sz
        current = block
        while sizefreed < sz
            current = walkprevious(heap, block)
            sizefreed += current.size
            evict!(cb, heap, block)
        end
    end

    # Resize our block and put it back in the heap.
    block.free = true
    block.size = sizefreed
    putback!(heap, block)
    return nothing
end

#####
##### Checker
#####

# Check the various invariants of the FastHeap
#
# 1. First Block should always have zero backsize
# 2. Any freelist entry in the freelist dict must have an entry in the `status` tree.
# 3. All non-entries in the `status` tree must not have entry in the freelist dict.
# 4. Forward and backsizes must be consistent.
# 5. Free blocks found by walking the heap must be consistent with free-blocks found walking
#    the freelists

function first_block_invariant(heap::FastHeap)
    block = baseblock(heap)
    if !iszero(block.backsize)
        println("Base block has nonzero backsize: $(block.backsize)")
        return false
    end
    return true
end

function freelist_status_invariant(heap::FastHeap)
    maxbin = binsize(heap, sizeof(heap))
    passed = true

    # Walk the `status` tree and correlate the entries with the freelist dict.
    for (i, mask) in enumerate(last(heap.status.runs))
        for j in 1:64
            bin = 64*(i-1) + j
            bin > maxbin && return passed

            if hasentryat(mask, j)
                # There must be a key in the `freelists` dictionary and it must be non-empty
                if !haskey(heap.freelists, bin)
                    println("Heap has a `status` entry for bin $bin but no corresponding freelist bound")
                    passed = false
                end
            else
                # No entry here - there must not be an entry in the dict.
                if haskey(heap.freelists, bin)
                    println("Heap as no `status` entry for $bin but found a freelist")
                    passed = false
                end
            end

            if haskey(heap.freelists, bin) && isempty(heap.freelists[bin])
                println("Found an empty freelist for bin $bin")
                passed = false
            end
        end
    end
    return passed
end

function size_invariant(heap::FastHeap)
    passed = true

    # This certainly better not be nothing!
    # Heaps should always have at least one entry
    y = iterate(heap)
    if isnothing(y)
        println("Empty Heap!!")
        passed = false
    end
    block, state = y

    count = 1
    while true
        y = iterate(heap, state)
        isnothing(y) && break
        next, state = y

        if next.backsize != block.size
            println("Block $(count + 1) has backsize $(next.backsize). Expected $(block.size)")
            passed = false
        end

        if block.free && next.free
            println("Unmerged free blocks in position $(count) and $(count + 1)")
            passed = false
        end
        count += 1
        block = next
    end
    return passed
end

function free_invariant(heap::FastHeap)
    passed = true
    counts = Dict{Int,Int}()

    for block in heap
        if isfree(block)
            bin = getbin(heap, block)
            get!(counts, bin, 0)
            counts[bin] += 1
        end
    end

    for (bin, freelist) in heap.freelists
        if !haskey(counts, bin)
            println("Found a freelist for bin $bin but no block of this size found")
            passed = false
        end

        if length(freelist) != counts[bin]
            println("Length of freelist is $(length(freelist)) but counted $(counts[bin]) blocks")
            passed = false
        end
        delete!(counts, bin)
    end

    # Did we miss any
    if !isempty(counts)
        println("Unaccounted blocks")
        passed = false
        for (bin, count) in counts
            println("    Bin: $bin - Count: $count")
        end
    end

    return passed
end

function check(heap::FastHeap)
    passed = true
    if !first_block_invariant(heap)
        printstyled(stdout, "Heap failed First Block Invariant\n"; color = :red)
        passed = false
    end

    if !freelist_status_invariant(heap)
        printstyled(stdout, "Heap failed First List Invariant\n"; color = :red)
        passed = false
    end

    if !size_invariant(heap)
        printstyled(stdout, "Heap failed size invariant\n"; color = :red)
        passed = false
    end

    if !free_invariant(heap)
        printstyled(stdout, "Heap failed free invariant\n"; color = :red)
        passed = false
    end
    return passed
end
