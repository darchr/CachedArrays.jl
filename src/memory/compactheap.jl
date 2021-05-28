# So we can access "binsize" before a `CompactHeap` is instantiated.
struct PowerOfTwo
    val::Int
end

poweroftwo(x::PowerOfTwo) = x
poweroftwo(x::Integer) = PowerOfTwo(x)

getbin(x::PowerOfTwo, sz) = ((sz - 1) >> x.val) + 1
binsize(x::PowerOfTwo, bin) = bin << x.val

#####
##### CompactHeap
#####

mutable struct CompactHeap{T} <: AbstractHeap
    allocator::T
    minallocation::PowerOfTwo

    # Where is and what kind of memory do we have?
    basehost::Any
    base::Ptr{Nothing}
    len::Int
    pool::Pool

    # Maintaining freelist status
    status::FindNextTree
    freelists::Vector{Freelist{Block}}
    canalloc::Bool
end

topointer(ptr::Ptr) = ptr
topointer(A::AbstractArray) = convert(Ptr{Nothing}, pointer(A))

function CompactHeap(
    allocator::T,
    sz;
    pool = Local,
    # Power of two
    minallocation = PowerOfTwo(21),
) where {T}
    minallocation = poweroftwo(minallocation)

    # Round down size to the nearest multiple of `minallocation`
    sz = (sz >> minallocation.val) << minallocation.val

    # Allocate the memory managed by this heap
    basehost = allocate(allocator, sz)
    base = topointer(basehost)

    numbins = getbin(minallocation, sz)
    status = FindNextTree(numbins)
    freelists = [Freelist{Block}() for _ = 1:numbins]

    heap = CompactHeap{T}(
        allocator,
        minallocation,
        basehost,
        base,
        sz,
        pool,
        status,
        freelists,
        true,
    )

    finalizer(heap) do x
        free(x.allocator, x.basehost)
    end

    # Add an entry for the biggest freelist.
    block = Block(base)
    block.size = sz
    block.backsize = 0
    block.free = true
    block.pool = pool
    block.next = Block()
    block.previous = Block()

    push!(heap, block)
    return heap
end

getbin(heap::CompactHeap, sz) = getbin(heap.minallocation, sz)
getbin(heap::CompactHeap, block::Block) = getbin(heap, block.size)

binsize(heap::CompactHeap, bin) = binsize(heap.minallocation, bin)

basepointer(heap::CompactHeap) = heap.base
Base.sizeof(heap::CompactHeap) = heap.len
baseaddress(heap::CompactHeap) = convert(UInt, heap.base)
numbins(heap::CompactHeap) = length(heap.freelists)

# The CompactHeap maintains backsizes, so we can also walk backwards.
function walkprevious(heap::CompactHeap, block)
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
    for freelist in heap.freelists
        sort!(freelist)
    end
end

function Base.push!(heap::CompactHeap, block::Block)
    bin = getbin(heap, block)
    list = @inbounds(heap.freelists[bin])

    isempty(list) && @inbounds(setentry!(heap.status, bin))
    push!(list, block)
    return nothing
end

popfreelist!(heap::CompactHeap, bin::Integer) = popfreelist!(heap, heap.freelists[bin], bin)
Base.@propagate_inbounds function popfreelist!(
    heap::CompactHeap,
    list::Freelist,
    bin::Integer,
)
    block = pop!(list)
    isempty(list) && clearentry!(heap.status, bin)
    return block
end

# Pop a block for this bin.
# If a freelist doesn't exist for this bin, split the next largest block.
#
# Assume `sz` is a multiple of the heap granularity
function Base.pop!(heap::CompactHeap, bin)
    list = @inbounds heap.freelists[bin]
    if !isempty(list)
        return @inbounds(popfreelist!(heap, list, bin))
    end

    # Okay, we don't have a block.
    # We need to get the next highest available block and split it.
    nextbin = @inbounds(findnext(heap.status, bin))

    # No bigger block available.
    # This means we can't fulfill this request.
    # Return `nothing` to indicate this.
    nextbin === nothing && return nothing

    # Otherwise, take this block and split it.
    # TODO: Maybe set a limit where we don't split if the over-allocation amount is
    # less than this limit.
    block = @inbounds(popfreelist!(heap, nextbin))

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
    if next !== nothing
        next.backsize = remainder.size
    end

    push!(heap, remainder)
    return block
end

function canpop(heap::CompactHeap, bin)
    list = @inbounds heap.freelists[bin]
    if !isempty(list)
        return true
    end

    # Okay, we don't have a block.
    # We need to get the next highest available block and split it.
    nextbin = @inbounds(findnext(heap.status, bin))

    # No bigger block available - cannot allocate.
    return nextbin === nothing ? false : true
end

function remove!(heap::CompactHeap, block::Block)
    bin = getbin(heap, block)
    list = @inbounds(heap.freelists[bin])
    remove!(list, block)
    isempty(list) && @inbounds(clearentry!(heap.status, bin))
    return nothing
end

function putback!(heap::CompactHeap, block::Block)
    @check isfree(block)

    # Check the block before and after are free.
    #
    # If so, merge the blocks
    previous = walkprevious(heap, block)
    next = walknext(heap, block)

    # If the block before this one is being evicted, we don't want to merge since that
    # will break upstream logic.
    if previous !== nothing && isfree(previous)
        remove!(heap, previous)
        previous.size += block.size
        block = previous
    end

    if next !== nothing
        # Similar to the case with "block.previous", if the next block is being evicted,
        # we don't want to try to merge since that will break eviction logic.
        if isfree(next)
            remove!(heap, next)
            block.size += next.size

            # Set the backedge for the nextnext block
            nextnext = walknext(heap, next)
            if nextnext !== nothing
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

function alloc(heap::CompactHeap, bytes::Integer, id::UInt)
    heap.canalloc || error("Uh oh")
    iszero(bytes) && return nothing

    needed_size = max(bytes + headersize(), one(bytes) << heap.minallocation.val)
    needed_size > sizeof(heap) && return nothing
    bin = getbin(heap, needed_size)
    block = pop!(heap, bin)
    block === nothing && return nothing

    # Setup the default state for the block
    clearbits!(block)
    block.id = id
    block.pool = heap.pool

    ptr = pointer(block) + headersize()
    return ptr
end

function canalloc(heap::CompactHeap, bytes::Integer)
    iszero(bytes) && return false

    needed_size = max(bytes + headersize(), one(bytes) << heap.minallocation.val)
    needed_size > sizeof(heap) && return false
    bin = getbin(heap, needed_size)
    return canpop(heap, bin)
end

function free(heap::CompactHeap, ptr::Ptr{Nothing})
    block = unsafe_block(ptr)
    if block.evicting == true
        block.orphaned = true
        return nothing
    end
    block.free = true
    putback!(heap, block)
    return nothing
end

#####
##### Eviction API
#####

function unsafe_free!(f, block::Block)
    @check !isfree(block)

    # Mark this block as being evicted.
    # This will short-circuit any calls to `free` the result from the callback while
    # still keeping the block as `not-free` to be compatible with the other heap logic.
    block.evicting = true

    # Do the callback on the block and then mark the blocked as freed.
    return f(block)
end

function evict!(f::F, heap, block) where {F}
    if isfree(block)
        remove!(heap, block)
        block.free = false
        return nothing
    else
        return unsafe_free!(f, block)
    end
end

# Helper method to get a block from a pointer.
function evictfrom!(heap::CompactHeap, ptr::Ptr{Nothing}, sz; kw...)
    evictfrom!(heap, unsafe_block(ptr), sz; kw...)
end

function evictfrom!(heap::CompactHeap, block::Block, sz; cb = donothing)
    heap.canalloc = false
    sizefreed = 0
    last = block
    current = block

    aborted = false

    # NOTE ON ABORTING.
    # Control flow must always pass through the bottom of this function to ensure
    # that even if an eviction is aborted early that we maintain the heap in a
    # consistent state.

    # Walk forward until either we have freed enough space or reach the end of the heap.
    while true
        abort = evict!(cb, heap, current)

        # Check for early abort
        if (abort === true)
            # Cleanup eviction status
            current.evicting = false

            # Aborted on the first block.
            # If this block was NOT orphaned as a side-effect of the eviction callback,
            # then we need to rollback the current block to the last successfully
            # processed block.
            #
            # However, if this block was recaimed, then it is free but hasn't been
            # put back on the heap.
            #
            # As a result, we need to include it in the freed frontier.
            if current.orphaned
                sizefreed += current.size
            else
                iszero(sizefreed) && return nothing
                current = last
            end

            aborted = true
            @goto cleanup
        end
        sizefreed += current.size

        # Update state
        last = current
        current = walknext(heap, current)
        # If we've either freed enough memory or walked off the end of the array, exit.
        (sizefreed >= sz || current === nothing) && break
    end

    # Walk backwards if we have to in order to free enough space.
    while sizefreed < sz
        block = walkprevious(heap, block)
        abort = evict!(cb, heap, block)
        if (abort === true)
            block.evicting = false
            if !block.orphaned
                # Roll back block to last successful eviction.
                block = walknext(heap, block)
            else
                sizefreed += block.size
            end
            @goto cleanup
        end
        sizefreed += block.size
    end

    # Resize our block and put it back in the heap.
    @label cleanup
    block.free = true
    block.orphaned = false
    block.size = sizefreed
    putback!(heap, block)
    heap.canalloc = true
    return nothing
end

#####
##### Materializer
#####

# Touch all the pages in an allocation.
function materialize_os_pages!(heap::CompactHeap)
    # First, we must make sure that NOTHING is allocated in the heap - otherwise, we might
    # over write some existing data.
    if length(heap) != 1 || !isfree(first(heap))
        throw(error("Can only materialize OS pages for an empty heap!"))
    end

    # Now, we threaded touch all pages in the heap
    #
    # Take steps of 4096, which is the smallest possible page size.
    ptr = convert(Ptr{UInt8}, datapointer(baseblock(heap)))
    Threads.@threads for i = 1:4096:sizeof(heap)
        unsafe_store!(ptr, one(UInt8), i)
    end
    return nothing
end

#####
##### Checker
#####

# Check the various invariants of the CompactHeap
#
# 1. First Block should always have zero backsize
# 2. Any freelist entry in the freelist dict must have an entry in the `status` tree.
# 3. All non-entries in the `status` tree must not have entry in the freelist dict.
# 4. Forward and backsizes must be consistent.
# 5. Free blocks found by walking the heap must be consistent with free-blocks found walking
#    the freelists

function first_block_invariant(heap::CompactHeap)
    block = baseblock(heap)
    if !iszero(block.backsize)
        println("Base block has nonzero backsize: $(block.backsize)")
        return false
    end
    return true
end

function freelist_status_invariant(heap::CompactHeap)
    maxbin = getbin(heap, sizeof(heap))
    passed = true

    # Walk the `status` tree and correlate the entries with the freelist dict.
    for (i, mask) in enumerate(last(heap.status.runs))
        for j = 1:64
            bin = 64 * (i - 1) + j
            bin > maxbin && return passed

            if hasentryat(mask, j)
                # There must be a key in the `freelists` dictionary and it must be non-empty
                if isempty(heap.freelists[bin])
                    println("Heap has a `status` entry for bin $bin but no corresponding freelist bound")
                    passed = false
                end
            else
                # No entry here - there must not be an entry in the dict.
                if !isempty(heap.freelists[bin])
                    println("Heap as no `status` entry for $bin but found a freelist")
                    passed = false
                end
            end
        end
    end
    return passed
end

function size_invariant(heap::CompactHeap)
    passed = true

    # This certainly better not be nothing!
    # Heaps should always have at least one entry
    y = iterate(heap)
    if y === nothing
        println("Empty Heap!!")
        passed = false
    end
    block, state = y

    count = 1
    while true
        y = iterate(heap, state)
        y === nothing && break
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

function free_invariant(heap::CompactHeap)
    passed = true
    counts = Dict{Int,Int}()

    for block in heap
        if isfree(block)
            bin = getbin(heap, block)
            get!(counts, bin, 0)
            counts[bin] += 1
        end
    end

    for (bin, freelist) in enumerate(heap.freelists)
        if !isempty(freelist)
            if !haskey(counts, bin)
                println("Found a freelist for bin $bin but no block of this size found")
                passed = false
            else
                if length(freelist) != counts[bin]
                    println("Length of freelist is $(length(freelist)) but counted $(counts[bin]) blocks")
                    passed = false
                end
                delete!(counts, bin)
            end
        end
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

function check(heap::CompactHeap)
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
