# So we can access "binsize" before a `CompactHeap` is instantiated.
struct PowerOfTwo
    val::Int
end

poweroftwo(x::PowerOfTwo) = x
poweroftwo(x::Integer) = PowerOfTwo(x)

getbin(x::PowerOfTwo, sz) = ((sz - 1) >> x.val) + 1
binsize(x::PowerOfTwo, bin) = bin << x.val

#####
##### Iterate over a vector of `Blocks` as a vector of `FreelistPtr`.
#####

struct FreelistIterator{T}
    lists::Vector{T}
end

Base.length(x::FreelistIterator) = length(x.lists)
Base.pointer(x::FreelistIterator, i) = pointer(x.lists, i)
function Base.iterate(x::FreelistIterator, i = 1)
    i > length(x) && return nothing
    return FreelistPtr(pointer(x, i)), i + 1
end

#####
##### CompactHeap
#####

mutable struct CompactHeap <: AbstractHeap
    minallocation::PowerOfTwo

    # Where is and what kind of memory do we have?
    base::Ptr{Nothing}
    len::Int
    allocated::Int
    pool::Pool

    # Maintaining freelist status
    status::FindNextTree
    freelists::Vector{Block}
    canalloc::Bool

    # Allocator token.
    # Can be used to preserve `base` as long as the heap is still alive.
    token::Any
end

topointer(ptr::Ptr) = ptr
topointer(A::AbstractArray) = convert(Ptr{Nothing}, pointer(A))
getstate(heap::CompactHeap) = (heap.allocated, heap.len)

getfreelist(heap::CompactHeap, i) = FreelistPtr(pointer(heap.freelists, i))
freelists(heap::CompactHeap) = FreelistIterator(heap.freelists)

function CompactHeap(
    allocator::AbstractAllocator,
    sz;
    pool = Local,
    # Power of two
    minallocation = PowerOfTwo(21),
)
    minallocation = poweroftwo(minallocation)

    # Round down size to the nearest multiple of `minallocation`
    sz = (sz >> minallocation.val) << minallocation.val

    # Allocate the memory managed by this heap
    base, token = allocate(allocator, sz)
    allocated = 0

    numbins = getbin(minallocation, sz)
    status = FindNextTree(numbins)
    freelists = [Block() for _ in Base.OneTo(numbins)]

    heap = CompactHeap(
        minallocation,
        base,
        sz,
        allocated,
        pool,
        status,
        freelists,
        true,
        token,
    )

    finalizer(heap) do x
        free(x.base, x.token)
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
function walkprevious(::CompactHeap, block)
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
sortfreelists!(heap) = foreach(sort!, freelists(heap))

function Base.push!(heap::CompactHeap, block::Block)
    bin = getbin(heap, block)
    list = @inbounds(getfreelist(heap, bin))

    isempty(list) && @inbounds(setentry!(heap.status, bin))
    push!(list, block)
    return nothing
end

popfreelist!(heap::CompactHeap, bin::Integer) = popfreelist!(heap, getfreelist(heap, bin), bin)
Base.@propagate_inbounds function popfreelist!(
    heap::CompactHeap,
    list::AbstractFreelist,
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
    if isset(heap.status, bin)
        return @inbounds(popfreelist!(heap, bin))
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
    isset(heap.status, bin) && return true

    # Okay, we don't have a block.
    # We need to get the next highest available block and split it.
    nextbin = @inbounds(findnext(heap.status, bin))

    # No bigger block available - cannot allocate.
    return nextbin === nothing ? false : true
end

function remove!(heap::CompactHeap, block::Block)
    bin = getbin(heap, block)
    list = getfreelist(heap, bin)
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

    if previous !== nothing && isfree(previous)
        remove!(heap, previous)
        previous.size += block.size

        # Purge metadata for this block to try and catch errors earlier.
        zerometa!(block)
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

            # Purge metadata for the next block to try and catch errors earlier.
            zerometa!(next)
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

const CAN_ALLOC_MESSAGE = """
Something went wrong. Trying to allocate from a heap that has been temporarily marked
as "no-allocate". It's likely something went wrong during eviction.
"""

function alloc(heap::CompactHeap, bytes::Integer, id::UInt)
    heap.canalloc || error(CAN_ALLOC_MESSAGE)
    iszero(bytes) && return nothing

    needed_size = max(bytes + headersize(), one(bytes) << heap.minallocation.val)
    needed_size > sizeof(heap) && return nothing
    bin = getbin(heap, needed_size)
    block = pop!(heap, bin)
    block === nothing && return nothing

    # Setup the default state for the block
    heap.allocated += sizeof(block)
    clearbits!(block)
    block.id = id
    block.pool = heap.pool

    # Preemptively mark as dirty.
    # Up to callee's to determine if this is actually clean or not.
    block.dirty = true
    ptr = pointer(block) + headersize()
    return ptr
end

@inline function canalloc(heap::CompactHeap, bytes::Integer)
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
    block.queued = false
    block.free = true
    heap.allocated -= sizeof(block)
    putback!(heap, block)
    return nothing
end

#####
##### Defrag API
#####

function nextfree(heap, block = baseblock(heap))
    while block !== nothing && !isfree(block)
        block = walknext(heap, block)
    end
    return block
end

"""
    defrag!(f::F, heap::CompactHeap; [nthreads], [queued_callback])

Defragment `heap`. Callback function `f` is given an ID and a new block.
If a block is marked as `queued`, then, by default, we can't touch it.
However, the caller may provide a zero argument `queued_callback` which must free this
block, in which case defragmentation may continue.
"""
function defrag!(
    f::F,
    heap::CompactHeap;
    nthreads = Threads.nthreads(),
    queued_callback = nothing,
) where {F}
    @checknothing freeblock = nextfree(heap)
    @checknothing block = walknext(heap, freeblock)

    while true
        # By the invariants we keep on the heap, the next block should be not-free.
        # However, it MIGHT be queued for deletion, in which case, we can't move it.
        if block.queued
            if queued_callback !== nothing
                queued_callback()
                # Recurse to begin the walk again since it's possible that blocks
                # we've already visited have now been cleaned up.
                # println("REWALKING!")
                defrag!(f, heap; nthreads, queued_callback)
                return nothing
            else
                @checknothing freeblock = nextfree(heap, block)
                @checknothing block = walknext(heap, freeblock)
            end
            continue
        end

        # The following operations need to be atomic as far as the GC is concerned.
        # This takes care of if the GC runs while we're moving data and the block we
        # are moving suddenly becomes free ...

        # Now, we need to move "block" to "freeblock" and then fixup the heap.
        id = block.id
        # safeprint("Defrag: Moving $id")
        pool = block.pool
        block.backsize = freeblock.backsize
        freeblock_size = freeblock.size
        remove!(heap, freeblock)

        # After the "memcpy", the new location for "block" is "freeblock".
        # Thus, we need to perform the callback so that references can be updated.
        aliases = pointer(freeblock) + sizeof(block) > pointer(block)
        f(id, freeblock, block)
        if aliases
            _memcpy!(pointer(freeblock), pointer(block), sizeof(block); nthreads = nothing)
        else
            _memcpy!(pointer(freeblock), pointer(block), sizeof(block); nthreads)
        end

        @check freeblock.id == id

        # Update siblings
        sibling = getsibling(freeblock)
        if sibling !== nothing
            sibling.sibling = freeblock
        end

        # Since we haven't changed the size of the block, "walknext" should still work,
        # though the actual block returned will be garbage.
        block = freeblock
        freeblock = walknext(heap, block)

        freeblock.backsize = block.size
        freeblock.size = freeblock_size
        freeblock.free = true

        # TODO: In all likelyhood, we're just going to be removing the freeblock again.
        # However, dealing with queued blocks makes this tricky so do the conservative
        # thing for now.
        putback!(heap, freeblock)

        @checknothing block = walknext(heap, freeblock)
        block.backsize = freeblock.size
    end
    return nothing
end

#####
##### Eviction API
#####

function unsafe_free!(f::F, block::Block) where {F}
    @check !isfree(block)

    # Mark this block as being evicted.
    # This will short-circuit any calls to `free` the result from the callback while
    # still keeping the block as `not-free` to be compatible with the other heap logic.
    block.evicting = true

    # Do the callback on the block and then mark the blocked as freed.
    return f(block)
end

function evict!(f::F, heap::CompactHeap, block::Block) where {F}
    if isfree(block)
        remove!(heap, block)
        block.free = false
        heap.allocated += sizeof(block)
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
                if iszero(sizefreed)
                    heap.canalloc = true
                    return nothing
                end
                current = last
            end

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
        candidate = walkprevious(heap, block)
        candidate === nothing && break
        # Intenionally overwrite the argument "block".
        block = candidate
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
    heap.allocated -= sizefreed

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
    Polyester.@batch per = thread for i = 1:4096:sizeof(heap)
        unsafe_store!(ptr, one(UInt8), i)
    end
    return nothing
end

function touchheap(heap::CompactHeap)
    v = zeros(UInt8, Threads.nthreads())
    ptr = convert(Ptr{UInt8}, datapointer(baseblock(heap)))
    Polyester.@batch per = thread for i = 1:64:sizeof(heap)
        v[Threads.threadid()] += unsafe_load(ptr, i)
    end
    return sum(v)
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
                if isempty(getfreelist(heap, bin))
                    println(
                        "Heap has a `status` entry for bin $bin but no corresponding freelist bound",
                    )
                    passed = false
                end
            else
                # No entry here - there must not be an entry in the dict.
                if !isempty(getfreelist(heap, bin))
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
            println(
                "Block $(count + 1) has backsize $(next.backsize). Expected $(block.size)",
            )
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

    for (bin, freelist) in enumerate(freelists(heap))
        if !isempty(freelist)
            if !haskey(counts, bin)
                println("Found a freelist for bin $bin but no block of this size found")
                passed = false
            else
                if length(freelist) != counts[bin]
                    println(
                        "Length of freelist is $(length(freelist)) but counted $(counts[bin]) blocks",
                    )
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
