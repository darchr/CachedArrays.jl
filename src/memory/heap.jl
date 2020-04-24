# A buddy-based heap allocator.
# All allocations supplied by this allocator are aligned to 64B
# We use a 64B header and footer to store metadata.
isnull(x::Ptr) = isnull(convert(UInt, x))
isnull(x::UInt) = iszero(x)

#####
##### Tunable Parameters
#####

# The minimum granularity that we allocate at.
const MIN_ALLOCATION = 4096
const LOG2_MIN_ALLOCATION = ceil(Int, log2(MIN_ALLOCATION))

getbin(sz) = ceil(Int, log2(sz)) - LOG2_MIN_ALLOCATION + 1
getbin(block::Block) = getbin(block.size)
binsize(i) = MIN_ALLOCATION * 2^(i-1)

mutable struct BuddyHeap{T}
    allocator::T

    # The base pointer for the heap we're managing.
    base::Ptr{Nothing}
    len::Int

    # Which pool is this?
    pool::Pool

    # Stats for tracking number of allocations, fragmentation etc.
    allocations::Int
    allocated::Int

    # The start to blocks, indexed by their size in the buddy system.
    freelists::Vector{Block}
end

# Get the buddy block for a given header.
# Ptr is a pointer to the block that `header` belongs to.
function getbuddy(heap::BuddyHeap, block::Block)
    # Is this the biggest block that we can allocate?
    # If so, it doesn't have a buddy.
    block.size == binsize(numbins(heap)) && return nothing

    base = baseaddress(heap)
    offset = address(block) - base
    shift = xor(block.size, offset)
    buddy_address = shift + base

    # If the buddy address is out of range, this block does not have a buddy.
    if buddy_address + block.size > base + sizeof(heap)
        return nothing
    else
        return Block(shift + base)
    end
end

Base.sizeof(heap::BuddyHeap) = heap.len

baseaddress(heap::BuddyHeap) = convert(UInt, heap.base)
numbins(heap::BuddyHeap) = length(heap.freelists)

function BuddyHeap(allocator::T, sz; pool = DRAM, maxallocation = nothing) where {T}
    # Make an initial allocation of `sz`.
    base = allocate(allocator, sz)

    # Initialize the freelists.
    # First - figure out how many bins we need.
    maxbin = getbin(sz)
    freelists = [Block() for _ in 1:maxbin]

    heap = BuddyHeap{T}(
        allocator,
        base,
        sz,
        pool,
        0,
        0,
        freelists,
    )

    finalizer(heap) do x
        free(x.allocator, x.base)
    end

    # Tile the free space.
    size_left = sz
    ptr = base

    # Decide where to start based on the max-allocation size
    currentsize = isnothing(maxallocation) ? binsize(maxbin) : binsize(getbin(maxallocation))

    while currentsize >= MIN_ALLOCATION
        # Check if we can allocate a chunk of this size.
        # If so, do it.
        if size_left >= currentsize
            block = Block(ptr)
            block.size = currentsize
            block.free = true

            push_freelist!(heap, block)

            size_left -= currentsize
            ptr += currentsize
        else
            currentsize = currentsize >> 1
        end
    end
    # Resize to reflect the amount of memory we are actually using.
    heap.len -= size_left
    return heap
end

# Implement a function to iterate through the whole heap.
function Base.iterate(heap::BuddyHeap)
    block = Block(heap.base)
    return (block, block)
end
function Base.iterate(heap::BuddyHeap, block::Block)
    if pointer(block) + block.size >= heap.base + sizeof(heap)
        return nothing
    end
    block = Block(pointer(block) + block.size)
    return (block, block)
end

function slowlength(heap::BuddyHeap)
    count = 0
    for block in heap
        count += 1
    end
    return count
end

# Push an item onto the free list.
function push_freelist!(heap::BuddyHeap, block::Block)
    # Get the header for this block - check what bin it should be in.
    @check ispow2(block.size)
    @check isfree(block)

    bin = getbin(block.size)
    base = heap.freelists[bin]
    # No entries in this list
    if isnull(base)
        block.next = Block()
    # Insert this block at the front of the freelist.
    else
        @check isnull(base.previous)
        base.previous = block
        block.next = base
    end

    # Null out the `previous` field of this header.
    block.previous = Block()
    heap.freelists[bin] = block
    return nothing
end


# Functions for managing the double-linked free lists.
function remove!(heap, block::Block)
    # In this case, we need to pop the block from the freelist
    # since it's the first one.
    if isnull(block.previous)
        bin = getbin(block.size)
        heap.freelists[bin] = block.next
    else
        block.previous.next = block.next
    end

    if !isnull(block.next)
        block.next.previous = block.previous
    end
    # Temporarily clear out next and previous fields.
    block.next = Block()
    block.previous = Block()
    return nothing
end

function split!(heap::BuddyHeap, block::Block)
    @check block.free

    # Split a block into two blocks half the size.
    #
    # Step 1: Remove this block from the freelist for this size bucket.
    remove!(heap, block)

    # Step 2: Split the block in half.
    newsize = block.size >> 1
    block.size = newsize

    # Get the buddy block and initialize its fields.
    # This is guarneteed to work because we're splitting, so we definitely know that
    # `block` has a buddy.
    buddy = getbuddy(heap, block)::Block

    block.next = buddy

    buddy.size = newsize
    buddy.free = true
    buddy.next = Block()
    buddy.previous = block

    # Tack this onto the end of the next level freelist
    return block, buddy
end

# Pop a block off the free list for the given bin.
# If no blocks are available, will recurse one level up and split.
function pop_freelist!(heap::BuddyHeap, bin)
    block = heap.freelists[bin]

    # If we've reached the top of the chain and we have no blocks available,
    # we've reached a dead end!
    if bin == numbins(heap) && isnull(block)
        return nothing
    end

    if isnull(block)
        # Could use recursion, but do this to avoid unnecessary stack growth.
        # Find the first non-null block
        range = bin+1:numbins(heap)
        i = findfirst(x -> !isnull(heap.freelists[x]), range)
        isnothing(i) && return nothing
        index = range[i]

        block = pop_freelist!(heap, index)
        local buddy
        for _ in bin+1:index
            block, buddy = split!(heap, block)
            push_freelist!(heap, buddy)
        end
        block.next = buddy
    end

    # Okay, now we have a block, simply clear out its successor and return it.
    if !isnull(block.next)
        block.next.previous = Block()
    end
    heap.freelists[bin] = block.next

    return block
end

function putback!(heap::BuddyHeap, block::Block)
    @check isfree(block)

    # Check if the buddy for this block is free.
    # If so, coaslesce the two of them together.
    buddy = getbuddy(heap, block)
    while !isnothing(buddy) && isfree(buddy) && (buddy.size == block.size)
        # Coalesce the two of them together.
        remove!(heap, buddy)

        block = (block < buddy) ? block : buddy
        block.size = block.size << 1

        # Check to see if we can go another iteration.
        buddy = getbuddy(heap, block)
    end
    push_freelist!(heap, block)

    return nothing
end

#####
##### High Level API
#####

function alloc(heap::BuddyHeap, bytes::Integer, id = nothing)
    iszero(bytes) && return nothing

    # Determine what bin this belongs to.
    # Make sure we include the sikkkkze of the header in this computation.
    bin = getbin(max(bytes + headersize(), MIN_ALLOCATION))
    block = pop_freelist!(heap, bin)
    if isnothing(block)
        return nothing
    else
        # Mark this block as used
        block.free = false
        block.evicting = false
        !isnothing(id) && (block.id = id)

        # Configure metadata
        # Set the pool number and zero out the sibling block.
        block.pool = heap.pool

        # Update statistics
        heap.allocations += 1
        heap.allocated += binsize(bin)

        ptr = pointer(block) + headersize()
        return ptr
    end
end

free(heap::BuddyHeap, ptr::Ptr) = free(heap, convert(Ptr{Nothing}, ptr))
function free(heap::BuddyHeap, ptr::Ptr{Nothing})
    # Get the block from the pointer
    block = unsafe_block(ptr)

    # Update stats
    heap.allocations -= 1
    heap.allocated -= block.size

    # This block is being evicted - bypass returning the block.
    block.evicting && return nothing

    block.free = true
    @check !iszero(block.size)
    # Put that thing back where it came from, or so help me!
    putback!(heap, block)
    return nothing
end

###
### 2LM Hack
###

@static if IS_2LM
    function alloc(heap::BuddyHeap{MemKindAllocator}, bytes::Integer, id = nothing)
        error("Trying to allocate from remote heap in 2LM!")
    end
end # @static if

#####
##### Utilities for eviction
#####

# Return `true` if we will be able to allocate a block of size `sz` by successively
# freeing larger buddy blocks starting at `block`.
function canallocfrom(heap::BuddyHeap, block::Block, sz)
    bin = getbin(sz + headersize())
    bin > numbins(heap) && return false
    bsz = binsize(bin)
    # Round up to the nearest multple of bsz
    md = mod(address(block) - baseaddress(heap), bsz)

    # If, starting at this address, we go down to the nearest block boundary that
    # can contain this block, go up by the size, and still remain in the heap,
    # then we can allocated from here.
    return address(block) - md + bsz <= baseaddress(heap) + sizeof(heap)
end

@inline function evict!(heap::BuddyHeap, block::Block; cb = donothing)
    @check !isfree(block)

    # Mark this block as being evicted.
    # This will ensure that any calls to `free` on this block that happen during the
    # callback `cb` will short-circuit.
    block.evicting = true

    # Perfrom the callback on the block ID.
    # After the call back, assume that the block is now ours.
    cb(block)
    block.free = true
    return nothing
end

# Work our way up the
function evictfrom!(heap::BuddyHeap, block::Block, sz; cb = donothing)
    bsz = binsize(getbin(sz + headersize()))
    # Find the base block for this future allocation.
    # We walk the heap through this block until we've freed everything.
    start = Block(address(block) - mod(address(block) - baseaddress(heap), bsz))
    stopaddress = address(start) + bsz

    # Can be equal if we're evicting a block that's the same size as the one we're trying
    # to put in.
    @check address(start) >= baseaddress(heap)
    @check start.size <= bsz

    current = start
    while address(current) < stopaddress
        # If this block is free, remove it from the freelist.
        # Otherwise, perfrom an eviction.
        isfree(current) ? remove!(heap, current) : evict!(heap, current; cb = cb)
        current = Block(address(current) + current.size)
    end

    # We should definitely end on a boundary.
    @check address(current) == stopaddress

    # Resize the start block and return it to the heap.
    start.size = bsz
    start.free = true
    putback!(heap, start)
    return nothing
end

#####
##### Utility Checking Functions.
#####

function freelist_length(heap::BuddyHeap, bin)
    block = heap.freelists[bin]
    count = 0
    while !isnull(block)
        count += 1
        block = block.next
    end
    return count
end

function zerocheck(heap::BuddyHeap, verbose = false)
    for (i, block) in enumerate(heap)
        if verbose
            println("Block: $(pointer(block))")
            println("Size: $(block.size)")
        end
        if block.size == 0
            println("Block: $(pointer(block))")
            println("Size: $(block.size)")
            println()
            error("Block $i has Zero Length!")
        end
    end
    return true
end

# Walk through the heap.
# If a block is free, its buddy should NOT be free.
# If they are both free, then they should have been cleaned up.
function buddycheck(heap::BuddyHeap)
    for block in heap
        if isfree(block)
            buddy = getbuddy(heap, block)
            if !isnothing(buddy) && (buddy.size == block.size) && isfree(buddy)
                return false
            end
        end
    end
    return true
end

function countcheck(heap::BuddyHeap)
    # Count the length of each free list, then walk the heap, counting the number of free
    # blocks in each bin.
    #
    # The numbers should match.
    passed = true
    counts = [0 for _ in 1:numbins(heap)]
    for block in heap
        if isfree(block)
            counts[getbin(block)] += 1
        end
    end

    for bin in 1:numbins(heap)
        len = freelist_length(heap, bin)
        if len != counts[bin]
            println("Failed for bin $bin")
            println("Counted: $(counts[bin])")
            println("Found: $len")
            passed = false
        end
    end
    return passed
end

function sizecheck(heap::BuddyHeap)
    sz = 0
    for block in heap
        sz += block.size
    end
    return sz == sizeof(heap)
end

function check(heap::BuddyHeap)
    zerocheck(heap)

    passed = true
    if !buddycheck(heap)
        printstyled(stdout, "BuddyHeap Failed Buddy Check\n"; color = :red)
        passed = false
    end

    if !countcheck(heap)
        printstyled(stdout, "BuddyHeap Failed Count Check\n"; color = :red)
        passed = false
    end

    if !sizecheck(heap)
        printstyled(stdout, "BuddyHeap Failed Size Check\n"; color = :red)
        passed = false
    end

    return passed
end
