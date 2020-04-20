# A buddy-based heap allocator.
# All allocations supplied by this allocator are aligned to 64B
# We use a 64B header and footer to store metadata.
#
# TODO: Reimplement recursion in places with normal iteration to avoid destroying
# the stack when using a large number of buckets.
isnull(x::Ptr) = isnull(convert(UInt, x))
isnull(x::UInt) = iszero(x)

#####
##### Tunable Parameters
#####

# The minimum granularity that we allocate at.
const MIN_ALLOCATION = 4096
const LOG2_MIN_ALLOCATION = ceil(Int, log2(MIN_ALLOCATION))

getbin(sz) = ceil(Int, log2(sz)) - LOG2_MIN_ALLOCATION + 1
binsize(i) = MIN_ALLOCATION * 2^(i-1)

struct Block
    ptr::Ptr{Nothing}
end
Block() = Block(Ptr{Nothing}(0))
Block(address::UInt) = Block(Ptr{Nothing}(address))

Base.pointer(x::Block) = getfield(x, :ptr)
isnull(x::Block) = isnull(x.ptr)
Base.isless(a::Block, b::Block) = a.ptr < b.ptr
getbin(block::Block) = getbin(block.size)

# Reserved room in for each allocation.
headersize() = 64

function Base.getproperty(x::Block, sym::Symbol)
    if sym == :ptr
        return pointer(x)
    elseif sym == :size
        # Load the first 8 bytes which encodes the size as a UInt64
        # Mask out the lower 6 bits since those are reserved for other metadata.
        sz = unsafe_load(convert(Ptr{UInt64}, pointer(x)))
        return sz & ~UInt(0x3F)
    elseif sym == :next
        return unsafe_load(convert(Ptr{Block}, pointer(x) + 8))
    elseif sym == :previous
        return unsafe_load(convert(Ptr{Block}, pointer(x) + 16))

    # Bitmask metadata
    elseif sym == :bitmasks
        return unsafe_load(convert(Ptr{UInt64}, pointer(x))) & UInt(0x3F)
    elseif sym == :free
        sz = unsafe_load(convert(Ptr{UInt64}, pointer(x)))
        return Bool(sz & UInt(0x1))
    else
        error()
    end
end

function Base.setproperty!(x::Block, name::Symbol, v)
    if name == :size
        sz = convert(UInt64, v) & ~UInt64(0x3f)
        unsafe_store!(convert(Ptr{UInt64}, pointer(x)), sz | x.bitmasks)
    elseif name == :next
        unsafe_store!(convert(Ptr{Block}, pointer(x) + 8), v::Block)
    elseif name == :previous
        unsafe_store!(convert(Ptr{Block}, pointer(x) + 16), v::Block)
    elseif name == :free
        v::Bool
        _ptr = convert(Ptr{UInt8}, pointer(x))
        unsafe_store!(_ptr, (unsafe_load(_ptr) & ~(UInt8(1))) | UInt8(v))
    else
        error()
    end
end

# Use the lower most bit to indicate if this block is free.
isfree(x::Block) = x.free
address(x::Block) = convert(UInt, pointer(x))

# Get the buddy block for a given header.
# Ptr is a pointer to the block that `header` belongs to.
function getbuddy(heap, block::Block)
    base = baseaddress(heap)
    offset = address(block) - base
    shift = xor(block.size, offset)
    buddy_address = shift + base

    # If the buddy address is out of range, this block does not have a buddy.
    if buddy_address + block.size > base + heap.len
        return nothing
    else
        return Block(shift + base)
    end
end

mutable struct Heap{T}
    allocator::T

    # The base pointer for the heap we're managing.
    base::Ptr{Nothing}
    len::Int

    # The start to blocks, indexed by their size in the buddy system.
    freelists::Vector{Block}
end

baseaddress(heap::Heap) = convert(UInt, heap.base)
numbins(heap::Heap) = length(heap.freelists)

function Heap(allocator::T, sz) where {T}
    # Make an initial allocation of `sz`.
    base = allocate(allocator, sz)

    # Initialize the freelists.
    # First - figure out how many bins we need.
    maxbin = getbin(sz)
    @show maxbin
    freelists = [Block() for _ in 1:maxbin]

    heap = Heap{T}(
        allocator,
        base,
        sz,
        freelists,
    )

    finalizer(heap) do x
        free(x.allocator, x.base)
    end

    # Tile the free space.
    size_left = sz
    ptr = base
    currentsize = binsize(maxbin)
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
    return heap
end

# Implement a function to iterate through the whole heap.
function Base.iterate(heap)
    block = Block(heap.base)
    return (block, block)
end
function Base.iterate(heap, block::Block)
    if pointer(block) + block.size >= heap.base + heap.len
        return nothing
    end
    block = Block(pointer(block) + block.size)
    return (block, block)
end

function slowlength(heap::Heap)
    count = 0
    for block in heap
        count += 1
    end
    return count
end

# Push an item onto the free list.
function push_freelist!(heap::Heap, block::Block)
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

function split!(heap::Heap, block::Block)
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
function pop_freelist!(heap::Heap, bin)
    block = heap.freelists[bin]

    # If we've reached the top of the chain and we have no blocks available,
    # we've reached a dead end!
    if bin == numbins(heap) && isnull(block)
        return nothing
    end

    # Will recurse up, splitting blocks as necessary.
    if isnull(block)
        next = pop_freelist!(heap, bin + 1)
        isnothing(next) && return nothing
        block, buddy = split!(heap, next)

        push_freelist!(heap, buddy)
        block.next = buddy
    end

    # Okay, now we have a block, simply clear out its successor and return it.
    if !isnull(block.next)
        block.next.previous = Block()
    end
    heap.freelists[bin] = block.next

    return block
end

function putback!(heap::Heap, block::Block)
    @check isfree(block)

    # Check if the buddy for this block is free.
    # If so, coaslesce the two of them together.
    buddy = getbuddy(heap, block)

    if !isnothing(buddy) && isfree(buddy) && (buddy.size == block.size)
        # Coalesce the two of them together.
        remove!(heap, buddy)

        # Determine which is at the lower address.
        # that is the base.
        base = (block < buddy) ? block : buddy
        base.size = base.size << 1
        putback!(heap, base)
    else
        push_freelist!(heap, block)
    end
    return nothing
end

#####
##### High Level API
#####

function alloc(heap::Heap, bytes::Integer)
    iszero(bytes) && return nothing

    # Determine what bin this belongs to.
    # Make sure we include the size of the header in this computation.
    bin = getbin(max(bytes, MIN_ALLOCATION) + headersize())
    block = pop_freelist!(heap, bin)
    if isnothing(block)
        return nothing
    else
        # Mark this block as used
        block.free = false
        return pointer(block) + headersize()
    end
end

free(heap::Heap, ptr::Ptr) = free(heap, convert(Ptr{Nothing}, ptr))
function free(heap::Heap, ptr::Ptr{Nothing})
    # Get the block from the pointer
    block = Block(ptr - headersize())
    block.free = true
    # Put that thing back where it came from, or so help me!
    putback!(heap, block)
    return nothing
end

#####
##### Utility Checking Functions.
#####

function freelist_length(heap::Heap, bin)
    block = heap.freelists[bin]
    count = 0
    while !isnull(block)
        count += 1
        block = block.next
    end
    return count
end

function zerocheck(heap::Heap)
    for (i, block) in enumerate(heap)
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
function buddycheck(heap::Heap)
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

function countcheck(heap::Heap)
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

function sizecheck(heap::Heap)
    sz = 0
    for block in heap
        sz += block.size
    end
    return sz == heap.len
end

function check(heap::Heap)
    zerocheck(heap)

    passed = true
    if !buddycheck(heap)
        printstyled(stdout, "Heap Failed Buddy Check\n"; color = :red)
        passed = false
    end

    if !countcheck(heap)
        printstyled(stdout, "Heap Failed Count Check\n"; color = :red)
        passed = false
    end

    if !sizecheck(heap)
        printstyled(stdout, "Heap Failed Size Check\n"; color = :red)
        passed = false
    end

    return passed
end
