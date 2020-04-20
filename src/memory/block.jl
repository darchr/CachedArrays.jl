#####
##### This is the header for a region of memory in the heap.
#####

# It is imlemented as just a pointer, which points to the beginning of a region of memory.
# However, we reserve the first 64 bytes (cache line) for use by the header.
#
# These fields are accessible using `getproperty` and `setproperty` overloading.
#
# The layout looks like this
#
# bytes 7..0
#    The top 58 bits describe the size of the memory for this block
#    The lower 6 bits are reserved for misc metadata.
#       We can save the lower 6 bites because we assume sizes are always multiples of 64 bytes.
#       TODO: We can actually do better since sizes will be multiples of MIN_ALLOCATION.
#
#       bit 0: 1 if the block is free, 0 if it is not free.
#       bit 1: Pool ID: 0 if local, 1 if remote.
#
#    Size can be obtained with `block.size`.
#    The free state is obtained with `block.free`.
#    All of the lower 6 bits are obtained with `block.bitmasks`.
#
# bytes 15..8
#    if the block is free, this is a pointer to the next block in the freelist.
#    Access with `block.next`.
#
#    If the block is NOT free, this is the ID, of the owner of the block.
#    Access this with `block.id`
#
# bytes 23..16
#    if the block is free, this is a pointer to the previous block in the freelist.
#    access with `block.previous`.
#
#    if block is not free, this is a pointer to the parent array.
#    access with 'block.parent'.
#    If 'isnull(block.parent)', then the parent does not exist.
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

    # Bitmask metadata
    elseif sym == :bitmasks
        return unsafe_load(convert(Ptr{UInt64}, pointer(x))) & UInt(0x3F)
    elseif sym == :free
        sz = unsafe_load(convert(Ptr{UInt64}, pointer(x)))
        return Bool(sz & UInt(0x1))
    elseif sym == :pool
        sz = unsafe_load(convert(Ptr{UInt64}, pointer(x)))
        return (sz & UInt(0x10)) >> 1

    # Bytes 15..8
    elseif sym == :next
        return unsafe_load(convert(Ptr{Block}, pointer(x) + 8))

    elseif sym == :id
        return unsafe_load(convert(Ptr{UInt64}, pointer(x) + 8))

    # Bytes 23..16
    elseif sym == :previous
        return unsafe_load(convert(Ptr{Block}, pointer(x) + 16))

    else
        error()
    end
end

function Base.setproperty!(x::Block, name::Symbol, v)
    # Bytes 7..0
    if name == :size
        sz = convert(UInt64, v) & ~UInt64(0x3f)
        unsafe_store!(convert(Ptr{UInt64}, pointer(x)), sz | x.bitmasks)
    elseif name == :free
        v::Bool
        _ptr = convert(Ptr{UInt8}, pointer(x))
        unsafe_store!(_ptr, (unsafe_load(_ptr) & ~(UInt8(1))) | UInt8(v))

    # bytes 15..8
    elseif name == :next
        unsafe_store!(convert(Ptr{Block}, pointer(x) + 8), v::Block)

    elseif name == :id
        unsafe_store!(convert(Ptr{UInt64}, pointer(x) + 8), convert(UInt64, v))

    # bytes 23..16
    elseif name == :previous
        unsafe_store!(convert(Ptr{Block}, pointer(x) + 16), v::Block)
    else
        error()
    end
end

# Use the lower most bit to indicate if this block is free.
isfree(x::Block) = x.free
address(x::Block) = convert(UInt, pointer(x))
