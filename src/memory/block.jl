#####
##### This is the header for a region of memory in the heap.
#####

@enum Pool::UInt DRAM=0 PMM=1
struct PoolType{T} end

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
#       We can save the lower 6 bits because we assume sizes are always multiples of 64 bytes.
#       TODO: We can actually do better since sizes will be multiples of MIN_ALLOCATION.
#
#       bit 0: 1 if the block is free, 0 if it is not free.
#       bit 1-2: Pool ID: 00 if DRAM, 01 if PMM. These bits correspond to the `Pool` enum.
#       bit 3: dirty bit: 0 if clean, 1 if dirty
#       bit 4: evicting bit: 0 if normal free, 1 if being evicted
#       big 5: queued bit: 1 if queued for freeing, 0 otherwise
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
#    if block is not free, this is a pointer to the sibling array.
#    access with 'block.sibling'.
#    If 'isnull(block.sibling)', then the sibling does not exist.
#    A sibling is a copy of the array that lives in another memory pool.
#
# bytes 31..24
#    the size of the block before this block.
#    used to walk backward through the heap/

struct Block <: AbstractMetadata
    ptr::Ptr{Nothing}
    # Inner constructor for ambiguity resolution
    Block(ptr::Ptr{Nothing}) = new(ptr)
end

Block() = Block(Ptr{Nothing}(0))
Block(ptr::Ptr) = Block(convert(Ptr{Nothing}, ptr))
Block(address::UInt) = Block(Ptr{Nothing}(address))

Base.length(block) = block.size - headersize()

Base.pointer(x::Block) = getfield(x, :ptr)
datapointer(x::Block) = pointer(x) + headersize()
isnull(x::Block) = isnull(x.ptr)
Base.isless(a::Block, b::Block) = a.ptr < b.ptr
address(x::Block) = convert(UInt, pointer(x))

unsafe_block(ptr::Ptr) = Block(convert(Ptr{Nothing}, ptr) - headersize())

struct BlockMeta end
metadata(x, ::BlockMeta) = unsafe_block(pointer(x))

# Reserved room in for each allocation.
headersize() = 64

mask(x) = one(x) << x
mask(x, y...) = mask(x) | mask(y...)

macro getbits(typ, ptr, x::Integer...)
    # Precomput the mask and minimum value
    msk = mask(x...)
    min = minimum(x)
    return :((unsafe_load(convert(Ptr{$typ}, $(esc(ptr)))) & $msk) >> $min)
end

macro setbits!(typ, ptr, v, x::Integer...)
    load_clear_mask = ~mask(x...)
    min = minimum(x)
    set_clear_mask = mask((x .- min)...)
    return quote
        cptr = convert(Ptr{$typ}, $(esc(ptr)))
        y = unsafe_load(cptr) & $load_clear_mask
        y |= ($(esc(v)) & $set_clear_mask) << $min
        unsafe_store!(cptr, y)
    end
end

function Base.getproperty(x::Block, name::Symbol)
    if name == :ptr
        return pointer(x)
    elseif name == :size
        # Load the first 8 bytes which encodes the size as a UInt64
        # Mask out the lower 6 bits since those are reserved for other metadata.
        return unsafe_load(convert(Ptr{UInt64}, pointer(x))) & ~UInt(0x7F)

    # Bitmask metadata
    elseif name == :bitmasks
        return unsafe_load(convert(Ptr{UInt64}, pointer(x))) & UInt(0x7F)
    elseif name == :free
        return Bool(@getbits(UInt, pointer(x), 0))
    elseif name == :pool
        return Pool(@getbits(UInt, pointer(x), 1, 2))
    elseif name == :dirty
        return Bool(@getbits(UInt, pointer(x), 3))
    elseif name == :evicting
        return Bool(@getbits(UInt, pointer(x), 4))
    elseif name == :queued
        return Bool(@getbits(UInt, pointer(x), 5))
    elseif name == :reclaimed
        return Bool(@getbits(UInt, pointer(x), 6))

    # Bytes 15..8
    elseif name == :next
        return unsafe_load(convert(Ptr{Block}, pointer(x) + 8))

    elseif name == :id
        return unsafe_load(convert(Ptr{UInt64}, pointer(x) + 8))

    # Bytes 23..16
    elseif name == :previous || name == :sibling
        return unsafe_load(convert(Ptr{Block}, pointer(x) + 16))

    # Bytes 31..24
    elseif name == :backsize
        return unsafe_load(convert(Ptr{UInt64}, pointer(x) + 24))

    else
        error()
    end
end

function Base.setproperty!(x::Block, name::Symbol, v)
    # Bytes 7..0
    if name == :size
        sz = convert(UInt64, v) & ~UInt64(0x7f)
        unsafe_store!(convert(Ptr{UInt64}, pointer(x)), sz | x.bitmasks)
    # Bit masks
    elseif name == :free
        @setbits!(UInt8, pointer(x), UInt8(v::Bool), 0)
    elseif name == :pool
        @setbits!(UInt8, pointer(x), UInt8(v::Pool), 1, 2)
    elseif name == :dirty
        @setbits!(UInt8, pointer(x), UInt8(v::Bool), 3)
    elseif name == :evicting
        @setbits!(UInt8, pointer(x), UInt8(v::Bool), 4)
    elseif name == :queued
        @setbits!(UInt8, pointer(x), UInt8(v::Bool), 5)
    elseif name == :reclaimed
        @setbits!(UInt8, pointer(x), UInt8(v::Bool), 6)

    # bytes 15..8
    elseif name == :next
        unsafe_store!(convert(Ptr{Block}, pointer(x) + 8), v::Block)

    elseif name == :id
        unsafe_store!(convert(Ptr{UInt64}, pointer(x) + 8), convert(UInt64, v))

    # bytes 23..16
    elseif name == :previous || name == :sibling
        unsafe_store!(convert(Ptr{Block}, pointer(x) + 16), v::Block)

    # bytes 31..24
    elseif name == :backsize
        unsafe_store!(convert(Ptr{UInt64}, pointer(x) + 24), convert(UInt64, v))

    else
        error("Unknown field: $name")
    end
end

isfree(x::Block) = x.free

#####
##### Metadata API
#####

setdirty!(x::Block, flag::Bool = true) = (x.dirty = flag)
isdirty(x::Block) = x.dirty
getid(x::Block) = x.id
getpool(x::Block) = x.pool

function getsibling(x::Block)
    sibling = x.sibling
    return isnull(sibling) ? nothing : sibling
end

setsibling!(x::Block, y::Block) = (x.sibling = y)

@inline markqueued!(x::Block) = (x.queued = true)
@inline isqueued(x::Block) = x.queued

#####
##### Display
#####

function Base.show(io::IO, block::Block)
    if isnull(block)
        println(io, "Null Block")
        return nothing
    end

    println(io, "Block")
    println(io, "    Address: 0x", string(address(block); base = 16))
    println(io, "    Size: ", block.size)
    println(io, "    Backsize: ", block.backsize)
    print(io, "    Availability: ")

    # Display the free attributes of the block
    if isfree(block)
        printstyled(io, "Free\n"; color = :green)
        println(io, "    Pool: ", block.pool)
        println(io, "    Next: 0x", string(address(block.next); base = 16))
        println(io, "    Previous: 0x", string(address(block.previous); base = 16))
    else
        printstyled(io, "Taken\n"; color = :red)
        println(io, "    Pool: ", block.pool)
        println(io, "    ID: ", block.id)
        println(io, "    Sibling: 0x", string(address(block.sibling); base = 16))
    end
end
