# Bottom Allocator types.
abstract type AbstractAllocator end

#####
##### Memory Map Allocator
#####

struct MmapAllocator <: AbstractAllocator
    dir::String
end

function allocate(allocator::MmapAllocator, sz)
    name = tempname(allocator.dir; cleanup = false)
    A = open(name; create = true, read = true, write = true) do io
        Mmap.mmap(io, Vector{UInt8}, sz)
    end
    # Remove the file - OS will keep it around as long as our process is running and the
    # cleanup once the process terminates.
    rm(name)
    return A
end

# Let the Mmap finalizer do its thing
function free(::MmapAllocator, _)
    return nothing
end

#####
##### DRAM Allocator
#####

struct AlignedAllocator <: AbstractAllocator end

allocate(::AlignedAllocator, sz) = aligned_alloc(sz)
free(::AlignedAllocator, ptr::Ptr{Nothing}) = Libc.free(ptr)

function aligned_alloc(sz::Integer; alignment = UInt(64))
    ptr_ref = Ref(Ptr{Nothing}())
    ret = ccall(
        :posix_memalign,
        Cint,
        (Ptr{Ptr{Cvoid}}, Csize_t, Csize_t),
        ptr_ref,
        alignment,
        sz
    )
    @assert ret == 0
    return ptr_ref[]
end
