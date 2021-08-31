# Bottom Allocator types.
abstract type AbstractAllocator end

#####
##### Memory Map Allocator
#####

struct MmapAllocator <: AbstractAllocator
    dir::String
end

const MMAP_ALLOCATED_ARRAYS = Dict{Ptr{Nothing},Vector{UInt8}}()

function allocate(allocator::MmapAllocator, sz)
    name = tempname(allocator.dir; cleanup = false)
    A = open(name; create = true, read = true, write = true) do io
        Mmap.mmap(io, Vector{UInt8}, sz)
    end
    ptr = Ptr{Nothing}(pointer(A))
    MMAP_ALLOCATED_ARRAYS[ptr] = A
    # Remove the file - OS will keep it around as long as our process is running and the
    # cleanup once the process terminates.
    rm(name)
    return ptr
end

# Let the Mmap finalizer do its thing
free(::MmapAllocator, ptr::Ptr) = delete!(MMAP_ALLOCATED_ARRAYS, ptr)

#####
##### Persistent Mmap Allocator
#####

struct PersistentMmapAllocator <: AbstractAllocator
    path::String
end

function allocate(allocator::PersistentMmapAllocator, sz)
    path = allocator.path
    A = open(path; create = true, read = true, write = true) do io
        Mmap.mmap(io, Vector{UInt8}, sz)
    end
    ptr = Ptr{Nothing}(pointer(A))
    MMAP_ALLOCATED_ARRAYS[ptr] = A
    return ptr
end

free(::PersistentMmapAllocator, ptr::Ptr) = delete!(MMAP_ALLOCATED_ARRAYS, ptr)

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
