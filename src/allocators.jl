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

#####
##### NUMA Allocator
#####

const NUMA_ALLOCATED_SIZES = Dict{Ptr{Nothing},Integer}()

struct NUMAAllocator <: AbstractAllocator
    nodeid::Integer
end

allocate(a::NUMAAllocator, sz) = numa_alloc(sz; nodeid = a.nodeid)
free(::NUMAAllocator, ptr::Ptr{Nothing}, sz::Integer) = numa_free(ptr, sz)

# Allocates whole pages (4096 bytes).
# NOTE: Apparently much slower than malloc. Faster alternatives did not
#       provide ability to allocate on specific nodes.
function numa_alloc(sz::Integer; alignment = UInt(64), nodeid::Integer = 0)
    # Need to round up size of allocation for alignment purposes
    obytes = sz % alignment
    if obytes > 0
        sz += alignment - obytes
    end

    ptr = Ptr{Nothing}()
    ret = ccall(
        (
            :numa_alloc_onnode,
            dlpath("libnuma")
        ),
        Ptr{Cvoid},
        (Csize_t, Cint),
        sz,
        nodeid
    )
    @assert ret == 0

    ptr = ret
    push!(NUMA_ALLOCATED_SIZES, ptr => sz)
    return ptr
end

function numa_free(ptr::Ptr{Nothing}, sz::Integer)
    pop!(NUMA_ALLOCATED_SIZES, ptr)
    ccall(
        (
            :numa_free,
            dlpath("libnuma")
        ),
        Cvoid,
        (Ptr{Cvoid}, Csize_t),
        ptr,
        sz
    )
end
