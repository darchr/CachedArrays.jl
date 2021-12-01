"""
    AbstractAllocator

Types implementing the `AbstractAllocator` interface need to define the following two
methods:

    allocate(allocator, bytes::Integer) -> (Ptr{Nothing}, Token)

Allocate `bytes` from `allocator`. Return type is a a tuple whose first element is a
pointer with type `Ptr{Nothing}` to the beginning of the allocated data and whose second
element is some token that can be passed to `free`.

    free(ptr::Ptr{Nothing}, token)

Complement to `allocate`: Takes the returned pointer and token and takes any steps
necessary to free the data.

In otherwords, the following should and clean up all allocated resources:

    free(allocate(allocator, bytes)...)


Example
=======
```julia-repl
julia> allocator = CachedArrays.AlignedAllocator()
CachedArrays.AlignedAllocator()

julia> ptr, token = CachedArrays.allocate(allocator, 100)
(Ptr{Nothing} @0x00005587e09cdf40, CachedArrays.AlignedAllocator())

julia> CachedArrays.free(ptr, token)
```

Note: If `token` will automatically handle resource reclamation using a finalizer, then
the type `NoopWrapper` can be wrapped around the token to turn `free` into a no-op.
"""
abstract type AbstractAllocator end

struct NoopWrapper{T}
    data::T
end
free(::Ptr{Nothing}, ::NoopWrapper) = nothing

#####
##### Memory Map Allocator
#####

struct MmapAllocator <: AbstractAllocator
    path::String
    persistent::Bool
end

MmapAllocator(dir::AbstractString) = MmapAllocator(dir, false)
PersistentMmapAllocator(path::AbstractString) = MmapAllocator(path, true)

function allocate(allocator::MmapAllocator, sz)
    # Decide if we're going to create a temporary mapping or use an existing mapping.
    path = allocator.path
    path = allocator.persistent ? path : tempname(path; cleanup = false)
    A = open(path; create = true, read = true, write = true) do io
        Mmap.mmap(io, Vector{UInt8}, sz)
    end

    # Remove the file - OS will keep it around as long as our process is running and
    # the cleanup once the process terminates.
    allocator.persistent || rm(path)
    ptr = Ptr{Nothing}(pointer(A))
    return ptr, NoopWrapper(A)
end

#####
##### DRAM Allocator
#####

struct AlignedAllocator <: AbstractAllocator end

allocate(::AlignedAllocator, sz) = aligned_alloc(sz)
free(ptr::Ptr{Nothing}, ::AlignedAllocator) = Libc.free(ptr)

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
    return ptr_ref[], AlignedAllocator()
end
