# Bottom Allocator types.
abstract type AbstractAllocator end

#####
##### Allocate through LibMemKind
#####

struct MemKindAllocator <: AbstractAllocator
    kind::MemKind.Kind
end

# Determine what to do based on environment variables.
# This is a little hacky ... but should work for now.
#
# in 2LM, we defer to `AlignedAllocator` to bootstrap the remote heap.
# However, we extend the `alloc` method for the heap to throw an error in 2LM.
@static if IS_2LM
    allocate(A::MemKindAllocator, sz) = allocate(AlignedAllocator(), sz)
    free(A::MemKindAllocator, ptr::Ptr{Nothing}) = free(AlignedAllocator(), ptr)
else
    allocate(A::MemKindAllocator, sz) = MemKind.malloc(A.kind, sz)
    free(A::MemKindAllocator, ptr::Ptr{Nothing}) = MemKind.free(A.kind, ptr)
end # @static if

#####
##### Allocate through LibC
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

