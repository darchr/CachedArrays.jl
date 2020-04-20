# Bottom Allocator types.
abstract type  AbstractAllocator end

#####
##### Allocate through LibMemKind
#####

struct MemKindAllocator <: AbstractAllocator
    kind::MemKind.Kind
end

allocate(A::MemKindAllocator, sz) = MemKind.malloc(A.kind, sz)
free(A::MemKindAllocator, ptr::Ptr{Nothing}) = MemKind.free(A.kind, ptr)

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

