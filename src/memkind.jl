module MemKind

using Libdl

# Setup paths
const SRCDIR = @__DIR__
const PKGDIR = dirname(@__DIR__)
const DEPSDIR = joinpath(PKGDIR, "deps")
const USRDIR = joinpath(DEPSDIR, "usr")
const LIBDIR = joinpath(USRDIR, "lib")

const libmemkind = joinpath(LIBDIR, "libmemkind.so")

# Open up the library
function __init__()
    global libmemkind
    Libdl.dlopen(libmemkind, Libdl.RTLD_GLOBAL)
end

# Wrapper for MemKind Memory Kinds
#
# The default ones are simply global pointers to the preallocated structs.
mutable struct Kind
    ptr::Ptr{Nothing}
end
Base.pointer(x::Kind) = x.ptr

# Bare-bones malloc
function malloc(kind::Kind, size)
    ptr = ccall(
        (:memkind_malloc, libmemkind),
        Ptr{Cvoid},
        (Ptr{Cvoid}, Csize_t),
        pointer(kind),
        size,
    )

    if ptr == C_NULL
        throw(error(ptr))
    end
    return ptr
end

# bare-bones free
free(kind::Kind, ptr::Ptr) = free(kind, convert(Ptr{Nothing}, ptr))
function free(kind::Kind, ptr::Ptr{Nothing})
    ccall((:memkind_free, libmemkind), Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}), pointer(kind), ptr)
    return nothing
end

# defrag
function defrag(kind::Kind, ptr::Ptr{Cvoid})
    return ccall((:memkind_defrag_realloc, libmemkind), Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}), poiner(kind), ptr)
end

# Create pmem
function create_pmem(dir::AbstractString, max_size)
    kind = Ref(Ptr{Cvoid}(0))
    val = ccall(
        (:memkind_create_pmem, libmemkind),
        Cint,
        (Cstring, Csize_t, Ptr{Ptr{Cvoid}}),
        dir,
        max_size,
        kind,
    )

    if val != 0
        throw(error(val))
    end
    obj = Kind(kind[])
    finalizer(destroy, obj)
    return obj
end

function destroy(kind::Kind)
    val = ccall((:memkind_destroy_kind, libmemkind), Cint, (Ptr{Cvoid},), pointer(kind))
    if val != 0
        throw(error(val))
    end
    return nothing
end

end # module
