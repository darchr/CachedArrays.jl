# # Hooks into persistent memory.
# module PmemObj
#
# using Libdl
#
# # Setup paths
# const SRCDIR = @__DIR__
# const PKGDIR = dirname(@__DIR__)
# const DEPSDIR = joinpath(PKGDIR, "deps")
# const USRDIR = joinpath(DEPSDIR, "usr")
# const LIBDIR = joinpath(USRDIR, "lib")
#
# const libpmem = joinpath(LIBDIR, "libpmem.so")
# const libpmemobj = joinpath(LIBDIR, "libpmemobj.so")
#
# # Versioning
# const VERSION_MAJOR = 2
# const VERSION_MINOR = 4
#
# # Open up the library
# function __init__()
#     global libpmem
#     global libpmemobj
#     Libdl.dlopen(libpmem, Libdl.RTLD_GLOBAL)
#     Libdl.dlopen(libpmemobj, Libdl.RTLD_GLOBAL)
#
#     # Perform a version check - also makes sure that we can actually load the library
#     # correctly.
#     check_version(VERSION_MAJOR, VERSION_MINOR)
# end
#
# function check_version(major, minor)
#     result = ccall((:pmemobj_check_version, libpmemobj), Cstring, (Cint, Cint), major, minor)
#     if result != C_NULL
#         error(unsafe_string(result))
#     end
#     return nothing
# end
#
# # Pool for allocation.
# struct ObjectPool
#     ptr::Ptr{Nothing}
# end
# isnull(pool::ObjectPool) = (pool.ptr == C_NULL)
#
# # This struct shadows the `PMEMoid` object in `include/libpmemobj/base.h`
# #
# # Here, `T` references the Julia type that this points to.
# struct PersistentOID{T}
#     pool_uuid_lo::UInt64
#     off::UInt64
# end
# isnull(x::PersistentOID) = iszero(x.pool_uuid_lo) && iszero(x.off)
#
# #####
# ##### Pool Functions
# #####
#
# """
#     close(pool::ObjectPool)
#
# Close the memory pool indicated by `pool` and delete the memory pool handle.
# The object store itself lives on in the file that contains it and may be reopened at a
# later time using [`open`](@ref).
# """
# close(pool::ObjectPool) = ccall((:pmemobj_close, libpmemobj), Cvoid, (Ptr{Nothing},), pool.ptr)
#
# """
#     create(path, poolsize; layout = "", mode = UInt32(0o666) -> ObjectPool
#
# Create a transactional object store with the given total `poolsize` in bytes. Argument
# `path` specifies the name of the memory pool file to be created. Argument `layout` specifies
# the application's layout type in the form of a string. The layout name is not interpreted by
# `libpmemobj`, but may be used as a check when [`pmemobj_open`](@ref`) is called.
# """
# function create(path, poolsize; layout = "", mode = UInt32(0o0666))
#     ptr = ccall(
#         (:pmemobj_create, libpmemobj),
#         Ptr{Nothing},
#         (Cstring, Cstring, Csize_t, Base.Cmode_t),
#         path, layout, poolsize, mode
#     )
#
#     return ObjectPool(ptr)
# end
#
# """
#     open(path; layout = "") -> ObjectPool
#
# Open an existing object store memory pool. Similar to [`create`](@ref), `path` must identify
# either an existing object memory pool file.
# Return an handle `ObjectPool`. If allocation fails, returns a null `ObjectPool` which can
# be tested with `isnull`.
# """
# function open(path; layout = "")
#     ptr = ccall(
#         (:pmemobj_open, libpmemobj),
#         Ptr{Nothing},
#         (Cstring, Cstring),
#         path, layout,
#     )
#
#     # TODO: Check for null
#     return ObjectPool(ptr)
# end
#
# """
#     check(path; layout = "") -> Bool
#
# Perform a consistency check of the file indicated by `path`. Return `true` if the file
# passes the check.
# """
# function check(path; layout = "")
#     val = ccall(
#         (:pmemobj_check, libpmemobj),
#         Cint,
#         (Cstring, Cstring),
#         path, layout
#     )
#
#     # According to the man page, a return value of zero indicates that something
#     # went wrong
#     return val != 0
# end
#
# #####
# ##### Deling with OIDs
# #####
#
# """
#     direct(oid::PersistentOID{T}) -> Ptr{T}
#
# Return a pointer to the persistent memory object with the handle `oid`.
# """
# function direct(oid::PersistentOID{T}) where {T}
#     return ccall(
#         (:pmemobj_direct, libpmemobj),
#         Ptr{T},
#         (PersistentOID{T},),
#         oid,
#     )
# end
#
# """
#     oid(ptr::Ptr{T}) -> PersistentOID{T}
#
# Return a [`PersistentOID{T}`](@ref) handle to the object pointed to by `ptr`.
# """
# function oid(ptr::Ptr{T}) where {T}
#     return ccall(
#         (:pmemobj_oid, libpmemobj),
#         PersistentOID{T},
#         (Ptr{Cvoid},),
#         ptr,
#     )
# end
#
# """
#     pool(ptr::Ptr) -> ObjectPool
#
# Return a [`ObjectPool`](@ref) handle to the pool containing the object containt the addresses
# pointed to by `ptr`.
# """
# function pool(ptr::Ptr)
#     pool = ccall(
#         (:pmemobj_pool_by_ptr, libpmemobj),
#         Ptr{Cvoid},
#         (Ptr{Cvoid},),
#         ptr
#     )
#     return ObjectPool(pool)
# end
#
# """
#     pool(oid::PersistentOid} -> ObjectPool
#
# Return an [`ObjectPool`](@ref) handle to the pool containing the object with handle `oid`.
# """
# function pool(oid::PersistentOID{T}) where {T}
#     pool = ccall(
#         (:pmemobj_pool_by_oid, libpmemobj),
#         Ptr{Cvoid},
#         (PersistentOID{T},),
#         oid
#     )
#     return ObjectPool(pool)
# end
#
# #####
# ##### pmemobj_root
# #####
#
# # NOTE: This is not used yet, but might be used in the future if we want to add more
# # durable persistence to our arrays.
#
# """
#     root(pool, size, [::Type{T}]) -> PersistentOID{T}
#
# Create or resize the root object for the persistent memory pool `pool`.  Return a
# [`PersistentOID{T}`](@ref) handle to the allocated root object. If `T` is not provided,
# default to `Nothing`.
# *   If this is the first call to `root`, the requested size is greater than zero and the
#     root object does not exist, it is implicitly allocated in a thread-safe manner.
# *   If the requested size is larger than the current size, the root object is automatically
#     resized. In such case, the old data is preserved and the extra space is zeroed.
# *   If the requested size is equal to zero, the root object is not allocated.
# """
# function root(pool::ObjectPool, size, ::Type{T} = Nothing) where {T}
#     oid = ccall(
#         (:pmemobj_root, libpmemobj),
#         PersistentOID{T},
#         (Ptr{Cvoid}, Csize_t),
#         pool.ptr, size
#     )
#     return oid
# end
#
# """
#     root_size(pool::ObjectPool) -> Int
#
# Return the size in bytes of the root object of `pool`.
# """
# function root_size(pool::ObjectPool)
#     size = ccall(
#         (:pmemobj_root_size, libpmemobj),
#         Csize_t,
#         (Ptr{Cvoid},),
#         pool.ptr
#     )
#     return Int(size)
# end
#
# #####
# ##### allocation
# #####
#
# # The allocation function in libpmem want a callable for initialization.
# # This is a do-nothing initializer
# _undef_initializer(pool::Ptr{ObjectPool}, ptr::Ptr{Nothing}, arg::Ptr{Nothing}) = zero(Int32)
#
# function alloc(pool::ObjectPool, size)
#     oid = Ref(PersistentOID{Nothing}(0,0))
#     f = @cfunction(_undef_initializer, Cint, (Ptr{ObjectPool}, Ptr{Nothing}, Ptr{Nothing}))
#     val = ccall(
#         (:pmemobj_alloc, libpmemobj),
#         Cint,
#         (Ptr{Cvoid}, Ptr{PersistentOID{Nothing}}, Csize_t, UInt64, Ptr{Cvoid}, Ptr{Cvoid}),
#         pool.ptr,
#         oid,
#         size,
#         UInt(0),
#         f,
#         Ptr{Cvoid}(),
#     )
#     if val != 0
#         throw(error(val))
#     end
#     return oid[]
# end
#
# function free(oid::PersistentOID{T}) where {T}
#     ccall((:pmemobj_free, libpmemobj), Cvoid, (Ptr{PersistentOID{T}},), Ref(oid))
#     return nothing
# end
#
# end # module
