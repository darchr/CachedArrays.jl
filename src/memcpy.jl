# Memcopy based on AvX instructions.

"""
    _mov(src::Ptr, dest::Ptr, ::Val{N}) where {N}

Unroll move `N` elements from `src` to `ptr`.
"""
@generated function _mov(
        ::Type{SIMD.Vec{N,T}},
        dest::Ptr{UInt8},
        src::Ptr{UInt8},
        ::Val{U}
    ) where {N,T,U}

    loads = load_impl(SIMD.Vec{N,T}, U)
    stores = store_impl(SIMD.Vec{N,T}, U)
    return quote
        $(Expr(:meta, :inline))
        _src = convert(Ptr{$T}, src)
        _dest = convert(Ptr{$T}, dest)
        $(loads...)
        $(stores...)
    end
end

function load_impl(::Type{T}, U::Integer) where {T <: SIMD.Vec}
    return map(0:U-1) do j
        x = Symbol("i_$j")
        # Vector Load
        # Val(true) implies aligned load
        :($x = SIMD.vload($T, _src + $(sizeof(T)) * $j, Val(true)))
    end
end

function store_impl(::Type{T}, U::Integer) where {T <: SIMD.Vec}
    return map(0:U-1) do j
        x = Symbol("i_$j")
        # Vector Store
        # first Val(true) implies aligned store
        # second Val(true) implies nontemporal store
        #
        # See SIMD.jl for documentation.
        :(SIMD.vstore($x, _dest + $(sizeof(T)) * $j, Val(true), Val(true)))
    end
end

function sfence()
    Base.llvmcall(
        raw"""
        tail call void asm sideeffect "sfence", "~{memory},~{dirflag},~{fpsr},~{flags}"()
        ret void
        """,
        Nothing,
        Tuple{},
    )
    return nothing
end

# Chunk up an array into N views so the start of each array is aligned
# on a cache boundary.
function aligned_chunk(nbytes, nchunks)
    isone(nchunks) && return (0, nbytes)

    # Round down `nbytes` to a multiple of 64
    extra = mod(nbytes, 64)
    _nbytes = nbytes - extra

    bytes_per_chunk, tail = divrem(_nbytes, nchunks)
    chunk_remainder = mod(bytes_per_chunk, 64)
    bytes_per_chunk = bytes_per_chunk - chunk_remainder

    last_chunk = bytes_per_chunk + extra + nchunks * chunk_remainder + tail
    return (bytes_per_chunk, last_chunk)
end

_isaligned(x::Integer) = iszero(mod(x, 64))
_isaligned(x::Ptr) = _isaligned(convert(Int, x))


# Top level entry point
#
# NOTE: Dest and Src must not alias!
"""
    memcpy!(dest::AbstractArray, src::AbstractArray, [toremote = false]; [forcesingle])

Copy the contents from `src` to `dest` using non-temporal AVX store instructions.
Pass `toremote = true` if `dest` lives in PMM for better performance.
Set keyword `forcesingle = true` to force only one thread to do the copy operations.

LIMITATIONS
-----------
* `dest` and `src` must not alias at all.
* The base pointers for `dest` and `src` must be 64-byte aligned.
"""
function memcpy!(dest::AbstractArray{T}, src::AbstractArray{T}, toremote = false; forcesingle = nothing) where {T}
    # Do type check
    if !isbitstype(T)
        throw(ArgumentError("Can only memcpy isbits types."))
    end

    # Do length checks
    if length(dest) != length(src)
        throw(ArgumentError("Source and Destination must have the same lengths."))
    end

    dest_ptr = convert(Ptr{UInt8}, pointer(dest))
    src_ptr = convert(Ptr{UInt8}, pointer(src))

    # Do alignment checks
    if !_isaligned(dest_ptr) || !_isaligned(src_ptr)
        @warn "Copying unaligned"
        copyto!(dest, src)
        return nothing
    end
    #if !iszero(mod(convert(Int, dest_ptr), 64))
    #    throw(ArgumentError("Destination is not aligned to a cache boundary!"))
    #end
    #if !iszero(mod(convert(Int, src_ptr), 64))
    #    throw(ArgumentError("Source is not aligned to a cache boundary!"))
    #end

    @static if THREADED_COPY
        # Determine if we're moving to the remote array.
        # Then only use 4 threads.
        #
        # Otherwise, use all available threads.
        if !isnothing(forcesingle)
            unsafe_memcpy!(+, dest_ptr, src_ptr, sizeof(dest))
            sfence()
        else
            nchunks = toremote ? 4 : Threads.nthreads()
            bytes_per_chunk, last_chunk = aligned_chunk(sizeof(dest), nchunks)

            Threads.@threads for i in 1:nchunks
                offset = bytes_per_chunk * (i-1)
                copybytes = (i == nchunks) ? last_chunk : bytes_per_chunk
                unsafe_memcpy!(+, dest_ptr + offset, src_ptr + offset, copybytes)
                sfence()
            end
        end
    else
        unsafe_memcpy!(+, dest_ptr, src_ptr, sizeof(T) * length(src))
        sfence()
    end
    return nothing
end

const AVX512BYTES = 64

_adjust(::typeof(+), x::Ptr{UInt8}, len) = x
_adjust(::typeof(-), x::Ptr{UInt8}, len) = x + len

inv(::typeof(+)) = -
inv(::typeof(-)) = +

"""
    unsafe_memcpy!(f, dest::Ptr{UInt8}, src::Ptr{UInt8}, len)

Copy `len` bytes from `src` to `dest`.
If `f == Base.+`, the copy will be done from low addresses to high addresses.
If `f == Base.-`, the copy will be done from low addresses to high addresses.
"""
function unsafe_memcpy!(f, dest::Ptr{UInt8}, src::Ptr{UInt8}, len)
    # By this point, assume that `dest`  and `src` are aligned.
    dest = _adjust(f, dest, len)
    src = _adjust(f, src, len)

    # While we can, manually unroll the AVX512 move operation by 32 times.
    while (len >= 32 * AVX512BYTES)
        _mov(SIMD.Vec{8,Int64}, dest, src, Val{32}())
        len = inv(f)(len, 32 * AVX512BYTES)
        dest = f(dest, 32 * AVX512BYTES)
        src = f(src, 32 * AVX512BYTES)
    end

    if (len >= 16 * AVX512BYTES)
        _mov(SIMD.Vec{8,Int64}, dest, src, Val{16}())
        len = inv(f)(len, 16 * AVX512BYTES)
        dest = f(dest, 16 * AVX512BYTES)
        src = f(src, 16 * AVX512BYTES)
    end

    if (len >= 8 * AVX512BYTES)
        _mov(SIMD.Vec{8,Int64}, dest, src, Val{8}())
        len = inv(f)(len, 8 * AVX512BYTES)
        dest = f(dest, 8 * AVX512BYTES)
        src = f(src, 8 * AVX512BYTES)
    end

    if (len >= 4 * AVX512BYTES)
        _mov(SIMD.Vec{8,Int64}, dest, src, Val{4}())
        len = inv(f)(len, 4 * AVX512BYTES)
        dest = f(dest, 4 * AVX512BYTES)
        src = f(src, 4 * AVX512BYTES)
    end

    if (len >= 2 * AVX512BYTES)
        _mov(SIMD.Vec{8,Int64}, dest, src, Val{2}())
        len = inv(f)(len, 2 * AVX512BYTES)
        dest = f(dest, 2 * AVX512BYTES)
        src = f(src, 2 * AVX512BYTES)
    end

    if (len >= 1 * AVX512BYTES)
        _mov(SIMD.Vec{8,Int64}, dest, src, Val{1}())
        len = inv(f)(len, 1 * AVX512BYTES)
        dest = f(dest, 1 * AVX512BYTES)
        src = f(src, 1 * AVX512BYTES)
    end

    # Normal MEMCPY the rest.
    unsafe_copyto!(dest, src, len)
    return nothing
end

