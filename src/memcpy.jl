# Memcopy based on AvX instructions.

"""
    _mov(src::Ptr, dst::Ptr, ::Val{N}) where {N}

Unroll move `N` elements from `src` to `ptr`.
"""
@generated function _mov(
        ::Type{SIMD.Vec{N,T}},
        src::Ptr{T},
        dst::Ptr{T},
        ::Val{U}
    ) where {N,T,U}

    loads = load_impl(SIMD.Vec{N,T}, U)
    stores = store_impl(SIMD.Vec{N,T}, U)
    return quote
        $(loads...)
        $(stores...)
    end
end

function load_impl(::Type{T}, U::Integer) where {T <: SIMD.Vec}
    return map(0:U-1) do j
        x = Symbol("i_$j")
        :($x = SIMD.vload($T, src + $(sizeof(T)) * $j, Val(true)))
    end
end

function store_impl(::Type{T}, U::Integer) where {T <: SIMD.Vec}
    return map(0:U-1) do j
        x = Symbol("i_$j")
        :(SIMD.vstore($x, dst + $(sizeof(T)) * $j, Val(true), Val(true)))
    end
end

