abstract type AbstractCachedArray{T,N} <: DenseArray{T,N} end
cacheable(::AbstractCachedArray) = Cacheable()

# Must make this mutable so we can attach a finalizer to it, unfortunately.
mutable struct CachedArray{T,N,C <: CacheManager} <: AbstractCachedArray{T,N}
    # This is the underlying data array.
    # When this array is remote, we use `unsafe_wrap` to wrap our own pointer.
    array::Array{T,N}
    manager::CacheManager

    # Inner constructor - do a type chack and make sure the finalizer is attached.
    function CachedArray{T,N,C}(
            array::Array{T,N},
            manager::C,
        ) where {T,N,C}

        if !isbitstype(T)
            throw(ArgumentError("Cannot construct a `CachedArray` from non-isbits types!"))
        end

        A = new{T,N,C}(
            array,
            manager,
        )

        register!(A)
        return A
    end
end

#####
##### `Cacheable` Interface
#####

Base.pointer(A::AbstractCachedArray) = pointer(A.array)
manager(x::AbstractCachedArray) = x.manager
replace!(C::CachedArray{T,N}, A::Array{T,N}) where {T,N} = (C.array = A)
arraytype(C::CachedArray{T,N}) where {T,N} = Array{T,N}

# utils
strip_params(::Type{<:CachedArray}) = CachedArray
strip_params(::T) where {T <: AbstractCachedArray} = strip_params(T)

#####
##### Constructors
#####

function CachedArray{T,N}(x::Array{T,N}, manager::C) where {T,N,C}
    _x = unsafe_alloc(PoolType{DRAM}(), manager, typeof(x), size(x))
    copyto!(_x, x)
    return CachedArray{T,N,C}(_x, manager)
end

function CachedArray{T}(::UndefInitializer, manager, i::Integer) where {T}
    return CachedArray{T}(undef, manager, (convert(Int, i),))
end

function CachedArray{T}(::UndefInitializer, manager::C, dims::NTuple{N,Int}) where {T,N,C}
    array = unsafe_alloc(PoolType{DRAM}(), manager, Array{T,N}, dims)
    A = CachedArray{T,N,C}(array, manager)
    return A
end

#####
##### Array Interface
#####

Base.unsafe_convert(::Type{Ptr{T}}, A::AbstractCachedArray{T}) where {T} = pointer(A)
@inline Base.size(A::AbstractCachedArray) = size(A.array)
Base.sizeof(A::AbstractCachedArray) = sizeof(A.array)
Base.elsize(::AbstractCachedArray{T}) where {T} = sizeof(T)

Base.@propagate_inbounds @inline Base.getindex(A::AbstractCachedArray, i::Int) = A.array[i]
Base.@propagate_inbounds @inline Base.setindex!(A::AbstractCachedArray, v, i::Int) = setindex!(A.array, v, i)
Base.IndexStyle(::Type{<:AbstractCachedArray}) = Base.IndexLinear()

function Base.similar(
        A::AbstractCachedArray,
        eltyp::Type{T} = eltype(A),
        dims::Tuple{Vararg{Int,N}} = size(A)
   ) where {T,N}

    strip_params(A){eltyp}(undef, manager(A), dims)
end

@inline Base.iterate(A::AbstractCachedArray) = iterate(A.array)
@inline Base.iterate(A::AbstractCachedArray, i) = iterate(A.array, i)

#####
##### Broadcasting
#####

# Hijack broadcasting so we prioritize CachedArrays.
const CachedStyle = Broadcast.ArrayStyle{CachedArray}

function Base.BroadcastStyle(::Type{T}) where {T <: AbstractCachedArray}
    return Broadcast.ArrayStyle{strip_params(T)}()
end

function Base.similar(
        bc::Broadcast.Broadcasted{Broadcast.ArrayStyle{T}},
        ::Type{ElType}
    ) where {T <: AbstractCachedArray, ElType}

    cached = findT(T, bc)
    return similar(cached, ElType, axes(bc))
end

findT(::Type{T}, bc::Base.Broadcast.Broadcasted) where {T} = findT(T, bc.args)
findT(::Type{T}, x::Tuple) where {T} = findT(T, findT(T, first(x)), Base.tail(x))
findT(::Type{T}, x::U) where {T, U <: T} = x
findT(::Type{T}, x) where {T} = nothing
findT(::Type{T}, ::Tuple{}) where {T} = nothing
findT(::Type{T}, x::U, rest) where {T, U <: T} = x
findT(::Type{T}, ::Any, rest) where {T} = findT(T, rest)

