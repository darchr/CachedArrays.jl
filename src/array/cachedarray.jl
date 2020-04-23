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

#####
##### Constructors
#####

function CachedArray{T,N}(x::Array{T,N}, parent, manager::C = GlobalManager[]) where {T,N,C}
    _x = unsafe_alloc(PoolType{DRAM}(), manager, typeof(x), size(x))
    copyto!(_x, x)
    return CachedArray{T,N,C}(_x, manager)
end

CachedArray(x::Array{T,N}) where {T,N} = CachedArray{T,N}(x, nothing)

function CachedArray{T}(::UndefInitializer, i::Integer) where {T}
    return CachedArray{T}(undef, (convert(Int, i),))
end

function CachedArray{T}(
        ::UndefInitializer,
        dims::NTuple{N,Int},
        manager::C = GlobalManager[],
    ) where {T,N,C}

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

# "Similar" variants in all their glory!
# TODO: Think about how to propagate metadata ...
function Base.similar(::Type{<:CachedArray}, ::Type{S}, dims::Tuple{Vararg{Int64,N}}) where {S,N}
    return CachedArray{S}(undef, dims)
end

function Base.similar(::Type{A}, dims::Tuple{Vararg{Int64,N}}) where {T,A <: AbstractCachedArray{T},N}
    return similar(A, T, dims)
end

function Base.similar(A::AbstractCachedArray, ::Type{S}, dims::Tuple{Vararg{Int64,N}}) where {S,N}
    return similar(typeof(A), S, dims)
end

@inline Base.iterate(A::AbstractCachedArray) = iterate(A.array)
@inline Base.iterate(A::AbstractCachedArray, i) = iterate(A.array, i)

#####
##### Broadcasting
#####

# Hijack broadcasting so we prioritize CachedArrays.
const CachedStyle = Broadcast.ArrayStyle{CachedArray}

Base.BroadcastStyle(::Type{<:CachedArray}) = CachedStyle()
function Base.similar(bc::Broadcast.Broadcasted{CachedStyle}, ::Type{ElType}) where {ElType}
    return similar(CachedArray{ElType}, axes(bc))
end

