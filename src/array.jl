# Must make this mutable so we can attach a finalizer to it.
mutable struct CachedArray{T,N} <: AbstractArray{T,N}
    # This is the underlying data array.
    # When this array is remote, we use `unsafe_wrap` to wrap our own pointer.
    array::Array{T,N}

    # Pointer to the remote the far memory location of the array.
    # This could be null if the remote hasn't been allocated.
    #
    # This array `A` is in remote storate if `pointer(A) == A.remote_ptr`
    remote_ptr::Ptr{T}

    # Hints regarding storage
    #
    # Should be conservative with this flag and always default to `true`.
    dirty::Bool
end

function CachedArray{T}(::UndefInitializer, i::Integer) where {T}
    return CachedArray{T}(undef, (convert(Int,i),), true)
end

function CachedArray{T}(::UndefInitializer, dims::NTuple{N,Int}) where {T,N}
    # Default alloc to near memory
    array = Array{T}(undef, dims)

    # Default the `remote_ptr` to a null ptr
    A = CachedArray{T,N}(array, Ptr{T}(0), true)
    return A
end

#####
##### Array Interface
#####

Base.pointer(A::CachedArray) = pointer(A.array)
@inline Base.size(A::CachedArray) = size(A.array)

Base.@propagate_inbounds @inline Base.getindex(A::CachedArray, i::Int) = A.array[i]
Base.@propagate_inbounds @inline Base.setindex!(A::CachedArray, v, i::Int) = setindex!(A.array, v, i)
Base.IndexStyle(::Type{<:CachedArray}) = Base.IndexLinear()

# "Similar" variants in all their glory!
# TODO: Think about how to propagate metadata ...
function Base.similar(::Type{<:CachedArray}, ::Type{S}, dims::Tuple{Vararg{Int64,N}}) where {S,N}
    return CachedArray{S}(undef, dims)
end

function Base.similar(::Type{<:CachedArray{T}}, dims::Tuple{Vararg{Int64,N}}) where {T,N}
    return similar(CachedArray, T, dims)
end

function Base.similar(A::CachedArray, ::Type{S}, dims::Tuple{Vararg{Int64,N}}) where {S,N}
    return similar(typeof(A), S, dims)
end

@inline Base.iterate(A::CachedArray) = iterate(A.array)
@inline Base.iterate(A::CachedArray, i) = iterate(A.array, i)

#####
##### Broadcasting
#####

# Hijack broadcasting so we prioritize CachedArrays.
const CachedStyle = Broadcast.ArrayStyle{CachedArray}
Base.BroadcastStyle(::Type{<:CachedArray}) = CachedStyle()

function Base.similar(bc::Broadcast.Broadcasted{CachedStyle}, ::Type{ElType}) where {ElType}
    return similar(CachedArray{ElType}, axes(bc))
end

