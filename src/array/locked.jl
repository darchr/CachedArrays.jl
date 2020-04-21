#####
##### LockedCachedArray
#####

# This version is an immutable version of the `CachedArray`.
# We specifically make `setindex!` throw an error.
#
# When `unlock` is called, we suspect that writing to the actual array is happening
# and so we can mark the original array as "dirty"
#
# Note that LockedCachedArrays don't need to worry about managing their state in the
# CacheManager because that is handled by the underlying CachedArray.
struct LockedCachedArray{T,N,C <: CacheManager} <: AbstractCachedArray{T,N}
    array::CachedArray{T,N,C}
end

LockedCachedArray(A::Array{T,N}) where {T,N} = lock(CachedArray(A))

function LockedCachedArray{T}(::UndefInitializer, i::Integer) where {T}
    return LockedCachedArray{T}(undef, (convert(Int, i),))
end

function LockedCachedArray{T}(::UndefInitializer, dims::NTuple{N,Int}) where {T,N}
    # Create the wrapped CachedArray
    array = CachedArray{T}(undef, dims)
    # We're creating it, so ensure that it is dirty.
    array.dirty = true
    return LockedCachedArray(array)
end

Base.lock(x::CachedArray) = LockedCachedArray(x)
Base.lock(x::LockedCachedArray) = x

unlock(x::CachedArray) = x
function unlock(x::LockedCachedArray)
    x.array.dirty = true
    return x.array
end

Base.convert(::Type{LockedCachedArray{T,N}}, x::CachedArray{T,N}) where {T,N} = lock(x)

function Base.similar(
        ::Type{<:LockedCachedArray},
        ::Type{S},
        dims::Tuple{Vararg{Int64,N}}
    ) where {S,N}

    return LockedCachedArray{S}(undef, dims)
end

#####
##### Broadcasting
#####

const LockedStyle = Broadcast.ArrayStyle{LockedCachedArray}

Base.BroadcastStyle(::Type{<:LockedCachedArray}) = LockedStyle()
function Base.similar(bc::Broadcast.Broadcasted{LockedStyle}, ::Type{ElType}) where {ElType}
    return similar(LockedCachedArray{ElType}, axes(bc))
end


#####
##### Array API
#####

# By punning of the `array` subfield, the definitions we make for `AbstractCachedArray`
# will automatically recurse from the `LockedCachedArray` to the `CachedArray`

# Here, we explicitly make setindex! and company errors to enforce the immutability
# of this type.
#
# This is not perfect because one can always take a pointer to the underlying data structure,
# but that would be out of contract and all kinds of crazy things could happen then.
function Base.setindex!(::LockedCachedArray, v, i::Int)
    error("Cannot call `setindex!` on a LockedCachedArray. Call `unlock` to mutate")
end

# For functions that globally modify an LockedCachedArray,
# we call `unlock` on the array.
# This automatically marks the array as dirty.
#
# Insert prefetches for each `copyto!` implementation
function Base.copyto!(
        dest::LockedCachedArray,
        desto::Integer,
        src::AbstractArray,
        srco::Integer,
        N::Integer
    )
    prefetch!(dest)
    return copyto!(unlock(dest), desto, src, srco, N)
end

function Base.copyto!(dest::LockedCachedArray, src::AbstractArray)
    prefetch!(dest)
    return lock(copyto!(unlock(dest), src))
end

function Base.copyto!(dest::LockedCachedArray, bc::Broadcast.Broadcasted)
    prefetch!(dest)
    return lock(copyto!(unlock(dest), bc))
end

#####
##### Cached API
#####

MacroTools.@forward LockedCachedArray.array (
    id,
    manager,
    parent,
    isparent,
    hasparent,
    islocal,
    isremote,
    isdirty,
    isclean,
    evict!,
    move_to_remote,
)

# If we prefetch a `LockedCachedArray`, then if the array is fetch, it should be marked
# as clean.
prefetch!(A::LockedCachedArray) = prefetch!(A.array; dirty = false)

