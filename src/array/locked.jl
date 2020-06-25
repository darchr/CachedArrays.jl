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

LockedCachedArray(A::Array, manager) = LockedCachedArray(CachedArray(A, manager))

function LockedCachedArray{T}(::UndefInitializer, manager, i::Integer) where {T}
    return LockedCachedArray{T}(undef, manager, (convert(Int, i),))
end

function LockedCachedArray{T}(::UndefInitializer, manager, dims::NTuple{N,Int}) where {T,N}
    # Create the wrapped CachedArray
    array = CachedArray{T}(undef, manager, dims)
    # We're creating it, so ensure that it is dirty.
    return LockedCachedArray(array)
end

strip_params(::Type{<:LockedCachedArray}) = LockedCachedArray
Base.lock(x::CachedArray) = LockedCachedArray(x)
Base.lock(x::LockedCachedArray) = x

Base.unlock(x::CachedArray) = x
function Base.unlock(x::LockedCachedArray)
    setdirty!(x)
    return x.array
end

Base.convert(::Type{LockedCachedArray{T,N}}, x::CachedArray{T,N}) where {T,N} = lock(x)

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

Base.getindex(A::LockedCachedArray, i::Int) = getindex(A.array, i)

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
    shallowfetch!(dest)
    return copyto!(unlock(dest), desto, src, srco, N)
end

function Base.copyto!(dest::LockedCachedArray, src::AbstractArray)
    shallowfetch!(dest)
    return lock(copyto!(unlock(dest), src))
end

function Base.copyto!(dest::LockedCachedArray, bc::Broadcast.Broadcasted)
    shallowfetch!(dest)
    return lock(copyto!(unlock(dest), bc))
end

# Ambiguity resolution
function Base.copyto!(
        dest::LockedCachedArray,
        bc::Broadcast.Broadcasted{<:Broadcast.AbstractArrayStyle{0}}
    )

    shallowfetch!(dest)
    return lock(copyto!(unlock(dest), bc))
end

#####
##### Cached API
#####

MacroTools.@forward LockedCachedArray.array (
    datapointer,
    Base.pointer,
    Base.size,
    manager,
    evict!,
    shallowfetch!,
)

# If we prefetch a `LockedCachedArray`, then if the array is fetch, it should be marked
# as clean.
prefetch!(A::LockedCachedArray) = prefetch!(A.array; dirty = false)

#####
##### Unlocking Definitions
#####

Random.rand!(rng::Random.AbstractRNG, A::LockedCachedArray) = Random.rand!(rng, unlock(A))
Random.randn!(rng::Random.AbstractRNG, A::LockedCachedArray) = Random.randn!(rng, unlock(A))
