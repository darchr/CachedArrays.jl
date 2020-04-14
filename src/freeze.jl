#####
##### FrozenCachedArray
#####

# This version is an immutable version of the `CachedArray`.
# We specifically make `setindex!` throw an error.
#
# When `unfreeze` is called, we suspect that writing to the actual array is happening
# and so we can mark the original array as "dirty"
#
# Note that FrozenCachedArrays don't need to worry about managing their state in the
# CacheManager because that is handled by the underlying CachedArray.
struct FrozenCachedArray{T,N,C <: CacheManager} <: AbstractCachedArray{T,N}
    array::CachedArray{T,N,C}
end

freeze(x::CachedArray) = FrozenCachedArray(x)
freeze(x::FrozenCachedArray) = x

unfreeze(x::CachedArray) = x
function unfreeze(x::FrozenCachedArray)
    x.array.dirty = true
    return x.array
end
#####
##### Array API
#####

# By punning of the `array` subfield, the definitions we make for `AbstractCachedArray`
# will automatically recurse from the `FrozenCachedArray` to the `CachedArray`

# Here, we explicitly make setindex! and company errors to enforce the immutability
# of this type.
#
# This is not perfect because one can always take a pointer to the underlying data structure,
# but that would be out of contract and all kinds of crazy things could happen then.
function Base.setindex!(::FrozenCachedArray, v, i::Int)
    error("Cannot call `setindex!` on a FrozenCachedArray. Call `unfreeze` to mutate")
end

function Base.copyto!(::FrozenCachedArray, desto, src, srco, N)
    error("Cannot call `copyto!` on a FrozenCachedArray. Call `unfreeze` to mutate")
end

# TODO: `Similar` and friends

#####
##### Cached API
#####

MacroTools.@forward FrozenCachedArray.array (
    id,
    manager,
    parent,
    isparent,
    hasparent,
    islocal,
    isdirty,
    isclean,
    prefetch!,
    evict!
)

