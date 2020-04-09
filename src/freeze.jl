#####
##### FrozenCachedArray
#####

# This version is an immutable version of the `CachedArray`.
# We specifically make `setindex!` throw an error.
#
# When `unfreeze` is called, we suspect that writing to the actual array is happening
# and so we can mark the original array as "dirty"
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

# TODO: Implement the rest of the Array API
# In general, this just forwards methods to the actual underlying `array`
#
# We can programatically generate these methods - using better more specific definitions
# as method ambiguities arise.

