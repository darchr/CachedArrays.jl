# Here's the problem:
#
# You have linearly spaced buckets for allocation.
# You try to allocate a bucket of size `N`, only to find that the freelist for size `N`
# is empty.
#
# How do you efficiently find the first non-empty bucket of size greater than `N`?
#
# One option is to linearly scan the freelists ... but that's linear.
#
# The approach here is use a `FindNextTree`.
#
# The idea is that we use a bitmask UInt64 to encode groups of 64 buckets.
# A 1 in the mask represents that a bucket is non-empty.
# A quick lookup in the mask can be done using an `and` and a call to `trailing_zeros`.
#
# Then, we encode a next level in the tree, also using UInt64 masks which will then
# cover 4096 buckets - using a 1 to indicate if any entry in the sub-tree is non empty.
# We can continue building trees as long as we want.
struct Mask
    val::UInt64
end

Base.:|(a::Mask, b::UInt64) = Mask(a.val | b)
Base.:&(a::Mask, b::UInt64) = Mask(a.val & b)

"""
    firstentry(m::Mask, [i::Integer])

Return the index of the first non-zero entry in `m`.
If `i` is provided, return the index of the first non-zero entry in `m` greater than `i`.

Behavior is undefined if `i > 64` - so don't so that.
"""
firstentry(m::Mask, i::Integer = 0) = trailing_zeros(m.val >> i) + i + 1

"""
    isvalidentry(x::Integer) -> Bool

Return `true` if the value returned by `firstentry` is a valid entry.
"""
isvalidentry(x::Integer) = x <= 64

"""
    hasentry(m::Mask, [i::Integer]) -> Bool

Return `true` is `m` has at least one non-zero entry.
If `i` is provided, return `true` is `m` has at least one non-zero entry greater than `i`.
"""
hasentry(m::Mask, i::Integer = 0) = (m.val >> i) >= one(UInt64)
hasentryat(m::Mask, i::Integer) = isodd(m.val >> (i-1))

"""
    numentries(m::Mask) -> Integer

Return the number of set bits in `m`.
"""
numentries(m::Mask) = count_ones(m.val)

cdiv(x::T, y::T) where {T} = one(T) + div(x - one(T), y)

struct FindNextTree
    # runs[1] is the highest-level
    # higher indices descend through the tree.
    # runs[N] indicate the leaves.
    runs::Vector{Vector{Mask}}
    length::Int
end

function FindNextTree(len::Integer)
    original_length = len
    runs = Vector{Vector{Mask}}()

    # Keep adding entries until we've covered the whole tree.
    while true
        thislen = cdiv(len, 64)
        pushfirst!(runs, [Mask(0) for _ in 1:thislen])
        thislen == 1 && break
        len = thislen
    end
    return FindNextTree(runs, original_length)
end

# Faster mod for powers of 64
@inline mod64(i::T) where {T} = i & T(63)
@inline div64(i::T) where {T} = i >> 6

@inline function divrem64(x::T) where {T}
    mod = mod64(x)
    div = div64(x)
    return iszero(mod) ? (div, T(64)) : (div+1, mod)
end

# Setting and clearing entries.
function setentry!(M::FindNextTree, index::Integer)
    # Bound check
    @boundscheck begin
        index > M.length && throw(BoundsError(M, index))
    end

    level = length(M.runs)
    while true
        run = M.runs[level]

        newindex, modindex = divrem64(index)
        @inbounds update = !hasentry(run[newindex])
        @inbounds run[newindex] |= (one(UInt64) << (modindex-1))

        # Break out if either we don't need an update, or this is the top level.
        (update && level > 1) || break
        index = newindex
        level -= 1
    end
    return nothing
end

function clearentry!(M::FindNextTree, index::Integer)
    # Bound check
    @boundscheck begin
        index > M.length && throw(BoundsError(M, index))
    end

    level = length(M.runs)
    while true
        run = M.runs[level]

        newindex, modindex = divrem64(index)

        @inbounds run[newindex] &= ~(one(UInt64) << (modindex-1))
        @inbounds update = !hasentry(run[newindex])
        (update && level > 1) || break
        index = newindex
        level -= 1
    end
    return nothing
end

function Base.findnext(M::FindNextTree, index::Integer)
    index > 64 * length(M.runs[end]) && return nothing
    level = length(M.runs)

    # Go up the tree until we find a non-empty entry.
    @inbounds while true
        run = M.runs[level]
        newindex, modindex = divrem64(index)
        mask = run[newindex]

        # Find the first entry for this index.
        # If it is valid, then we're good to go.
        entry = firstentry(mask, modindex)
        if isvalidentry(entry)
            # Go into this level.
            index = ((newindex - 1) << 6) + entry
            level += 1
            break
        end

        index = newindex
        # If we're at the top level and haven't found anything, we won't find anything.
        level == 1 && return nothing
        level -= 1
    end

    # Now, we have to go back down the tree to get the final index
    @inbounds while level <= length(M.runs)
        run = M.runs[level]
        index = ((index - 1) << 6) + firstentry(run[index])
        level += 1
    end
    return index
end

