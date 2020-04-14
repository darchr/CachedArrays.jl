struct Priority{T}
    priority::Int
    val::T
    sz::Int
end

getsize(P::Priority) = P.sz
Base.isless(a::P, b::P) where {P <: Priority} = isless(a.priority, b.priority)
Base.:(==)(a::P, b::P) where {P <: Priority} = a.priority == b.priority
Base.hash(a::Priority, h::UInt = UInt(0x0)) = hash(a.priority, h)

# LRU Policy for eviction.
mutable struct LRUCache{T}
    # Keep track of the maximum size for the cache as well as the current size.
    maxsize::Int
    currentsize::Int
    count::Int

    # Wrap around a mutable binary heap.
    heap::DataStructures.MutableBinaryHeap{Priority{T}, DataStructures.LessThan}

    # Map items to their handles
    handles::Dict{T,Int}
end

donothing(x...) = nothing
function LRUCache{T}(maxsize) where {T}
    return LRUCache(
        maxsize,
        0,
        0,
        DataStructures.MutableBinaryMinHeap{Priority{T}}(),
        Dict{T,Int}(),
    )
end

@inline freespace(C::LRUCache) = (C.maxsize - C.currentsize)
@inline currentsize(C::LRUCache) = C.currentsize

"""
    free!(C::LRUCache, sz::Int)

Make sure there is `sz` space avalable in `C`.
"""
function free!(C::LRUCache, sz::Int, cb)
    while freespace(C) < sz
        pop!(C, cb)
    end
    return nothing
end
Base.empty!(C::LRUCache; cb = donothing) = free!(C, C.maxsize, cb)

"""
    pop!(C::LRUCache)

Remove the least recently used item from the cache and return it.
"""
function Base.pop!(C::LRUCache, cb)
    # Pop items off the top of the heap.
    # Keep doing this as long as we are getting sentinels.
    v = pop!(C.heap)
    delete!(C.handles, v.val)

    # Now, we have a nonsentinel value.
    # Update the current size and then perform the callback
    C.currentsize -= getsize(v)
    cb(v.val)
    return v.val
end

function Base.push!(C::LRUCache, v, sz; cb = donothing)
    # Assert this for now.
    @check !haskey(C.handles, v)

    # Free up space for this object.
    free!(C, sz, cb)

    # Add this to the heap and record its handle.
    C.handles[v] = push!(C.heap, Priority(C.count, v, sz))
    C.count += 1
    C.currentsize += sz
end

"""
    update!(C::LRUCache, v)

Update `v` to the bottom of the heap.
"""
function update!(C::LRUCache, v, sz)
    delete!(C.heap, C.handles[v])
    C.handles[v] = push!(C.heap, Priority(C.count, v, sz))
    C.count += 1
end

"""
    delete!(C::LRUManager, v, sz)

Delete `v` from the cache.
This is assumed to be a user-invoked method and the callback will **NOT** be called on
the removed item.
"""
function Base.delete!(C::LRUCache, v, sz)
    delete!(C.heap, C.handles[v])
    delete!(C.handles, v)
    C.currentsize -= sz
    return C
end

Base.in(v, C::LRUCache) = haskey(C.handles, v)

