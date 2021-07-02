struct Priority{T}
    priority::Int
    val::T
end

Base.isless(a::P, b::P) where {P <: Priority} = isless(a.priority, b.priority)
Base.:(==)(a::P, b::P) where {P <: Priority} = a.priority == b.priority
Base.hash(a::Priority, h::UInt = UInt(0x0)) = hash(a.priority, h)

getval(::Type{T}, x::Priority{T}) where {T} = x.val

# LRU Policy for eviction.
mutable struct LRU{T}
    count::Int

    # Wrap around a mutable binary heap.
    heap::DataStructures.MutableBinaryHeap{Priority{T}, Base.ForwardOrdering}
    # Map items to their handles
    handles::Dict{T,Int}
end

function LRU{T}() where {T}
    return LRU(
        0,
        DataStructures.MutableBinaryMinHeap{Priority{T}}(),
        Dict{T,Int}(),
    )
end

Base.eltype(::LRU{T}) where {T} = T
fulleltype(::LRU{T}) where {T} = Priority{T}

Base.isempty(lru::LRU) = isempty(lru.heap)

"""
    pop!(C::LRU)

Remove the least recently used item from the cache and return it.
"""
function Base.pop!(C::LRU)
    # Pop items off the top of the heap.
    # Keep doing this as long as we are getting sentinels.
    v = pop!(C.heap)
    delete!(C.handles, v.val)
    return v.val
end

function fullpop!(C::LRU)
    v = pop!(C.heap)
    delete!(C.handles, v.val)
    return v
end

function Base.push!(C::LRU{T}, v::T, pool) where {T}
    pool == Remote && return v
    # Assert this for now.
    @check !haskey(C.handles, v)

    # Add this to the heap and record its handle.
    C.handles[v] = push!(C.heap, Priority(C.count, v))
    C.count += 1
    return v
end

function Base.push!(C::LRU{T}, v::Priority{T}) where {T}
    @check !haskey(C.handles, v.val)
    C.handles[v.val] = push!(C.heap, v)
    return v
end

"""
    update!(C::LRU, v)

Update `v` to the bottom of the heap.
"""
function update!(C::LRU, v)
    delete!(C.heap, C.handles[v])
    C.handles[v] = push!(C.heap, Priority(C.count, v))
    C.count += 1
end

"""
    delete!(C::LRUManager, v)

Delete `v` from the cache.

This is assumed to be a user-invoked method and the callback will **NOT** be called on
the removed item.
"""
function Base.delete!(C::LRU, v)
    in(v, C) || return nothing
    delete!(C.heap, C.handles[v])
    delete!(C.handles, v)
    return C
end

Base.in(v, C::LRU) = haskey(C.handles, v)

