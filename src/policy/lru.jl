struct Priority{T}
    priority::Int
    val::T
end

Base.isless(a::P, b::P) where {P <: Priority} = isless(a.priority, b.priority)
Base.:(==)(a::P, b::P) where {P <: Priority} = a.priority == b.priority
Base.hash(a::Priority, h::UInt = UInt(0x78907890)) = hash(a.priority, h)
unwrap(x::Priority) = x.val

# LRU Policy for eviction.
const MutableMinHeap{T} = DataStructures.MutableBinaryHeap{T,Base.ForwardOrdering}
mutable struct LRU{T}
    heap::MutableMinHeap{Priority{T}}
    handles::Dict{T,Int}
end

LRU{T}() where {T} = LRU(MutableMinHeap{Priority{T}}(), Dict{T,Int}())
Base.isempty(lru::LRU) = isempty(lru.heap)
Base.length(lru::LRU) = length(lru.heap)

function Base.pop!(lru::LRU)
    v = pop!(lru.heap)
    delete!(lru.handles, v.val)
    return v.val
end

function Base.push!(lru::LRU{T}, v::T, priority::Int) where {T}
    # Assert this for now.
    @check !haskey(lru.handles, v)

    # Add this to the heap and record its handle.
    lru.handles[v] = push!(lru.heap, Priority(priority, v))
    return v
end

Base.first(lru::LRU) = unwrap(first(lru.heap))

function update!(lru::LRU{T}, v::T, priority::Int) where {T}
    handle = lru.handles[v]
    DataStructures.update!(lru.heap, handle, Priority(priority, v))
end

function Base.delete!(lru::LRU{T}, v::T) where {T}
    delete!(lru.heap, lru.handles[v])
    delete!(lru.handles, v)
    return lru
end

Base.in(v::T, lru::LRU{T}) where {T} = haskey(lru.handles, v)

