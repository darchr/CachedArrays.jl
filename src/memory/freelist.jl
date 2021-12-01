#####
##### Freelists
#####

abstract type AbstractFreelist{T} end

# Parameterize the freelist for testing purposts.
# Expected api for T:
#      .next
#      .previous
#      address
#      isnull
mutable struct Freelist{T} <: AbstractFreelist{T}
    base::T
end
Freelist{T}() where {T} = Freelist(T())
getbase(x::Freelist) = x.base
setbase!(x::Freelist{T}, v::T) where {T} = (x.base = v)

# Raw pointer based list
struct FreelistPtr{T} <: AbstractFreelist{T}
    ptr::Ptr{T}
end
getbase(x::FreelistPtr) = unsafe_load(x.ptr)
setbase!(x::FreelistPtr{T}, v::T) where {T} = unsafe_store!(x.ptr, v)

#####
##### Implementation
#####

function Base.length(x::AbstractFreelist)
    count = 0
    item = getbase(x)
    while !isnull(item)
        count += 1
        item = item.next
    end

    return count
end

Base.isempty(x::AbstractFreelist) = isnull(getbase(x))

function Base.pop!(x::AbstractFreelist{T}) where {T}
    # Pull out the base
    item = getbase(x)

    # Set the base block to the next block
    # If this hasn't emptied the freelist, null out the previous field
    setbase!(x, item.next)
    if !isempty(x)
        getbase(x).previous = T()
    end
    item.next = T()
    return item
end

function Base.push!(x::AbstractFreelist{T}, item::T) where {T}
    if isempty(x)
        item.next = T()
    else
        @check isnull(getbase(x).previous)
        getbase(x).previous = item
        item.next = getbase(x)
    end
    item.previous = T()
    setbase!(x, item)
    return x
end

function swap!(a::T, b::T) where {T}
    @check a.next == b
    @check b.previous == a

    # Swap neighbors
    isnull(a.previous) || (a.previous.next = b)
    isnull(b.next) || (b.next.previous = a)

    b.previous = a.previous
    a.next = b.next
    b.next = a
    a.previous = b
    return nothing
end

function remove!(x::AbstractFreelist{T}, item::T) where {T}
    # Update the linked-list.
    if isnull(item.previous)
        setbase!(x, item.next)
    else
        item.previous.next = item.next
    end

    if !isnull(item.next)
        item.next.previous = item.previous
    end
    item.next = item.previous = T()
    return nothing
end

# Sort the freelist by some parameter
# uses a simple bubble sort - so probably not that fast.
function Base.sort!(x::AbstractFreelist; lt = isless)
    isempty(x) && return nothing

    while true
        swapped = false
        current = getbase(x)

        next = current.next
        while !isnull(next)
            if lt(next, current)
                # Implicitly updates `block`.
                swap!(current, next)
                # Update the base element if needed.
                # Handling of `null` will be dealt with in `swap!`.
                current == getbase(x) && (setbase!(x, next))
                swapped = true
            else
                current = next
            end
            next = current.next
        end

        # If we haven't updated anything, then we're done.
        swapped || break
    end
    return nothing
end

function Base.iterate(x::AbstractFreelist, item = getbase(x))
    isnull(item) && return nothing
    return (item, item.next)
end

