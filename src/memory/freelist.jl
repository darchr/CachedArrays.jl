#####
##### Freelists
#####

# Parameterize the freelist for testing purposts.
# Expected api for T:
#      .next
#      .previous
#      address
#      isnull
mutable struct Freelist{T}
    base::T
end

Freelist{T}() where {T} = Freelist(T())

function Base.length(x::Freelist)
    count = 0
    item = x.base
    while !isnull(item)
        count += 1
        item = item.next
    end

    return count
end

Base.isempty(x::Freelist) = isnull(x.base)

function Base.pop!(x::Freelist{T}) where {T}
    # Pull out the base
    item = x.base

    # Set the base block to the next block
    # If this hasn't emptied the freelist, null out the previous field
    x.base = item.next
    if !isempty(x)
        x.base.previous = T()
    end
    item.next = T()
    return item
end

function Base.push!(x::Freelist{T}, item::T) where {T}
    if isempty(x)
        item.next = T()
    else
        @check isnull(x.base.previous)
        x.base.previous = item
        item.next = x.base
    end
    item.previous = T()
    x.base = item
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

function remove!(x::Freelist{T}, item::T) where {T}
    # Update the linked-list.
    if isnull(item.previous)
        x.base = item.next
    else
        item.previous.next = item.next
    end

    isnull(item.next) || (item.next.previous = item.previous)
    item.next = item.previous = T()
    return nothing
end

# Sort the freelist by some parameter
# uses a simple bubble sort - so probably not that fast.
function Base.sort!(x::Freelist; lt = isless)
    isempty(x) && return nothing

    while true
        swapped = false
        current = x.base

        next = current.next
        while !isnull(next)
            if lt(next, current)
                # Implicitly updates `block`.
                swap!(current, next)
                # Update the base element if needed.
                # Handling of `null` will be dealt with in `swap!`.
                current == x.base && (x.base = next)
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

function Base.iterate(x::Freelist, item = x.base)
    isnull(item) && return nothing
    return (item, item.next)
end

