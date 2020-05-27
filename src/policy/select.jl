# For now, just keep track of clean vs dirty data.
# Prioritize eviction of clean data.
mutable struct Select{T}
    clean::Set{T}
    dirty::Set{T}
end

Select{T}() where {T} = Select{T}(Set{T}(), Set{T}())

getset(x::Select{T}, v::T) where {T} = isdirty(v) ? x.dirty : x.clean

Base.eltype(::Select{T}) where {T} = T
fulleltype(::Select{T}) where {T} = T

Base.isempty(x::Select) = isempty(x.clean) && isempty(x.dirty)

Base.pop!(x::Select) = isempty(x.clean) ? pop!(x.dirty) : pop!(x.clean)
fullpop!(x::Select) = pop!(x)

function Base.push!(x::Select{T}, v::T) where {T}
    set = getset(x, v)
    @check !in(v, set)
    push!(set, v)
    return x
end

update!(x::Select{T}, v::T) where {T} = nothing

function Base.delete!(x::Select{T}, v::T) where {T}
    set = getset(x, v)
    @check in(v, set)
    return delete!(set, v)
end

function Base.in(v::T, x::Select{T}) where {T}
    set = getset(x, v)
    return in(v, set)
end

#####
##### Hints
#####

function setdirty!(x::Select{T}, v::T, flag) where {T}
    # Remove the item from the clean set and add it to the dirty set.
    @check in(v, x.clean) || in(v, x.dirty)
    if flag
        delete!(x.clean, v)
        push!(x.dirty, v)
    # Otherwise, do the opposite.
    else
        delete!(x.dirty, v)
        push!(x.clean, v)
    end
    return nothing
end
