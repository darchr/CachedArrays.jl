abstract type AbstractCachedArray{T,N} <: DenseArray{T,N} end

# Must make this mutable so we can attach a finalizer to it.
mutable struct CachedArray{T,N,C <: CacheManager} <: AbstractCachedArray{T,N}
    # This is the underlying data array.
    # When this array is remote, we use `unsafe_wrap` to wrap our own pointer.
    array::Array{T,N}

    # Pointer to the remote the far memory location of the array.
    # This could be null if the remote hasn't been allocated.
    #
    # This array `A` is in remote storate if `pointer(A) == A.remote_ptr`
    parent::Union{Nothing,Array{T,N}}

    # Hints regarding storage
    manager::CacheManager

    # Should be conservative with this flag and always default to `true`.
    dirty::Bool
    id::UInt

    # Inner constructor - do a type chack and make sure the finalizer is attached.
    function CachedArray{T,N}(
            array::Array{T,N},
            parent,
            manager::C = GlobalManager[],
        ) where {T,N,C}

        if !isbitstype(T)
            throw(ArgumentError("Cannot construct a `CachedArray` from non-isbits types!"))
        end

        # Default settings.
        dirty = true

        A = new{T,N,C}(
            array,
            parent,
            manager,
            dirty,
            getid(manager),
        )

        # Attach finalizer.
        #
        # Small arrays always live in the local memory, so we never have to worry about
        # a remote array being allocated and then being lost because the finalizer
        # is never called.
        if !issmall(A)
            finalizer(cleanup, A)
            registerlocal!(A)
        end
        return A
    end
end

CachedArray(x::Array{T,N}) where {T,N} = CachedArray{T,N}(x, nothing)

id(x::AbstractCachedArray) = x.id
manager(x::AbstractCachedArray) = x.manager

# Finalizer
function cleanup(A::CachedArray)
    issmall(A) && return nothing

    # Clean up local storage on the manager
    if islocal(A)
        freelocal!(A)
    end

    # Call `free` on the remote array and clean up remote statistics on the manager.
    if hasparent(A)
        freeremote!(A)
        MemKind.free(A.manager.kind, pointer(parent(A)))
    end
end

function CachedArray{T}(::UndefInitializer, i::Integer) where {T}
    return CachedArray{T}(undef, (convert(Int, i),))
end

function CachedArray{T}(::UndefInitializer, dims::NTuple{N,Int}) where {T,N}
    # Default alloc to near memory
    array = Array{T}(undef, dims)

    # Default the `remote_ptr` to a null ptr
    A = CachedArray{T,N}(
        array,
        nothing,
    )
    return A
end

# This function should never be called directly by external users.
_array(A::CachedArray) = A.array

parent(A::CachedArray) = A.parent
isparent(A::CachedArray) = (_array(A) === parent(A))
hasparent(A::CachedArray) = !isnothing(parent(A))

islocal(A::CachedArray) = isnothing(parent(A)) || !isparent(A)

isdirty(A::CachedArray) = A.dirty
isclean(A::CachedArray) = !isdirty(A)

# Check for small sized arrays
issmall(A::AbstractCachedArray) = sizeof(A) <= SMALL_ALLOC_SIZE

#####
##### Array Interface
#####

Base.pointer(A::AbstractCachedArray) = pointer(A.array)
Base.unsafe_convert(::Type{Ptr{T}}, A::AbstractCachedArray{T}) where {T} = pointer(A)
@inline Base.size(A::AbstractCachedArray) = size(A.array)
Base.sizeof(A::AbstractCachedArray) = sizeof(A.array)
Base.elsize(::AbstractCachedArray{T}) where {T} = sizeof(T)

Base.@propagate_inbounds @inline Base.getindex(A::AbstractCachedArray, i::Int) = A.array[i]
Base.@propagate_inbounds @inline Base.setindex!(A::AbstractCachedArray, v, i::Int) = setindex!(A.array, v, i)
Base.IndexStyle(::Type{<:AbstractCachedArray}) = Base.IndexLinear()

# "Similar" variants in all their glory!
# TODO: Think about how to propagate metadata ...
function Base.similar(::Type{<:CachedArray}, ::Type{S}, dims::Tuple{Vararg{Int64,N}}) where {S,N}
    return CachedArray{S}(undef, dims)
end

function Base.similar(::Type{array_style}, dims::Tuple{Vararg{Int64,N}}) where {T,array_style <: AbstractCachedArray{T},N}
    return similar(array_style, T, dims)
end

function Base.similar(A::AbstractCachedArray, ::Type{S}, dims::Tuple{Vararg{Int64,N}}) where {S,N}
    return similar(typeof(A), S, dims)
end

@inline Base.iterate(A::AbstractCachedArray) = iterate(A.array)
@inline Base.iterate(A::AbstractCachedArray, i) = iterate(A.array, i)

#####
##### Broadcasting
#####

# Hijack broadcasting so we prioritize CachedArrays.
const AbstractCachedStyle = Broadcast.ArrayStyle{AbstractCachedArray}
Base.BroadcastStyle(::Type{<:AbstractCachedArray}) = AbstractCachedStyle()

function Base.similar(bc::Broadcast.Broadcasted{AbstractCachedStyle}, ::Type{ElType}) where {ElType}
    return similar(CachedArray{ElType}, axes(bc))
end

#####
##### API for fetching and storing the array.
#####

function prefetch!(A::CachedArray; dirty = true)
    (issmall(A) || islocal(A)) && return nothing

    # Need to allocate a local array.
    localstorage = similar(_array(A))

    # TODO: Fast copy using AVX
    #copyto!(localstorage, _array(A))
    memcpy!(localstorage, _array(A))
    registerlocal!(A)

    # Update the CachedArray
    A.parent = _array(A)
    A.array = localstorage

    # Be pessimistic with the `dirty` flag.
    # This can be cleared by later uses, but shouldn't be the default.
    A.dirty = dirty
end

function evict!(A::CachedArray)
    (issmall(A) || isparent(A)) && return nothing

    # If this array does not have a parent and it's being evicted rather than freed,
    # Then we have to create a parent array for it.
    hp = hasparent(A)

    # If we just created the parent or if this array is dirty,
    # we must move it to the remote storage.
    if isdirty(A) || !hp
        move_to_remote!(A)
    end
    if !hp
        registerremote!(A)
    end
    freelocal!(A)
    A.dirty = false

    return nothing
end

function move_to_remote!(A::CachedArray)
    if !hasparent(A)
        A.parent = unsafe_remote_alloc(typeof(_array(A)), A.manager, size(A))
    end
    #copyto!(parent(A), _array(A))
    memcpy!(parent(A), _array(A), true)
    A.array = parent(A)

    return nothing
end


#####
##### Allocate remote arrays
#####

function remote_alloc(::Type{Array{T,N}}, manager::CacheManager, dims::NTuple{N,Int}) where {T,N}
    A = unsafe_remote_alloc(Array{T,N}, manager, dims)
    finalizer(A) do x
        MemKind.free(manager.kind, pointer(A))
    end
    return A
end

# Allocate an object in remote memory.
function unsafe_remote_alloc(
        ::Type{Array{T,N}},
        manager::CacheManager,
        dims::NTuple{N,Int}
    ) where {T,N}

    # Get the allocation size
    sz = sizeof(T) * prod(dims)

    # Perform the allocation, getting a raw pointer back
    ptr = convert(Ptr{T}, MemKind.malloc(manager.kind, sz))

    # Wrap the pointer in an array.
    # We don't set the finalizer in this function because there are times when we want to
    # manage the destruction of the array at a higher level.
    A = unsafe_wrap(Array, ptr, dims; own = false)

    return A
end


