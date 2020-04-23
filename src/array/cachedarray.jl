abstract type AbstractCachedArray{T,N} <: DenseArray{T,N} end

# Must make this mutable so we can attach a finalizer to it, unfortunately.
mutable struct CachedArray{T,N,C <: CacheManager} <: AbstractCachedArray{T,N}
    # This is the underlying data array.
    # When this array is remote, we use `unsafe_wrap` to wrap our own pointer.
    array::Array{T,N}
    manager::CacheManager

    # Inner constructor - do a type chack and make sure the finalizer is attached.
    function CachedArray{T,N,C}(
            array::Array{T,N},
            manager::C,
        ) where {T,N,C}

        if !isbitstype(T)
            throw(ArgumentError("Cannot construct a `CachedArray` from non-isbits types!"))
        end

        A = new{T,N,C}(
            array,
            manager,
        )

        register!(PoolType{DRAM}(), manager, A)
        finalizer(cleanup, A)
        return A
    end
end

# Callback, update the backing array for this CachedArray.
update_pointer!(C::CachedArray{T,N}, A::Array{T,N}) where {T,N} = (C.array = A)
arraytype(C::CachedArray{T,N}) where {T,N} = Array{T,N}

# Get the metadata block for this cached array.
Block(A::CachedArray) = unsafe_block(pointer(A))

function CachedArray{T,N}(x::Array{T,N}, parent, manager::C = GlobalManager[]) where {T,N,C}
    _x = unsafe_alloc(PoolType{DRAM}(), manager, typeof(x), size(x))
    copyto!(_x, x)
    return CachedArray{T,N,C}(_x, manager)
end

CachedArray(x::Array{T,N}) where {T,N} = CachedArray{T,N}(x, nothing)
manager(x::AbstractCachedArray) = x.manager

function CachedArray{T}(::UndefInitializer, i::Integer) where {T}
    return CachedArray{T}(undef, (convert(Int, i),))
end

function CachedArray{T}(
        ::UndefInitializer,
        dims::NTuple{N,Int},
        manager::C = GlobalManager[],
    ) where {T,N,C}

    array = unsafe_alloc(PoolType{DRAM}(), manager, Array{T,N}, dims)

    # Default the `remote_ptr` to a null ptr
    A = CachedArray{T,N,C}(array, manager)
    return A
end

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

function Base.similar(::Type{A}, dims::Tuple{Vararg{Int64,N}}) where {T,A <: AbstractCachedArray{T},N}
    return similar(A, T, dims)
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
const CachedStyle = Broadcast.ArrayStyle{CachedArray}

Base.BroadcastStyle(::Type{<:CachedArray}) = CachedStyle()
function Base.similar(bc::Broadcast.Broadcasted{CachedStyle}, ::Type{ElType}) where {ElType}
    return similar(CachedArray{ElType}, axes(bc))
end

#####
##### API for fetching and storing the array.
#####

# # Prefetch `A` into local memory.
# # If `A` is already local, don't do any updates, including anything with the `dirty` flag.
# # If `A` is remote, fetch it and set the dirty flag.
# function prefetch!(A::CachedArray; dirty = true)
#     # If already local, update position in the LRU
#     if islocal(A)
#         updatelocal!(A)
#         return nothing
#     end
#
#     # Need to allocate a local array.
#     #localstorage = similar(_array(A))
#     localstorage = unsafe_local_alloc(manager(A), typeof(_array(A)), size(_array(A)), id(A))
#
#     # TODO: Fast copy using AVX
#     memcpy!(localstorage, _array(A))
#
#     # Update the CachedArray
#     A.parent = _array(A)
#     A.array = localstorage
#
#     # Register with the cache manager
#     registerlocal!(A)
#     A.dirty = dirty
# end
#
# function evict!(A::CachedArray)
#     isremote(A) && return nothing
#
#     # If this array does not have a parent and it's being evicted rather than freed,
#     # Then we have to create a parent array for it.
#     hp = hasparent(A)
#
#     # If we just created the parent or if this array is dirty,
#     # we must move it to the remote storage.
#     move_to_remote!(A)
#     if !hp
#         registerremote!(A)
#     end
#     freelocal!(A)
#     A.dirty = false
#
#     return nothing
# end
#
# function move_to_remote!(A::CachedArray)
#     hp = hasparent(A)
#     dirty = isdirty(A)
#     if !hp
#         A.parent = remote_alloc(manager(A), typeof(_array(A)), size(A))
#     end
#
#     # Write back if the array is dirty of if the parent was just created.
#     if dirty || !hp
#         memcpy!(parent(A), _array(A), true)
#     end
#
#     # Clean up the local array.
#     local_free(manager(A), _array(A))
#     A.array = parent(A)
#
#     return nothing
# end
#
