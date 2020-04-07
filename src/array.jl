# Must make this mutable so we can attach a finalizer to it.
mutable struct CachedArray{T,N,C <: CacheManager} <: AbstractArray{T,N}
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

    # Lock to ensure only one thread can manage the store at a time.
    movelock::ReentrantLock

    # Should be conservative with this flag and always default to `true`.
    dirty::Bool

    # Inner constructor - do a type chack and make sure the finalizer is attached.
    function CachedArray{T,N}(
            array::Array{T,N},
            parent,
            manager::C = GlobalManager[],
        ) where {T,N,C}

        # Default settings.
        movelock = ReentrantLock()
        dirty = true

        A = new{T,N,C}(
            array,
            parent,
            manager,
            movelock,
            dirty,
        )

        # Attach finalizer.
        finalizer(cleanup, A)
        registerlocal!(manager, A)
        return A
    end
end

# Finalizer
function cleanup(A::CachedArray)
    # Clean up local storage on the manager
    if islocal(A)
        freelocal!(A.manager, A)
    end

    # Call `free` on the remote array and clean up remote statistics on the manager.
    if hasparent(A)
        freeremote!(A.manager, A)
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

#####
##### Array Interface
#####

Base.pointer(A::CachedArray) = pointer(A.array)
@inline Base.size(A::CachedArray) = size(A.array)
Base.sizeof(A::CachedArray) = sizeof(A.array)

Base.@propagate_inbounds @inline Base.getindex(A::CachedArray, i::Int) = A.array[i]
Base.@propagate_inbounds @inline Base.setindex!(A::CachedArray, v, i::Int) = setindex!(A.array, v, i)
Base.IndexStyle(::Type{<:CachedArray}) = Base.IndexLinear()

# "Similar" variants in all their glory!
# TODO: Think about how to propagate metadata ...
function Base.similar(::Type{<:CachedArray}, ::Type{S}, dims::Tuple{Vararg{Int64,N}}) where {S,N}
    return CachedArray{S}(undef, dims)
end

function Base.similar(::Type{<:CachedArray{T}}, dims::Tuple{Vararg{Int64,N}}) where {T,N}
    return similar(CachedArray, T, dims)
end

function Base.similar(A::CachedArray, ::Type{S}, dims::Tuple{Vararg{Int64,N}}) where {S,N}
    return similar(typeof(A), S, dims)
end

@inline Base.iterate(A::CachedArray) = iterate(A.array)
@inline Base.iterate(A::CachedArray, i) = iterate(A.array, i)

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

function prefetch!(A::CachedArray, force = false)
    # Quick path - if the array is alread local, we don't have to do anything.
    # This could be racey if something else is pushing this array to the remote store,
    # so we provide an option to force capturing the lock.
    if !force
        islocal(A) && return nothing
    end

    # Make sure we have the lock
    Base.@lock A.movelock begin
        islocal(A) && return nothing

        # Need to allocate a local array.
        localstorage = similar(_array(A))

        # TODO: Fast copy
        copyto!(localstorage, _array(A))
        registerlocal!(A.manager, A)

        # Update the CachedArray
        A.parent = _array(A)
        A.array = localstorage

        # Be pessimistic with the `dirty` flag.
        # This can be cleared by later uses, but shouldn't be the default.
        A.dirty = true
    end
end

function evict!(A::CachedArray, force = false)
    if !force
        isparent(A) && return nothing
    end

    Base.@lock A.movelock begin
        isparent(A) && return nothing

        # Check if this array has a parent and is dirty.
        # If so, just copy the local array to the parent.
        hp = hasparent(A)
        if A.dirty && hp
            copyto!(parent(A), _array(A))
            A.array = parent(A)
            freelocal!(A.manager, A)
            return nothing
        end

        # Regardless of whether this is clean or not, an eviction without a parent means
        # we have to create the parent.
        if !hp
            P = unsafe_remote_alloc(typeof(_array(A)), A.manager, size(A))
            A.array = P
            A.parent = P
            A.dirty = false

            # Maintain status in the cache manager.
            registerremote!(A.manager, A)
            freelocal!(A.manager, A)
        end
        return nothing
    end
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
