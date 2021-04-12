abstract type AbstractCachedArray{T,N} <: DenseArray{T,N} end
metastyle(::AbstractCachedArray) = BlockMeta()

# Must make this mutable so we can attach a finalizer to it, unfortunately.
mutable struct CachedArray{T,N,C <: CacheManager} <: AbstractCachedArray{T,N}
    # This is the underlying data array.
    # When this array is remote, we use `unsafe_wrap` to wrap our own pointer.
    ptr::Ptr{T}
    dims::NTuple{N,Int}
    manager::CacheManager

    # Inner constructor - do a type chack and make sure the finalizer is attached.
    function CachedArray{T,N,C}(
            ptr::Ptr{T},
            dims::NTuple{N,Int},
            manager::C,
        ) where {T,N,C}

        if !isbitstype(T)
            throw(ArgumentError("Cannot construct a `CachedArray` from non-isbits types!"))
        end

        A = new{T,N,C}(ptr, dims, manager)
        register!(A)
        return A
    end
end

# Get a pointer to the `ptr` field of a CachedArray object.
datapointer(A::CachedArray) = pointer_from_objref(A) + fieldoffset(typeof(A), 1)

# Simplify Stacktraces
#function Base.show(io::IO, x::Type{<:AbstractCachedArray{T,N}}) where {T,N}
#    print(io, "CachedArrays.CachedArray{$T,$N}")
#end

#####
##### `Cacheable` Interface
#####

Base.pointer(A::AbstractCachedArray) = A.ptr

# We need to put a return type annotation because otherwise, Julia doesn't like
# inferring this.
function manager(x::CachedArray{T,N,C})::C where {T,N,C}
    return x.manager
end

function replace!(C::CachedArray{T}, ptr::Ptr{T}) where {T}
    # TODO: In `pendantic` mode, unsafe wrap both `ptrs` to check equality.
    C.ptr = ptr
    return nothing
end

# utils
strip_params(::Type{<:CachedArray}) = CachedArray
strip_params(::T) where {T <: AbstractCachedArray} = strip_params(T)

#####
##### Constructors
#####

CachedArray(x::Array{T,N}, manager) where {T,N} = CachedArray{T,N}(x, manager)

function CachedArray{T,N}(x::Array{T,N}, manager::C) where {T,N,C}
    ptr = lock(manager.lock) do
        unsafe_alloc(T, manager, sizeof(x))
    end

    unsafe_copyto!(ptr, pointer(x), length(x))
    return CachedArray{T,N,C}(ptr, size(x), manager)
end

function CachedArray{T}(::UndefInitializer, manager, i::Integer) where {T}
    return CachedArray{T}(undef, manager, (convert(Int, i),))
end

function CachedArray{T}(::UndefInitializer, manager::C, dims::NTuple{N,Int}) where {T,N,C}
    isbitstype(T) || error("Can only create CachedArrays of `isbitstypes`!")
    ptr = lock(manager.lock) do
        unsafe_alloc(T, manager, prod(dims) * sizeof(T))
    end
    A = CachedArray{T,N,C}(ptr, dims, manager)
    return A
end

#####
##### Array Interface
#####

Base.unsafe_convert(::Type{Ptr{T}}, A::AbstractCachedArray{T}) where {T} = pointer(A)
Base.sizeof(A::AbstractCachedArray) = prod(size(A)) * sizeof(eltype(A))
@inline Base.size(A::AbstractCachedArray) = A.dims

Base.elsize(::AbstractCachedArray{T}) where {T} = sizeof(T)
Base.elsize(::Type{<:AbstractCachedArray{T}}) where {T} = sizeof(T)

function Base.getindex(A::AbstractCachedArray, i::Int)
    @boundscheck checkbounds(A, i)
    return Base.unsafe_load(pointer(A), i)
end

function Base.setindex!(A::AbstractCachedArray, v, i::Int)
    @boundscheck checkbounds(A, i)
    return Base.unsafe_store!(pointer(A), v, i)
end

Base.IndexStyle(::Type{<:AbstractCachedArray}) = Base.IndexLinear()

function Base.similar(
        A::AbstractCachedArray,
        eltyp::Type{T} = eltype(A),
        dims::Tuple{Vararg{Int,N}} = size(A)
   ) where {T,N}

    strip_params(A){eltyp}(undef, manager(A), dims)
end

function Base.iterate(A::AbstractCachedArray, i::Int = 1)
    i > length(A) && return nothing
    return (@inbounds A[i], i+1)
end

# For alias detection
Base.dataids(A::AbstractCachedArray) = (UInt(pointer(A)),)

#####
##### Broadcasting
#####

# Hijack broadcasting so we prioritize CachedArrays.
function Base.BroadcastStyle(::Type{T}) where {T <: AbstractCachedArray}
    return Broadcast.ArrayStyle{strip_params(T)}()
end

# TODO - right now, we traverse through the BC object to find the first intance of an
# AbstractCachedArray.
#
# A more general solution would gather all such arrays and check that all the CachedArrays
# are the same.
function Base.similar(
        bc::Broadcast.Broadcasted{Broadcast.ArrayStyle{T}},
        ::Type{ElType}
    ) where {T <: AbstractCachedArray, ElType}

    cached = findT(T, bc)
    return similar(cached, ElType, axes(bc))
end

# We need to search through the GC object for the first instance of T
# Once we find it, return it.
findT(::Type{T}, bc::Base.Broadcast.Broadcasted) where {T} = findT(T, bc.args)
findT(::Type{T}, x::Tuple) where {T} = findT(T, findT(T, first(x)), Base.tail(x))
findT(::Type{T}, x::U) where {T, U <: T} = x
findT(::Type{T}, x) where {T} = nothing
findT(::Type{T}, ::Tuple{}) where {T} = nothing
findT(::Type{T}, x::U, rest) where {T, U <: T} = x
findT(::Type{T}, ::Any, rest) where {T} = findT(T, rest)

# We hit this case when there's Float32/Float64 confusion ...
findT(::Type{T}, x::Base.Broadcast.Extruded{U}) where {T, U <: T} = x.x

