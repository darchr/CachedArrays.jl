abstract type AbstractStatus end
struct Locked <: AbstractStatus end
struct ReadOnly <: AbstractStatus end
struct ReadWrite <: AbstractStatus end

const Readable = Union{ReadOnly, ReadWrite}
const Writable = ReadWrite

# Must make this mutable so we can attach a finalizer to it, unfortunately.
struct CachedArray{T,N,S<:AbstractStatus,M} <: DenseArray{T,N}
    region::Region{M}
    dims::NTuple{N,Int}

    # Inner constructor - do a type chack and make sure the finalizer is attached.
    function CachedArray{T,N}(
        region::Region{M},
        dims::NTuple{N,Int},
        status::S = ReadWrite(),
    ) where {T,N,S,M}

        if !isbitstype(T)
            throw(ArgumentError("Cannot construct a `CachedArray` from non-isbits types!"))
        end

        return new{T,N,S,M}(region, dims)
    end
end

# Get a pointer to the `ptr` field of a CachedArray object.
# ArrayInterface.size(A::CachedArray) = size(A)
# ArrayInterface.strides(A) = (ArrayInterface.static(1), Base.tail(strides(A))...)
# ArrayInterface.stride_rank(::Type{<:CachedArray{T,N}}) where {T,N} = ArrayInterface.nstatic(Val(N))
# ArrayInterface.contiguous_axis(::Type{<:CachedArray{T,N}}) where {T,N} = ArrayInterface.One()
# ArrayInterface.contiguous_batch_size(::Type{<:CachedArray{T,N}}) where {T,N} = ArrayInterface.Zero()

metastyle(::CachedArray) = BlockMeta()
datapointer(A::CachedArray) = datapointer(A.region)
manager(A::CachedArray) = manager(A.region)

#@inline Base.pointer(A::CachedArray) = A.ptr
@inline Base.pointer(A::CachedArray{T}) where {T} = Ptr{T}(pointer(A.region))

# utils
strip_params(::Type{<:CachedArray}) = CachedArray
strip_params(::T) where {T<:CachedArray} = strip_params(T)

#####
##### Constructors
#####

CachedArray(x::Array{T,N}, manager) where {T,N} = CachedArray{T,N}(x, manager)

function CachedArray{T,N}(x::Array{T,N}, manager) where {T,N}
    region = alloc(manager, sizeof(x))
    unsafe_copyto!(Ptr{T}(pointer(region)), pointer(x), length(x))
    return CachedArray{T,N}(region, size(x))
end

function CachedArray{T}(::UndefInitializer, manager, i::Integer) where {T}
    return CachedArray{T}(undef, manager, (convert(Int, i),))
end

function CachedArray{T}(::UndefInitializer, manager, dims::NTuple{N,Int}) where {T,N}
    isbitstype(T) || error("Can only create CachedArrays of `isbitstypes`!")
    region = alloc(manager, prod(dims) * sizeof(T))
    A = CachedArray{T,N}(region, dims)
    return A
end

#####
##### Array Interface
#####

Base.unsafe_convert(::Type{Ptr{T}}, A::CachedArray{T}) where {T} = pointer(A)
Base.sizeof(A::CachedArray) = prod(size(A)) * sizeof(eltype(A))
@inline Base.size(A::CachedArray) = A.dims

Base.elsize(::CachedArray{T}) where {T} = sizeof(T)
Base.elsize(::Type{<:CachedArray{T}}) where {T} = sizeof(T)

function Base.getindex(A::CachedArray{<:Any,<:Any,S}, i::Int) where {S <: Readable}
    @boundscheck checkbounds(A, i)
    return LoadStore.unsafe_custom_load(pointer(A), i)
    #return VectorizationBase.vload(VectorizationBase.stridedpointer(A), (i,))
    #return unsafe_load(pointer(A, i))
end

function Base.setindex!(A::CachedArray{<:Any,<:Any,S}, v, i::Int) where {S <: Writable}
    @boundscheck checkbounds(A, i)
    return LoadStore.unsafe_custom_store!(pointer(A), v, i)
    # return VectorizationBase.vstore!(VectorizationBase.stridedpointer(A), v, (i,))
    #return unsafe_store!(pointer(A, i), v)
end

Base.IndexStyle(::Type{<:CachedArray}) = Base.IndexLinear()

function Base.similar(
    A::CachedArray,
    eltyp::Type{T} = eltype(A),
    dims::Tuple{Vararg{Int,N}} = size(A),
) where {T,N}
    strip_params(A){eltyp}(undef, manager(A), dims)
end

function Base.iterate(A::CachedArray{<:Any,<:Any,S}, i::Int = 1) where {S <: Readable}
    i > length(A) && return nothing
    return (@inbounds A[i], i + 1)
end

# For alias detection
Base.dataids(A::CachedArray) = (UInt(pointer(A)),)

#####
##### Broadcasting
#####

# Hijack broadcasting so we prioritize CachedArrays.
function Base.BroadcastStyle(::Type{T}) where {T<:CachedArray}
    return Broadcast.ArrayStyle{CachedArray}()
end

# TODO - right now, we traverse through the BC object to find the first intance of an
# CachedArray.
#
# A more general solution would gather all such arrays and check that all the CachedArrays
# are the same.
function Base.similar(
    bc::Broadcast.Broadcasted{Broadcast.ArrayStyle{T}},
    ::Type{ElType},
) where {T<:CachedArray,ElType}
    cached = findT(T, bc)
    return similar(cached, ElType, axes(bc))
end

# We need to search through the GC object for the first instance of T
# Once we find it, return it.
findT(::Type{T}, bc::Base.Broadcast.Broadcasted) where {T} = findT(T, bc.args)
findT(::Type{T}, x::Tuple) where {T} = findT(T, findT(T, first(x)), Base.tail(x))
findT(::Type{T}, x::U) where {T,U<:T} = x
findT(::Type{T}, x) where {T} = nothing
findT(::Type{T}, ::Tuple{}) where {T} = nothing
findT(::Type{T}, x::U, rest) where {T,U<:T} = x
findT(::Type{T}, ::Any, rest) where {T} = findT(T, rest)

# We hit this case when there's Float32/Float64 confusion ...
findT(::Type{T}, x::Base.Broadcast.Extruded{U}) where {T,U<:T} = x.x

