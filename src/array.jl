abstract type AbstractStatus end
struct NotBusy <: AbstractStatus end
struct ReadOnly <: AbstractStatus end
struct ReadWrite <: AbstractStatus end

const Readable = Union{ReadOnly,ReadWrite}
const Writable = ReadWrite

struct CachedArray{T,N,S<:AbstractStatus,M} <: DenseArray{T,N}
    region::Region{M}
    dims::NTuple{N,Int}

    # Inner constructor - do a type check and make sure the finalizer is attached.
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

@inline region(A::CachedArray) = A.region
metastyle(::CachedArray) = BlockMeta()
datapointer(A::CachedArray) = datapointer(region(A))
manager(A::CachedArray) = manager(region(A))

@inline Base.pointer(A::CachedArray{T}) where {T} = Ptr{T}(pointer(region(A)))

# State Transitions
# N.B. - Oned state tracking is implemented in the backend, make sure to use semaphores
# since it's conceivable that multiple sections of code could be working with an array at
# a time.
#
# We could even go crazy and implement Rust like semantics about mutable and immutable
# copies (albeit much less strictly enforced on the compiler level)
readable(x::CachedArray{T,N,S}) where {T,N,S<:Readable} = x
function readable(x::CachedArray{T,N,S}) where {T,N,S}
    return CachedArray{T,N}(region(x), size(x), ReadOnly())
end

writable(x::CachedArray{T,N,S}) where {T,N,S<:Writable} = x
function writable(x::CachedArray{T,N,S}) where {T,N,S}
    return CachedArray{T,N}(region(x), size(x), ReadWrite())
end

# The behavior of these two is the same for the time being.
readwrite(x::CachedArray) = writable(x)

release(x::CachedArray{T,N,NotBusy}) where {T,N} = x
function release(x::CachedArray{T,N,S}) where {T,N,S}
    return CachedArray{T,N}(region(x), size(x), NotBusy())
end

#####
##### Constructors
#####

CachedArray(x::Array{T,N}, manager) where {T,N} = CachedArray{T,N}(x, manager)

function CachedArray{T,N}(x::Array{T,N}, manager) where {T,N}
    # Make sure we don't catch an interrupt between asking for an allocation and then
    # finishing.
    #
    # TODO: Maybe extend the allocation API to handle this automatically ...
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
    return CachedArray{T,N}(region, dims)
end

#####
##### Array Interface
#####

Base.unsafe_convert(::Type{Ptr{T}}, A::CachedArray{T}) where {T} = pointer(A)
Base.sizeof(A::CachedArray) = prod(size(A)) * sizeof(eltype(A))
@inline Base.size(A::CachedArray) = A.dims

Base.elsize(::CachedArray{T}) where {T} = sizeof(T)
Base.elsize(::Type{<:CachedArray{T}}) where {T} = sizeof(T)

function Base.getindex(A::CachedArray{<:Any,<:Any,S}, i::Int) where {S<:Readable}
    @boundscheck checkbounds(A, i)
    return LoadStore.unsafe_custom_load(pointer(A), i)
end

function Base.setindex!(A::CachedArray{<:Any,<:Any,S}, v, i::Int) where {S<:Writable}
    @boundscheck checkbounds(A, i)
    return LoadStore.unsafe_custom_store!(pointer(A), v, i)
end

Base.IndexStyle(::Type{<:CachedArray}) = Base.IndexLinear()

function Base.similar(
    A::CachedArray,
    eltyp::Type{T} = eltype(A),
    dims::Tuple{Vararg{Int,N}} = size(A),
) where {T,N}
    CachedArray{eltyp}(undef, manager(A), dims)
end

function Base.iterate(A::CachedArray{<:Any,<:Any,S}, i::Int = 1) where {S<:Readable}
    i > length(A) && return nothing
    return (@inbounds A[i], i + 1)
end

# For alias detection
Base.dataids(A::CachedArray) = (UInt(pointer(A)),)

function Base.reshape(x::CachedArray{T,M,S}, dims::NTuple{N,Int}) where {T,N,M,S}
    throw_dmrsa(dims, len) =
        throw(DimensionMismatch("new dimensions $(dims) must be consistent with array size $len"))

    if prod(dims) != length(x)
        throw_dmrsa(dims, length(x))
    end

    if N == M && dims == size(x)
        return x
    end

    # Keep the same underlying region.
    return CachedArray{T,N}(region(x), dims, S())
end

#####
##### Speedup functions
#####

function Base.copy(x::CachedArray)
    y = similar(x)
    unsafe_copyto!(pointer(y), pointer(x), length(x))
    return y
end

#####
##### Broadcasting
#####

# Hijack broadcasting so we prioritize CachedArrays.
function Base.BroadcastStyle(::Type{T}) where {T<:CachedArray}
    return Broadcast.ArrayStyle{CachedArray}()
end

function Base.BroadcastStyle(::Type{T}) where {U<:CachedArray,T<:SubArray{<:Any,<:Any,U}}
    return Broadcast.ArrayStyle{CachedArray}()
end

# TODO - right now, we traverse through the BC object to find the first intance of an
# CachedArray.
#
# A more general solution would gather all such arrays and check that all the CachedArrays
# are the same.
function Base.similar(
    bc::Broadcast.Broadcasted{Broadcast.ArrayStyle{CachedArray}},
    ::Type{ElType},
) where {ElType}
    cached = findT(CachedArray, bc)
    return similar(cached, ElType, axes(bc))
end

# We need to search through the `bc` object for the first instance of T
# Once we find it, return it.
findT(::Type{T}, bc::Base.Broadcast.Broadcasted) where {T} = findT(T, bc.args)
findT(::Type{T}, x::Tuple) where {T} = findT(T, findT(T, first(x)), Base.tail(x))
findT(::Type{T}, x::U) where {T,U<:T} = x
findT(::Type{T}, x::SubArray{<:Any,<:Any,U}) where {T,U<:T} = findT(T, parent(x))
findT(::Type{T}, x) where {T} = nothing
findT(::Type{T}, ::Tuple{}) where {T} = nothing
findT(::Type{T}, x::U, rest) where {T,U<:T} = x
findT(::Type{T}, ::Any, rest) where {T} = findT(T, rest)

# We hit this case when there's Float32/Float64 confusion ...
findT(::Type{T}, x::Base.Broadcast.Extruded{U}) where {T,U<:T} = x.x

