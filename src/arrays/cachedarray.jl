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
        status::S = NotBusy(),
    ) where {T,N,S,M}
        if !isbitstype(T)
            throw(ArgumentError("Cannot construct a `CachedArray` from non-isbits types!"))
        end
        return new{T,N,S,M}(region, dims)
    end
end

# Aliases
const ReadableCachedArray{T,N} = CachedArray{T,N,<:Readable,<:Any}
const UnreadableCachedArray{T,N} = CachedArray{T,N,NotBusy,<:Any}
const WritableCachedArray{T,N} = CachedArray{T,N,<:Writable,<:Any}
const UnwritableCachedArray{T,N} = CachedArray{T,N,<:Union{NotBusy,ReadOnly},<:Any}
const BusyCachedArray{T,N} = CachedArray{T,N,<:Union{ReadOnly,ReadWrite},<:Any}

@inline region(A::CachedArray) = A.region
metastyle(::CachedArray) = BlockMeta()
datapointer(A::CachedArray) = datapointer(region(A))
manager(A::CachedArray) = manager(region(A))

@inline unsafe_pointer(A::CachedArray{T}) where {T} = Ptr{T}(pointer(region(A)))

# Don't let functions normally take pointers to un-acquired CachedArrays.
Base.pointer(A::CachedArray) = unsafe_pointer(A)
function Base.pointer(A::CachedArray{<:Any,<:Any,NotBusy})
    error("Cannot take a pointer to to a `NotBusy` CachedArray")
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

function CachedArray{T}(::UndefInitializer, manager, i::Integer...) where {T}
    return CachedArray{T}(undef, manager, convert.(Int, i))
end

function CachedArray{T}(::UndefInitializer, manager, dims::NTuple{N,Int}; status = NotBusy()) where {T,N}
    isbitstype(T) || error("Can only create CachedArrays of `isbitstypes`!")
    region = alloc(manager, prod(dims) * sizeof(T))
    return CachedArray{T,N}(region, dims, status)
end

#####
##### Array Interface
#####

# Go through the "pointer" API to capture "NotBusy" arrays.
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
    dims::Tuple{Vararg{Int,N}} = size(A);
    status = ReadWrite(),
) where {T,N}
    CachedArray{T}(undef, manager(A), dims; status = status)
end

function Base.iterate(A::CachedArray{<:Any,<:Any,S}, i::Int = 1) where {S<:Readable}
    i > length(A) && return nothing
    return (@inbounds A[i], i + 1)
end

# For alias detection
Base.dataids(A::CachedArray) = (UInt(pointer(A)),)

function Base.reshape(
    x::CachedArray{T,M,S},
    dims::NTuple{N,Int},
) where {T,N,M,S}
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

#####
##### ArrayInterface
#####

# Need to fill these out to make LoopVectorization happy.
ArrayInterface.parent_type(x::CachedArray) = x
ArrayInterface.defines_strides(::Type{<:CachedArray}) = true
ArrayInterface.can_avx(::CachedArray) = true

# Axes
ArrayInterface.axes_types(::Type{<:CachedArray{T,N}}) where {T,N} =
    Tuple{Vararg{Base.OneTo{Int},N}}
ArrayInterface.contiguous_axis(::Type{<:CachedArray}) = ArrayInterface.One()
ArrayInterface.stride_rank(::Type{<:CachedArray{T,N}}) where {T,N} =
    ArrayInterface.nstatic(Val(N))
ArrayInterface.contiguous_batch_size(::Type{<:CachedArray{T,N}}) where {T,N} =
    ArrayInterface.Zero()
ArrayInterface.dense_dims(::Type{<:CachedArray{T,N}}) where {T,N} =
    ArrayInterface._all_dense(Val{N}())

ArrayInterface.device(::Type{<:CachedArray}) = ArrayInterface.CPUPointer()

#####
##### State Changes
#####

# N.B. - Once state tracking is implemented in the backend, make sure to use semaphores
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

# Automatic Conversion.
function Base.convert(::Type{CachedArray{T,N,NotBusy,M}}, x::CachedArray{T,N,S,M}) where {T,N,S,M}
    return release(x)
end
