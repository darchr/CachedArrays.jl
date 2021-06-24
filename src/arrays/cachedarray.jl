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

region(A::CachedArray) = A.region
metastyle(::CachedArray) = BlockMeta()
datapointer(A::CachedArray) = datapointer(region(A))
manager(A::CachedArray) = manager(region(A))

# Escape hatch to ALWAYS get a pointer, regardless of the status of the array.
# Should only be called directly by `Base.pointer` or by the `CacheManager`.
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

function CachedArray{T,N}(x::Array{T,N}, manager; status = NotBusy()) where {T,N}
    region = alloc(manager, sizeof(x))
    unsafe_copyto!(Ptr{T}(pointer(region)), pointer(x), length(x))
    return CachedArray{T,N}(region, size(x), status)
end

function CachedArray{T}(::UndefInitializer, manager, i::Integer...; kw...) where {T}
    return CachedArray{T}(undef, manager, convert.(Int, i); kw...)
end

function CachedArray{T}(
    ::UndefInitializer,
    manager,
    dims::NTuple{N,Int};
    status = NotBusy(),
) where {T,N}
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

function Base.getindex(A::ReadableCachedArray, i::Int)
    @boundscheck checkbounds(A, i)
    return LoadStore.unsafe_custom_load(pointer(A), i)
end

function Base.setindex!(A::WritableCachedArray, v, i::Int)
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

function Base.iterate(A::ReadableCachedArray, i::Int = 1)
    i > length(A) && return nothing
    return (@inbounds A[i], i + 1)
end

# For alias detection
Base.dataids(A::CachedArray) = (UInt(pointer(A)),)

function Base.reshape(x::CachedArray{T,M,S}, dims::NTuple{N,Int}) where {T,N,M,S}
    throw_dmrsa(dims, len) = throw(
        DimensionMismatch("new dimensions $(dims) must be consistent with array size $len"),
    )

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

#####
##### ArrayInterface
#####

# Need to fill these out to make LoopVectorization happy.
ArrayInterface.parent_type(x::CachedArray) = x
ArrayInterface.defines_strides(::Type{<:CachedArray}) = true
ArrayInterface.can_avx(::CachedArray) = true

# Axes
function ArrayInterface.axes_types(::Type{<:CachedArray{T,N}}) where {T,N}
    return Tuple{Vararg{Base.OneTo{Int},N}}
end
ArrayInterface.contiguous_axis(::Type{<:CachedArray}) = ArrayInterface.One()
function ArrayInterface.stride_rank(::Type{<:CachedArray{T,N}}) where {T,N}
    return ArrayInterface.nstatic(Val(N))
end
function ArrayInterface.contiguous_batch_size(::Type{<:CachedArray{T,N}}) where {T,N}
    return ArrayInterface.Zero()
end
function ArrayInterface.dense_dims(::Type{<:CachedArray{T,N}}) where {T,N}
    return ArrayInterface._all_dense(Val{N}())
end

ArrayInterface.device(::Type{<:CachedArray}) = ArrayInterface.CPUPointer()

#####
##### Conversion Functions
#####

const __fnmap = [
    :NotBusy => :release,
    :ReadOnly => :readable,
    :ReadWrite => :writable,
]

for (typ, fn) in __fnmap
    # No-op if already correct type.
    @eval function $fn(x::CachedArray{T,N,$typ}) where {T,N}
        return x
    end

    # Adjust type parameter.
    @eval function $fn(x::CachedArray{T,N,S}) where {T,N,S}
        # Optional Telemetry
        @telemetry manager(x) begin
            telemetry_change(
                gettelemetry(manager(x)),
                getid(metadata(x)),
                $(QuoteNode(typ)),
                Symbol(S),
            )
        end
        return CachedArray{T,N}(region(x), size(x), $typ())
    end

    # Define `Base.convert` in terms of the "release, readable, writable" methods
    # defined above.
    @eval function Base.convert(
        ::Type{CachedArray{T,N,$typ,M}},
        x::CachedArray{T,N,S,M},
    ) where {T,N,S,M}
        return $fn(x)
    end
end

