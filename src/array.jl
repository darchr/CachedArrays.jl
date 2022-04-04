#####
##### Status Types
#####

# We use a type based strategy for controlling access to a CachedArray.
# Transitions from one status to another (e.g. NotBusy -> ReadOnly) will potentially
# trigger state updates in the manager/policy and record optional telemetry.
abstract type AbstractStatus end
struct NotBusy <: AbstractStatus end
struct ReadOnly <: AbstractStatus end
struct ReadWrite <: AbstractStatus end

_sym(::Type{NotBusy}) = :NotBusy
_sym(::Type{ReadOnly}) = :ReadOnly
_sym(::Type{ReadWrite}) = :ReadWrite

const Readable = Union{ReadOnly,ReadWrite}
const Writable = ReadWrite

######
###### CachedArray
######

struct CachedArray{T,N,S<:AbstractStatus,M} <: DenseArray{T,N}
    object::Object{M}
    dims::NTuple{N,Int}

    # Inner constructor - do a type check and make sure the finalizer is attached.
    function CachedArray{T,N}(
        object::Object{M}, dims::NTuple{N,Int}, status::S = NotBusy()
    ) where {T,N,S,M}
        if !isbitstype(T)
            throw(ArgumentError("Cannot construct a `CachedArray` from non-isbits types!"))
        end
        return new{T,N,S,M}(object, dims)
    end
end

# Aliases
const ReadableCachedArray{T,N} = CachedArray{T,N,<:Readable,<:Any}
const UnreadableCachedArray{T,N} = CachedArray{T,N,NotBusy,<:Any}
const WritableCachedArray{T,N} = CachedArray{T,N,<:Writable,<:Any}
const UnwritableCachedArray{T,N} = CachedArray{T,N,<:Union{NotBusy,ReadOnly},<:Any}
const BusyCachedArray{T,N} = CachedArray{T,N,<:Union{ReadOnly,ReadWrite},<:Any}

isreadonly(::CachedArray) = false
isreadonly(::UnwritableCachedArray) = true

# Cached API
object(A::CachedArray) = A.object
metadata(A::CachedArray) = metadata(object(A))
datapointer(A::CachedArray) = datapointer(object(A))
manager(A::CachedArray) = manager(object(A))
unsafe_free(A::CachedArray) = unsafe_free(object(A))

# Escape hatch to ALWAYS get a pointer, regardless of the status of the array.
# Should only be called directly by `Base.pointer` or by the `CacheManager`.
@inline unsafe_pointer(A::CachedArray{T}) where {T} = Ptr{T}(pointer(object(A)))

# Don't let functions normally take pointers to un-acquired CachedArrays.
Base.pointer(A::CachedArray) = unsafe_pointer(A)
function Base.pointer(A::UnreadableCachedArray)
    return error("Cannot take a pointer to to a `NotBusy` CachedArray")
end

# Consructors
CachedArray(x::Array{T,N}, manager; kw...) where {T,N} = CachedArray{T,N}(x, manager; kw...)
function CachedArray{T,N}(
    x::Array{T,N}, manager; status = ReadWrite(), priority = PreferLocal
) where {T,N}
    object = alloc(manager, sizeof(x), priority)
    unsafe_copyto!(Ptr{T}(pointer(object)), pointer(x), length(x))
    return CachedArray{T,N}(object, size(x), status)
end

function CachedArray{T}(::UndefInitializer, manager, i::Integer...; kw...) where {T}
    return CachedArray{T}(undef, manager, convert.(Int, i); kw...)
end

function CachedArray{T}(
    ::UndefInitializer,
    manager,
    dims::NTuple{N,Int};
    status = NotBusy(),
    priority = PreferLocal,
) where {T,N}
    object = alloc(manager, prod(dims) * sizeof(T), priority)
    return CachedArray{T,N}(object, dims, status)
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
    ::Type{T} = eltype(A),
    dims::Tuple{Vararg{Int,N}} = size(A);
    status = ReadWrite(),
    priority = ForceLocal,
) where {T,N}
    return CachedArray{T}(undef, manager(A), dims; status = status, priority = priority)
end

function Base.iterate(A::ReadableCachedArray, i::Int = 1)
    i > length(A) && return nothing
    return (@inbounds A[i], i + 1)
end

# For alias detection
Base.dataids(A::CachedArray) = (UInt(pointer(A)),)

function Base.reshape(x::CachedArray{T,M,S}, dims::NTuple{N,Int}) where {T,N,M,S}
    function throw_dmrsa(dims, len)
        return throw(
            DimensionMismatch(
                "new dimensions $(dims) must be consistent with array size $len"
            ),
        )
    end

    if prod(dims) != length(x)
        throw_dmrsa(dims, length(x))
    end

    if N == M && dims == size(x)
        return x
    end

    # Keep the same underlying object.
    return CachedArray{T,N}(object(x), dims, S())
end

######
###### ArrayInterface
######

# Hack to deal with all the `ArrayInterface` stuff.
# TODO: Implement the `ArrayInterface` correctly.
#ArrayInterface.parent_type(::Type{<:CachedArray{T,N}}) where {T,N} = Array{T,N}
#ArrayInterface.strides(A::CachedArray) = Base.strides(A)

function ArrayInterface.dense_dims(::Type{<:CachedArray{T,N}}) where {T,N}
    return ntuple(Returns(ArrayInterface.True()), Val(N))
end

# # Traits
# ArrayInterface.can_change_size(::Type{<:CachedArray}) = false
# ArrayInterface.can_setindex(::Type{<:WritableCachedArray}) = true
# ArrayInterface.contiguous_axis(::Type{<:CachedArray{T,N}}) where {T,N} = ArrayInterface.One()
# ArrayInterface.size(A::CachedArray) = size(A)
# ArrayInterface.strides(A) = strides(A)
# ArrayInterface.defines_strides(::Type{<:CachedArray}) = true
# ArrayInterface.fast_scalar_indexing(::Type{<:CachedArray}) = true
# ArrayInterface.has_parent(::Type{<:CachedArray}) = false
# ArrayInterface.is_column_major(::Type{<:CachedArray}) = true

function test_array_interface(a, b)
    # Type based functions
    type_fns = [
        ArrayInterface.can_change_size,
        ArrayInterface.can_setindex,
        ArrayInterface.contiguous_axis,
        ArrayInterface.contiguous_axis_indicator,
        ArrayInterface.contiguous_batch_size,
        ArrayInterface.defines_strides,
        ArrayInterface.device,
        ArrayInterface.dimnames,
        ArrayInterface.fast_matrix_colors,
        ArrayInterface.fast_scalar_indexing,
        ArrayInterface.has_dimnames,
        ArrayInterface.has_parent,
        ArrayInterface.is_column_major,
        ArrayInterface.is_lazy_conjugate,
        ArrayInterface.ismutable,
        ArrayInterface.isstructured,
        ArrayInterface.is_splat_index,
        ArrayInterface.known_length,
        ArrayInterface.known_offsets,
        ArrayInterface.known_size,
        ArrayInterface.known_step,
        ArrayInterface.known_strides,
        ArrayInterface.ndims_index,
        ArrayInterface.axes,
        #ArrayInterface.axes_types,
        ArrayInterface.dense_dims,
        ArrayInterface.indices,
        ArrayInterface.offset1,
        ArrayInterface.offsets,
        #ArrayInterface.parent_type,
        ArrayInterface.size,
        ArrayInterface.strides,
    ]

    for fn in type_fns
        @show fn
        @assert fn(a) == fn(b)
    end
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
    bc::Broadcast.Broadcasted{Broadcast.ArrayStyle{CachedArray}}, ::Type{ElType}
) where {ElType}
    cached = findT(CachedArray, bc)
    return similar(cached, ElType, axes(bc))
end

#####
##### Conversion Functions
#####

const __fnmap = [:NotBusy => :release, :ReadOnly => :readable, :ReadWrite => :writable]
const __updates = Dict(
    # Implications of making an array notbusy.
    # 1. TODO:
    :release => [],
    # Implications of making an array readable.
    # 1. TODO: Usage should be updated
    :readable => [],
    # Implications of making an array writable.
    # 1. Array should be marked as dirty.
    # 2. TODO: Usage should be updated
    :writable => [:(setdirty!(x))],
)

for (typ, fn) in __fnmap
    # No-op if already correct type.
    @eval $fn(x::CachedArray{<:Any,<:Any,$typ}) = x
    @eval function $fn(x::CachedArray{T,N}) where {T,N}
        if !isnull(unsafe_pointer(x.object))
            # Optional Telemetry
            @telemetry manager(x) begin
                telemetry_change(
                    gettelemetry(manager(x)), getid(metadata(x)), $(QuoteNode(typ))
                )
            end
            # unpack any potential policy updates.
            $(__updates...)
        end
        return CachedArray{T,N}(object(x), size(x), $typ())
    end

    # Define `Base.convert` in terms of the "release, readable, writable" methods
    # defined above.
    @eval function Base.convert(
        ::Type{CachedArray{T,N,$typ,M}}, x::CachedArray{T,N,S,M}
    ) where {T,N,S,M}
        return $fn(x)
    end

    # ChainRules
end

# ChainRules
function ChainRulesCore.rrule(::typeof(readable), x::CachedArray)
    return (readable(x), dy -> (ChainRulesCore.NoTangent(), dy))
end
function ChainRulesCore.rrule(::typeof(writable), x::CachedArray)
    return (writable(x), dy -> (ChainRulesCore.NoTangent(), dy))
end
function ChainRulesCore.rrule(::typeof(release), x::CachedArray)
    return (release(x), dy -> (ChainRulesCore.NoTangent(), dy))
end

