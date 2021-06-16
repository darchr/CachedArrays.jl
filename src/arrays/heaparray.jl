struct HeapArray{T,N,M} <: DenseArray{T,N}
    region::HeapRegion{M}
    dims::NTuple{N,Int}
    function HeapArray{T,N}(region::HeapRegion{M}, dims::NTuple{N,Int}) where {T,N,M}
        if !isbitstype(T)
            throw(ArgumentError("Cannot construct a `HeapArray` from non-isbits types!"))
        end
        return new{T,N,M}(region, dims)
    end
end

region(A::HeapArray) = A.region
manager(A::HeapArray) = manager(region(A))
Base.pointer(A::HeapArray{T}) where {T} = Ptr{T}(pointer(region(A)))

#####
##### Constructors
#####

HeapArray(x::Array{T,N}, manager) where {T,N} = HeapArray{T,N}(x, manager)
function HeapArray{T,N}(x::Array{T,N}, manager) where {T,N}
    # Make sure we don't catch an interrupt between asking for an allocation and then
    # finishing.
    #
    # TODO: Maybe extend the allocation API to handle this automatically ...
    region = alloc(manager, sizeof(x))
    unsafe_copyto!(Ptr{T}(pointer(region)), pointer(x), length(x))
    return HeapArray{T,N}(region, size(x))
end

function HeapArray{T}(::UndefInitializer, manager, i::Integer...) where {T}
    return HeapArray{T}(undef, manager, convert.(Int, i),)
end

function HeapArray{T}(::UndefInitializer, manager, dims::NTuple{N,Int}) where {T,N}
    region = alloc(manager, prod(dims) * sizeof(T))
    return HeapArray{T,N}(region, dims)
end

#####
##### Array Interface
#####

Base.unsafe_convert(::Type{Ptr{T}}, A::HeapArray{T}) where {T} = pointer(A)
Base.sizeof(A::HeapArray) = prod(size(A)) * sizeof(eltype(A))
@inline Base.size(A::HeapArray) = A.dims

Base.elsize(::HeapArray{T}) where {T} = sizeof(T)
Base.elsize(::Type{<:HeapArray{T}}) where {T} = sizeof(T)

function Base.getindex(A::HeapArray, i::Int)
    @boundscheck checkbounds(A, i)
    return LoadStore.unsafe_custom_load(pointer(A), i)
end

function Base.setindex!(A::HeapArray, v, i::Int)
    @boundscheck checkbounds(A, i)
    return LoadStore.unsafe_custom_store!(pointer(A), v, i)
end

Base.IndexStyle(::Type{<:HeapArray}) = Base.IndexLinear()

function Base.similar(
    A::HeapArray,
    eltyp::Type{T} = eltype(A),
    dims::Tuple{Vararg{Int,N}} = size(A)
) where {T,N}
    HeapArray{T}(undef, manager(A), dims)
end

function Base.iterate(A::HeapArray, i::Int = 1)
    i > length(A) && return nothing
    return (@inbounds A[i], i + 1)
end

# For alias detection
Base.dataids(A::HeapArray) = (UInt(pointer(A)),)

function Base.reshape(
    x::HeapArray{T,M},
    dims::NTuple{N,Int},
) where {T,N,M}
    throw_dmrsa(dims, len) =
        throw(DimensionMismatch("new dimensions $(dims) must be consistent with array size $len"))

    if prod(dims) != length(x)
        throw_dmrsa(dims, length(x))
    end

    if N == M && dims == size(x)
        return x
    end

    # Keep the same underlying region.
    return HeapArray{T,N}(region(x), dims)
end

#####
##### Speedup functions
#####

function Base.copy(x::HeapArray)
    y = similar(x)
    unsafe_copyto!(pointer(y), pointer(x), length(x))
    return y
end

#####
##### Broadcasting
#####

# Hijack broadcasting so we prioritize HeapArrays.
function Base.BroadcastStyle(::Type{T}) where {T<:HeapArray}
    return Broadcast.ArrayStyle{HeapArray}()
end

function Base.BroadcastStyle(::Type{T}) where {U<:HeapArray,T<:SubArray{<:Any,<:Any,U}}
    return Broadcast.ArrayStyle{HeapArray}()
end

# TODO - right now, we traverse through the BC object to find the first intance of an
# HeapArray.
#
# A more general solution would gather all such arrays and check that all the HeapArrays
# are the same.
function Base.similar(
    bc::Broadcast.Broadcasted{Broadcast.ArrayStyle{HeapArray}},
    ::Type{ElType},
) where {ElType}
    cached = findT(HeapArray, bc)
    return similar(cached, ElType, axes(bc))
end

#####
##### ArrayInterface
#####

# Need to fill these out to make LoopVectorization happy.
ArrayInterface.parent_type(x::HeapArray) = x
ArrayInterface.defines_strides(::Type{<:HeapArray}) = true
ArrayInterface.can_avx(::HeapArray) = true

# Axes
ArrayInterface.axes_types(::Type{<:HeapArray{T,N}}) where {T,N} =
    Tuple{Vararg{Base.OneTo{Int},N}}
ArrayInterface.contiguous_axis(::Type{<:HeapArray}) = ArrayInterface.One()
ArrayInterface.stride_rank(::Type{<:HeapArray{T,N}}) where {T,N} =
    ArrayInterface.nstatic(Val(N))
ArrayInterface.contiguous_batch_size(::Type{<:HeapArray{T,N}}) where {T,N} =
    ArrayInterface.Zero()
ArrayInterface.dense_dims(::Type{<:HeapArray{T,N}}) where {T,N} =
    ArrayInterface._all_dense(Val{N}())

ArrayInterface.device(::Type{<:HeapArray}) = ArrayInterface.CPUPointer()
