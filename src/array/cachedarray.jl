abstract type AbstractCachedArray{T,N} <: DenseArray{T,N} end
metastyle(::AbstractCachedArray) = BlockMeta()

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

        A = new{T,N,C}(array, manager)
        register!(A)
        return A
    end
end

#####
##### `Cacheable` Interface
#####

Base.pointer(A::AbstractCachedArray) = pointer(A.array)

# We need to put a return type annotation because otherwise, Julia doesn't like
# inferring this.
function manager(x::CachedArray{T,N,C})::C where {T,N,C}
    return x.manager
end

function replace!(C::CachedArray{T,N}, A::Array{T,N}) where {T,N}
    PEDANTIC && @assert C.array == A

    C.array = A
    return nothing
end
arraytype(C::CachedArray{T,N}) where {T,N} = Array{T,N}

# utils
strip_params(::Type{<:CachedArray}) = CachedArray
strip_params(::T) where {T <: AbstractCachedArray} = strip_params(T)

#####
##### Constructors
#####

function CachedArray{T,N}(x::Array{T,N}, manager::C) where {T,N,C}
    _x = unsafe_alloc(PoolType{DRAM}(), manager, typeof(x), size(x))
    copyto!(_x, x)
    return CachedArray{T,N,C}(_x, manager)
end

function CachedArray{T}(::UndefInitializer, manager, i::Integer) where {T}
    return CachedArray{T}(undef, manager, (convert(Int, i),))
end

function CachedArray{T}(::UndefInitializer, manager::C, dims::NTuple{N,Int}) where {T,N,C}
    array = unsafe_alloc(PoolType{DRAM}(), manager, Array{T,N}, dims)
    A = CachedArray{T,N,C}(array, manager)
    return A
end

#####
##### Array Interface
#####

Base.unsafe_convert(::Type{Ptr{T}}, A::AbstractCachedArray{T}) where {T} = pointer(A)
@inline Base.size(A::AbstractCachedArray) = size(A.array)
Base.sizeof(A::AbstractCachedArray) = sizeof(A.array)
Base.elsize(::AbstractCachedArray{T}) where {T} = sizeof(T)

Base.@propagate_inbounds @inline Base.getindex(A::AbstractCachedArray, i::Int) = A.array[i]
Base.@propagate_inbounds @inline Base.setindex!(A::AbstractCachedArray, v, i::Int) = setindex!(A.array, v, i)
Base.IndexStyle(::Type{<:AbstractCachedArray}) = Base.IndexLinear()

function Base.similar(
        A::AbstractCachedArray,
        eltyp::Type{T} = eltype(A),
        dims::Tuple{Vararg{Int,N}} = size(A)
   ) where {T,N}

    strip_params(A){eltyp}(undef, manager(A), dims)
end

@inline Base.iterate(A::AbstractCachedArray) = iterate(A.array)
@inline Base.iterate(A::AbstractCachedArray, i) = iterate(A.array, i)

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

#####
##### `aligned_unsafe_load` and `aligned_unsafe_store!`
#####

# Julia pessimizes loading and storing from pointers - defaulting to unaligned loads
# and stores.
#
# This can be problematic when we're trying to get code to vertorize.
#
# Here, we specialize our getindex for getting items from our arrays because we can guarentee
# alignment
#
# TODO: How do we deal with general `isbits` types?

const llvmstring = Dict{DataType,String}(
    # signed
    Bool    => "i8",
    Int8    => "i8",
    Int16   => "i16",
    Int32   => "i32",
    Int64   => "i64",
    Int128  => "i128",

    # unsigned
    UInt8   => "i8",
    UInt16  => "i16",
    UInt32  => "i32",
    UInt64  => "i64",
    UInt128 => "i128",

    # float
    Float32 => "float",
    Float64 => "double",
)

#const AlignedTypes = Union{Bool,Int8,Int16,Int32,Int64,Int128,UInt8,UInt16,UInt32,UInt64,UInt128,Float32,Float64}

@generated function aligned_unsafe_load(ptr::Ptr{T}, i::Integer = 1) where {T}
    llvmtype = llvmstring[T]

    str = """
        %3 = add i64 %1, -1
        %4 = inttoptr i64 %0 to $llvmtype*
        %5 = getelementptr inbounds $llvmtype, $llvmtype* %4, i64 %3
        %6 = load $llvmtype, $llvmtype* %5, align $(sizeof(T))
        ret $llvmtype %6
        """

    return :(Base.llvmcall($str, $T, Tuple{UInt64,Int64}, convert(UInt64, ptr), i))
end

@generated function aligned_unsafe_store!(ptr::Ptr{T}, x, i::Integer = 1) where {T}
    llvmtype = llvmstring[T]

    str = """
        %4 = add i64 %2, -1
        %5 = inttoptr i64 %0 to $llvmtype*
        %6 = getelementptr inbounds $llvmtype, $llvmtype* %5, i64 %4
        store $llvmtype %1, $llvmtype* %6, align $(sizeof(T))
        ret i64 %0
        """

    return quote
        Base.llvmcall(
            $str,
            Ptr{$T},
            Tuple{UInt64,$T,Int64},
            convert(UInt64, ptr),
            convert($T, x),
            convert(Int64, i)
        )
    end
end

