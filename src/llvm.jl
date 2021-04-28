module LoadStore

# Use LLVM to generate the low level code for these aligned load and store operations.
#
# There are two main pain points to defining these ourselves:
#
# 1. Normal `unsafe_load` and `unsafe_store!` operations don't align based on the data size.
# 2. The normal pointer load/store instructions don't include TBAA data, which means LLVM
#    gets confused when generating code involving loads from these pointers and normal
#    arrays, which results in things not getting vectorized.
#
# This gets around both those issues using `LLVM.jl` to get the Julia `arraybuf` TBAA nodes
# and annotate the `load` and `store` instructions with TBAA access tags derived from the
# `arraybuf` struct.
#
# This should be legal as long as we make sure the types of our arrays are `isbitstype`s.
# If they aren't ... we're in BIG trouble.

using LLVM
using LLVM.Interop

# Type-based Alias Analysis for pointer accesses.
function arraybuf_tbaa(ctx)
    # Get the array_buf tbaa node.
    # Look in `codegen.cpp` for the name of this node.
    tbaa_root = MDNode([MDString("jtbaa", ctx)], ctx)

    tbaa_struct = MDNode([
        MDString("jtbaa_arraybuf", ctx),
        tbaa_root,
        LLVM.ConstantInt(0, ctx)
    ], ctx)

    tbaa_tag = MDNode([
        tbaa_struct,
        tbaa_struct,
        # Offset - must set to 0
        LLVM.ConstantInt(0, ctx),
        # Constant - set to 0
        LLVM.ConstantInt(0, ctx),
    ], ctx)

    return tbaa_tag
end

@generated function unsafe_custom_load(ptr::Ptr{T}, i::Integer=1) where {T}
    JuliaContext() do ctx
        # Convert the element type
        eltyp = convert(LLVMType, T, ctx)

        # LLVM Int and Pointer types
        T_int = LLVM.IntType(sizeof(Int)*8, ctx)
        T_ptr = LLVM.PointerType(eltyp)

        # Create a function
        paramtyps = [T_int, T_int]
        llvmf, _ = create_function(eltyp, paramtyps)

        # Generate IR
        Builder(ctx) do builder
            entry = BasicBlock(llvmf, "entry", ctx)
            position!(builder, entry)

            ptr = inttoptr!(builder, parameters(llvmf)[1], T_ptr)
            ptr = gep!(builder, ptr, [parameters(llvmf)[2]])

            val = load!(builder, ptr)

            # Assign alignment
            alignment!(val, sizeof(T))
            # Add tbaa data to match julia's `arraybuf` tbaa.
            metadata(val)[LLVM.MD_tbaa] = arraybuf_tbaa(ctx)

            ret!(builder, val)
        end

        return call_function(llvmf, T, Tuple{Ptr{T}, Int}, :(ptr, Int(i-1)))
    end
end

@generated function unsafe_custom_store!(ptr::Ptr{T}, v, i::Integer=1) where {T}
    JuliaContext() do ctx
        # Convert the element type
        eltyp = convert(LLVMType, T, ctx)

        # LLVM Int and Pointer Types
        T_int = LLVM.IntType(sizeof(Int) * 8, ctx)
        T_ptr = LLVM.PointerType(eltyp)

        # Create a function
        paramtyps = [T_int, eltyp, T_int]
        llvmf, _ = create_function(LLVM.VoidType(ctx), paramtyps)

        # Generate IR
        Builder(ctx) do builder
            entry = BasicBlock(llvmf, "entry", ctx)
            position!(builder, entry)

            ptr = inttoptr!(builder, parameters(llvmf)[1], T_ptr)
            ptr = gep!(builder, ptr, [parameters(llvmf)[3]])

            # Emit store instruction
            val = parameters(llvmf)[2]
            st = store!(builder, val, ptr)

            # Add alignment and tbaa information
            alignment!(st, sizeof(T))
            metadata(st)[LLVM.MD_tbaa] = arraybuf_tbaa(ctx)

            ret!(builder)
        end

        return call_function(
            llvmf,
            Cvoid,
            Tuple{Ptr{T}, T, Int},
            :(ptr, convert(T, v), Int(i-1)),
        )
    end
end

#####
##### For testing purposes.
#####

mutable struct UnsafeArray{T,N} <: DenseArray{T,N}
    ptr::Ptr{T}
    dims::NTuple{N,Int}
end

Base.pointer(x::UnsafeArray) = x.ptr
Base.size(x::UnsafeArray) = x.dims
Base.IndexStyle(::Type{<:UnsafeArray}) = Base.IndexLinear()

Base.getindex(x::UnsafeArray, i::Int) = unsafe_custom_load(pointer(x), i)
Base.setindex!(x::UnsafeArray, v, i::Int) = unsafe_custom_store!(pointer(x), v, i)

end
