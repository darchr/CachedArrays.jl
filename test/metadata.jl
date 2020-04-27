mutable struct TestMeta <: CachedArrays.AbstractMetadata
    dirty::Bool
    id::UInt
    pool::CachedArrays.Pool
    sibling::Union{Nothing,TestMeta}
end

TestMeta(id) = TestMeta(false, id, CachedArrays.DRAM, nothing)

# Implement the API
CachedArrays.setdirty!(T::TestMeta, flag) = (T.dirty = flag)
CachedArrays.isdirty(T::TestMeta) = (T.dirty)
CachedArrays.getid(T::TestMeta) = T.id
CachedArrays.getpool(T::TestMeta) = T.pool
CachedArrays.getsibling(T::TestMeta) = T.sibling
CachedArrays.setsibling!(A::TestMeta, B::TestMeta) = (A.sibling = B)

struct ArrayWrapper
    array::Array
end

const TRACKER = IdDict{ArrayWrapper,TestMeta}

CachedArrays.metastyle(::ArrayWrapper) = TestMeta
CachedArrays.metadata(x::ArrayWrapper, ::Type{TestMeta})::TestMeta = TRACKER[x]

@testset "Testing Metadata API" begin
    x = rand(Float32, 100)
    A = ArrayWrapper(x)
    TRACKER[A] = TestMeta(0)

    @test CachedArrays.isdirty(A) == false
    CachedArrays.setdirty!(A, true)
    @test CachedArrays.isdirty(A) == true

    @test CachedArrays.getid(A) == zero(UInt)
    @test CachedArrays.getpool(A) == CachedArrays.DRAM
    @test isnothing(CachedArrays.getsibling(A))

    # Make another array and make a sibling for the array.
    B = ArrayWrapper(rand(Float32, 100))
    CachedArrays.setsibling!(A, B)

    @test CachedArrays.getsibling(A) === B

    # cleanup
    empty!(TRACKER)
end
