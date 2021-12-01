struct Null end
CachedArrays.isnull(::Null) = true

mutable struct TestBlock
    isnull::Bool
    next::Union{TestBlock,Null}
    previous::Union{TestBlock,Null}
    address::Int
end

TestBlock(isnull = true, address::Integer = 0) = TestBlock(isnull, Null(), Null(), address)

CachedArrays.isnull(x::TestBlock) = x.isnull
CachedArrays.address(x::TestBlock) = x.address
Base.isless(a::TestBlock, b::TestBlock) = CachedArrays.address(a) < CachedArrays.address(b)

@testset "Testing Freelist" begin
    isnull(x) = CachedArrays.isnull(x)
    null = Null()

    @test isnull(TestBlock())

    @testset "Swapping" begin
        #####
        ##### Test Swapping - four cases:
        #####

        # Null - A - B - Null
        a = TestBlock(false, null, null, 0)
        b = TestBlock(false, null, null, 1)

        a.next = b
        b.previous = a
        CachedArrays.swap!(a, b)
        @test !isnull(a)
        @test !isnull(b)
        @test isnull(b.previous)
        @test b.next === a
        @test a.previous === b
        @test isnull(a.next)

        # C - A - B - Null
        a = TestBlock(false, null, null, 0)
        b = TestBlock(false, null, null, 1)
        c = TestBlock(false, null, null, 2)

        c.next = a

        a.next = b
        a.previous = c

        b.previous = a
        CachedArrays.swap!(a, b)

        # tests
        @test isnull(c.previous)
        @test c.next === b

        @test b.previous === c
        @test b.next === a

        @test a.previous === b
        @test isnull(a.next)

        # Null - A - B - D
        a = TestBlock(false, null, null, 0)
        b = TestBlock(false, null, null, 1)
        d = TestBlock(false, null, null, 2)

        a.next = b

        b.previous = a
        b.next = d

        d.previous = b
        CachedArrays.swap!(a, b)

        @test isnull(b.previous)
        @test b.next === a

        @test a.previous === b
        @test a.next === d
        @test d.previous === a
        @test isnull(d.next)

        # C - A - B - D
        c = TestBlock(false, null, null, 3)
        a = TestBlock(false, null, null, 0)
        b = TestBlock(false, null, null, 1)
        d = TestBlock(false, null, null, 2)

        c.next = a
        a.previous = c
        a.next = b
        b.previous = a
        b.next = d
        d.previous = b

        CachedArrays.swap!(a, b)
        @test isnull(c.previous)
        @test c.next === b
        @test b.previous === c
        @test b.next === a
        @test a.previous === b
        @test a.next === d
        @test d.previous === a
        @test isnull(d.next)
    end

    @testset "Freelist" begin
        list = CachedArrays.Freelist{TestBlock}()
        @test length(list) == 0
        @test isempty(list)
        @test issorted(list)

        # Push blocks in reverse order so we can test out sorting.
        block0 = TestBlock(false, 0)
        push!(list, block0)

        @test !isempty(list)
        @test length(list) == 1
        @test issorted(list)
        @test CachedArrays.getbase(list) === block0

        # Pop an empty list
        popped = pop!(list)
        @test popped === block0
        @test isempty(list)

        push!(list, block0)

        # Add another block, with a higher address
        block1 = TestBlock(false, 1)
        block1.previous = block0

        # Make sure `previous` fields get cleared
        @test !isnull(block1.previous)
        push!(list, block1)
        @test isnull(block1.previous)

        @test length(list) == 2
        @test !issorted(list)

        # Do some ordering checks
        blocks = collect(list)
        @test blocks[1] === block1
        @test blocks[2] === block0

        @test isnull(block1.previous)
        @test block1.next === block0
        @test block0.previous === block1
        @test isnull(block0.next)

        # Add yet another block
        block2 = TestBlock(false, 2)
        push!(list, block2)

        @test length(list) == 3
        blocks = collect(list)
        @test blocks[1] === block2
        @test blocks[2] === block1
        @test blocks[3] === block0

        # Try to pop a block off
        block = pop!(list)
        @test block === block2
        @test length(list) == 2

        # Make sure the block1 got nulled out.
        @test isnull(block1.previous)
        push!(list, block2)

        # Now try sorting
        @test length(list) == 3
        @test !issorted(list)
        sort!(list)
        @test length(list) == 3
        @test issorted(list)
        blocks = collect(list)
        @test blocks[1] === block0
        @test blocks[2] === block1
        @test blocks[3] === block2
    end

    @testset "FreelistPtr" begin
        list_ref = Ref(CachedArrays.Block())
        list = CachedArrays.FreelistPtr(
            Base.unsafe_convert(Ptr{CachedArrays.Block}, list_ref)
        )

        # Allocate a collection of blocks in an array to provide stable addresses
        nblocks = 3
        raw_memory = zeros(UInt8, nblocks * CachedArrays.headersize())
        raw_blocks = map(1:nblocks) do i
            return CachedArrays.Block(
                Ptr{Nothing}(pointer(raw_memory, 1 + i * CachedArrays.headersize()))
            )
        end

        block0 = raw_blocks[1]
        block1 = raw_blocks[2]
        block2 = raw_blocks[3]

        @test length(list) == 0
        @test isempty(list)
        @test issorted(list)

        # Push blocks in reverse order so we can test out sorting.
        push!(list, block0)

        @test !isempty(list)
        @test length(list) == 1
        @test issorted(list)
        @test CachedArrays.getbase(list) === block0

        # Pop an empty list
        popped = pop!(list)
        @test popped === block0
        @test isempty(list)

        push!(list, block0)

        # Add another block, with a higher address
        block1.previous = block0

        # Make sure `previous` fields get cleared
        @test !isnull(block1.previous)
        push!(list, block1)
        @test isnull(block1.previous)

        @test length(list) == 2
        @test !issorted(list)

        # Do some ordering checks
        blocks = collect(list)
        @test blocks[1] === block1
        @test blocks[2] === block0

        @test isnull(block1.previous)
        @test block1.next === block0
        @test block0.previous === block1
        @test isnull(block0.next)

        # Add yet another block
        push!(list, block2)

        @test length(list) == 3
        blocks = collect(list)
        @test blocks[1] === block2
        @test blocks[2] === block1
        @test blocks[3] === block0

        # Try to pop a block off
        block = pop!(list)
        @test block === block2
        @test length(list) == 2

        # Make sure the block1 got nulled out.
        @test isnull(block1.previous)
        push!(list, block2)

        # Now try sorting
        @test length(list) == 3
        @test !issorted(list)
        sort!(list)
        @test length(list) == 3
        @test issorted(list)
        blocks = collect(list)
        @test blocks[1] === block0
        @test blocks[2] === block1
        @test blocks[3] === block2
    end
end

