@testset "Testing Block" begin
    # Test "Block"
    #
    # Make a 64 byte vector that our "block" will modify
    V = zeros(UInt64, 64)
    block = CachedArrays.Block(convert(Ptr{Nothing}, pointer(V)))

    @test pointer(block) == convert(Ptr{Nothing}, pointer(V))
    @test CachedArrays.address(block) == convert(UInt, pointer(block))

    ### printing code

    # Make sure printing null block doesn't segfault ...
    println(devnull, CachedArrays.Block())
    println(devnull, block)


    # Since we've started this from an array of all zeros, the size, next, and previous
    # blocks should all be zer.
    @test block.size == 0
    @test CachedArrays.isnull(block.next)
    @test CachedArrays.isnull(block.previous)
    @test block.free == false

    # Make sure that our configuration code for setting lower order bits doesn't affect
    # the bits it's not supposed to.
    freeness = (true, false)
    pools = (CachedArrays.DRAM, CachedArrays.PMM)
    sizes = (2^20, 2^30, 4096)
    orders = [
        (:size, :free, :pool),
        (:size, :pool, :free),
        (:free, :size, :pool),
        (:free, :pool, :size),
        (:pool, :size, :free),
        (:pool, :free, :size),
    ]

    for (sz, f, p) in Iterators.product(sizes, freeness, pools)
        nt = (
            size = sz,
            free = f,
            pool = p,
        )

        for order in orders
            for field in order
                setproperty!(block, field, nt[field])
            end
            @test block.size == sz
            @test block.free == f
            @test block.pool == p
        end
    end

    # Try changing the size
    block.size = 2^20
    @test block.size == 2^20
    block.free = true
    @test block.size == 2^20

    @test block.free
    @test CachedArrays.isfree(block)
    block.free = false
    @test !block.free

    x = Ptr{UInt64}(rand(UInt64))
    b = CachedArrays.Block(x)
    block.next = b
    @test block.next == b
    @test pointer(block.next) == x
end
