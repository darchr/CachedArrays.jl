@testset "Testing MaskTree" begin
    @testset "Mask" begin
        m = CachedArrays.Mask(0)
        @test CachedArrays.hasentry(m) == false
        for i in 1:64
            @test CachedArrays.hasentry(m) == false
        end

        for i in 1:64
            # Test with single bits set.
            m = CachedArrays.Mask(one(UInt64) << (i-1))
            @test CachedArrays.hasentry(m)
            @test CachedArrays.hasentryat(m, i)
            for j in 1:i-2
                @test CachedArrays.hasentry(m, j) == true
            end
            for j in i-1:64
                @test CachedArrays.hasentry(m, j) == false
            end

            # Now, set all kinds of bits
            m = CachedArrays.Mask((one(UInt64) << (i - 1)) - 1)
            # If i == 1, then the mask value is, in fact, zero
            if i > 1
                @test CachedArrays.hasentry(m)
            end

            for j in 1:i-2
                @test CachedArrays.hasentry(m, j) == true
            end
            for j in i-1:64
                @test CachedArrays.hasentryat(m, i) == false
                @test CachedArrays.hasentry(m, j) == false
            end
        end
    end

    @testset "MaskTree" begin
        # Start simple with just a 1-level tree
        tree = CachedArrays.MaskTree(64)
        @test length(tree.runs) == 1

        for i in 1:64
            CachedArrays.setentry!(tree, i)
            @test CachedArrays.hasentryat(tree.runs[end][1], i) == true
            CachedArrays.clearentry!(tree, i)
            @test CachedArrays.hasentryat(tree.runs[end][1], i) == false
        end

        # Okay, now we get a little wilder
    end
end
