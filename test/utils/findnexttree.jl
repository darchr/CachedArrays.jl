@testset "Testing FindNextTree" begin
    # test ceiling integer division
    @test CachedArrays.cdiv(64, 64) == 1
    @test CachedArrays.cdiv(63, 64) == 1
    @test CachedArrays.cdiv(65, 64) == 2

    @test CachedArrays.cdiv(128, 64) == 2
    @test CachedArrays.cdiv(127, 64) == 2
    @test CachedArrays.cdiv(129, 64) == 3

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
            for j in 1:i-1
                @test CachedArrays.hasentry(m, j) == true
            end
            for j in i:64
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
                @test CachedArrays.hasentry(m, j) == false
            end
        end
    end

    @testset "FindNextTree" begin
        # Start simple with just a 1-level tree
        tree = CachedArrays.FindNextTree(64)
        @test length(tree.runs) == 1

        for i in 1:64
            CachedArrays.setentry!(tree, i)
            @test CachedArrays.hasentryat(tree.runs[end][1], i) == true
            CachedArrays.clearentry!(tree, i)
            @test CachedArrays.hasentryat(tree.runs[end][1], i) == false
        end

        # For now, test corner cases of interest.
        # Later, we'll move onto a longer stress test.
        setentry!(x...) = CachedArrays.setentry!(x...)
        clearentry!(x...) = CachedArrays.clearentry!(x...)
        numentries(x...) = CachedArrays.numentries(x...)

        # Okay, now we get a little wilder
        # Use a bit-array to provide a test against.
        len = 64^3
        tree = CachedArrays.FindNextTree(len)
        @test length(tree.runs) == 3

        # Right now, the top most level of the tree should have no entries.
        @test all(x -> iszero(numentries(x)), tree.runs[1])
        setentry!(tree, 1)

        # Make sure the top level of the tree got set.
        @test numentries(tree.runs[1][1]) == 1
        @test numentries(tree.runs[2][1]) == 1

        # If we set another bit in the same bin, it shouldn't change anything.
        setentry!(tree, 2)
        @test numentries(tree.runs[2][1]) == 1
        @test findnext(tree, 1) == 2
        @test findnext(tree, 2) == nothing

        setentry!(tree, 64)
        @test findnext(tree, 1) == 2
        @test findnext(tree, 2) == 64
        clearentry!(tree, 64)
        @test findnext(tree, 1) == 2
        @test findnext(tree, 2) == nothing

        setentry!(tree, 120)
        @test findnext(tree, 2) == 120
        clearentry!(tree, 120)
        @test findnext(tree, 2) == nothing

        # Set the entry at the end of the mask
        setentry!(tree, 64^3)
        @test findnext(tree, 2) == 64^3
        @test findnext(tree, 1) == 2
        clearentry!(tree, 64^3)
        @test findnext(tree, 2) == nothing
        clearentry!(tree, 1)
        @test findnext(tree, 1) == 2
        clearentry!(tree, 2)
        @test findnext(tree, 1) == nothing

        # Start stress testing.
        Random.seed!(1234)

        reference = falses(len)
        setentries = ones(Int64, 10000)
        search_indices = ones(Int64, 100000)

        @time for iter in 1:10
            # Populate indices to set
            for i in eachindex(setentries)
                setentries[i] = rand(1:len)
            end

            # Set entries
            for entry in setentries
                reference[entry] = true
                setentry!(tree, entry)
            end

            # Populate search indices
            for i in eachindex(search_indices)
                search_indices[i] = rand(1:len)
            end

            # Fuzz test
            for i in search_indices
                # There's a slight difference in behavior between the tree and the reference.
                # If an entry is set, `findnext(::BitArray, i)` will return `i`, where-as
                # the tree will still search for the next.
                #
                # So, if this entry is set, we expect the behavior to be different and skip
                # this test.
                reference[i] && continue

                if findnext(reference, i) != findnext(tree, i)
                    message = """
                    Failed on iteration $iter and index $i
                    Reference: $(findnext(reference, i))
                    Tree: $(findnext(tree, i))
                    """
                    error(message)
                end
            end

            # Clear entries for next round.
            for entry in setentries
                reference[entry] = false
                clearentry!(tree, entry)
            end
        end
    end
end
