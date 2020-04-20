# @testset "Testing LRU Cache" begin
#     # Test `priority`
#     x = CachedArrays.Priority(0, 0, 10)
#     y = CachedArrays.Priority(1, 5, 20)
#     @test CachedArrays.getsize(x) == 10
#     @test CachedArrays.getsize(y) == 20
#
#     @test x < y
#     @test y > x
#     @test x != y
#
#     z = CachedArrays.Priority(0, 2, 5)
#     @test z == x
#     @test hash(z) == hash(x)
#
#     # Now, construct the LRU cache and try some operations on it.
#     # Construct a callback that mutates a global vector so we can see which items have
#     # been evicted.
#     evictions = Int[]
#     callback = x -> push!(evictions, x)
#
#     maxsize = 2
#     cache = CachedArrays.LRUCache{Int}(maxsize)
#     @test CachedArrays.freespace(cache) == maxsize
#     @test CachedArrays.currentsize(cache) == 0
#
#     # The three items we are going to be playing with.
#     A = 1
#     B = 2
#     C = 3
#
#     push!(cache, A, 1; cb = callback)
#     @test CachedArrays.currentsize(cache) == 1
#     @test CachedArrays.freespace(cache) == maxsize - 1
#     @test in(A, cache)
#     @test !in(B, cache)
#     @test !in(C, cache)
#
#     # Make sure we get an error if we try to push it again.
#     @test_throws AssertionError push!(cache, A, 1; cb = callback)
#
#     # If we now add "B", there should be no evictions
#     push!(cache, B, 1; cb = callback)
#     @test CachedArrays.currentsize(cache) == 2
#     @test CachedArrays.freespace(cache) == maxsize - 2
#     @test in(A, cache)
#     @test in(B, cache)
#     @test !in(C, cache)
#
#     # Now, when we add C, it should evict A
#     push!(cache, C, 1; cb = callback)
#     @test CachedArrays.currentsize(cache) == 2
#     @test CachedArrays.freespace(cache) == maxsize - 2
#     @test evictions == [A]
#     empty!(evictions)
#
#     @test !in(A, cache)
#     @test in(B, cache)
#     @test in(C, cache)
#
#     # If we update B and then add C, we'd expect C to be evicted
#     CachedArrays.update!(cache, B, 1)
#     push!(cache, A, 1; cb = callback)
#     @test CachedArrays.currentsize(cache) == 2
#     @test CachedArrays.freespace(cache) == maxsize - 2
#     @test evictions == [C]
#     empty!(evictions)
#
#     @test in(A, cache)
#     @test in(B, cache)
#     @test !in(C, cache)
#
#     # If we give "B" a size of 2 and add it, then A and C should be evicted
#     push!(cache, C, 2; cb = callback)
#     @test evictions == [B,A]
#     empty!(evictions)
#
#     # Empty the cache - make sure "C" pops out.
#     empty!(cache; cb = callback)
#     @test evictions == [C]
#     empty!(evictions)
#
#     # Make sure deleting works properly
#     push!(cache, A, 1; cb = callback)
#     push!(cache, B, 1; cb = callback)
#     delete!(cache, A, 1)
#     @test isempty(evictions)
#     @test CachedArrays.currentsize(cache) == 1
#
#     # Pushing Again should not yield any errors.
#     push!(cache, A, 1; cb = callback)
#     @test CachedArrays.currentsize(cache) == 2
# end
