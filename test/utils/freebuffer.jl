mutable struct MarkedQueued
    queued::Bool
end
MarkedQueued() = MarkedQueued(false)
CachedArrays.markqueued!(x::MarkedQueued) = (x.queued = true)
CachedArrays.isqueued(x::MarkedQueued) = (x.queued)

@testset "Testing FreeBuffer" begin
    buffer = CachedArrays.FreeBuffer{MarkedQueued}()
    @test CachedArrays.candrain(buffer) == false

    a = MarkedQueued()
    @test CachedArrays.isqueued(a) == false
    push!(buffer, a)
    @test CachedArrays.isqueued(a) == true
    @test CachedArrays.candrain(buffer) == true
    CachedArrays.unsafe_swap!(buffer)

    buffer = CachedArrays.unsafe_get(buffer)
    @test buffer == [a]
end
