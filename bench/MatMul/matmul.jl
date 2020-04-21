module MatMul

using Random
using CachedArrays

collection(f, num, sz) = [f(randn(Float32, sz, sz)) for _ in 1:num]

go(num, sz; kw...) = go(identity, num, sz; kw...)

function go(f, num, sz; iters = 1000)
    Random.seed!(123)
    matrices = collection(f, num, sz)

    # Show the total size of all the matrices.
    @show sum(sizeof, matrices)

    @time for _ in 1:iters
        x = rand(matrices) * rand(matrices)
        i = rand(1:length(matrices))
        matrices[i] = x
    end
    return matrices
end

function go_inplace(f, num, sz; iters = 1000)
    Random.seed!(123)
    matrices = collection(f, num, sz)

    # Show the total size of all the matrices.
    @show sum(sizeof, matrices)
    @time for _ in 1:iters
        i = rand(1:length(matrices))
        matrices[i] .= rand(matrices) * rand(matrices)
    end
    return matrices
end

#####
##### Record tests here because of the ever increasing number of steps required.
#####

function largish_test()
    GC.gc()
    manager = CachedArrays.GlobalManager[]
    resize!(manager, 20_000_000_000)
    CachedArrays.resize_remote!(manager, 20_000_000_000)
    manager.flushpercent = 0.8

    return (
        () -> go_inplace(identity, 10, 10000; iters = 30),
        () -> go_inplace(LockedCachedArray, 10, 10000; iters = 30),
    )
end

end

