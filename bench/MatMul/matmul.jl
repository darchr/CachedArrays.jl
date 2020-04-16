module MatMul

using Random
using CachedArrays

collection(f, num, sz) = [f(rand(Float32, sz, sz)) for _ in 1:num]

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

end

