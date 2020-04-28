function alloc_2d(manager, totalsize, arraysize)
    # Decide how bit each array should be, based on the total size.
    numelements = div(arraysize, sizeof(Float32))
    dims = isqrt(numelements)
    numarrays = div(totalsize, sizeof(Float32) * dims^2)
    arrays = [LockedCachedArray{Float32}(undef, manager, (dims, dims)) for _ in 1:numarrays]

    for A in arrays
        # Unlock the array to get proper `setindex!`
        _A = CachedArrays.unlock(A)
        Threads.@threads for i in eachindex(_A)
            _A[i] = zero(eltype(_A))
        end
    end

    return arrays
end

function matrix_mult_forward(arrays::Vector; iterations = 1)
    for _ in 1:iterations
        @showprogress 1 for i in 1:3:(length(arrays)-2)
            mul!(arrays[i+2], arrays[i], arrays[i+1])

            # Annotate elements i and i+1 as cheap.
            CachedArrays.cheapevict(arrays[i])
            CachedArrays.cheapevict(arrays[i+1])
        end
    end
end

function matrix_mult_forward_and_back(arrays::Vector; iterations = 1)
    for _ in 1:iterations
        @showprogress 1 for i in 1:3:(length(arrays)-2)
            mul!(arrays[i+2], arrays[i], arrays[i+1])

            # Annotate elements i and i+1 as cheap.
            CachedArrays.evict!(arrays[i])
            CachedArrays.evict!(arrays[i+1])
        end
        @showprogress 1 for i in (length(arrays)-2):-3:1
            mul!(arrays[i], arrays[i-2], arrays[i-1])

            # Annotate elements i and i+1 as cheap.
            CachedArrays.cheapevict(arrays[i-1])
            CachedArrays.cheapevict(arrays[i-2])
        end
    end
end
