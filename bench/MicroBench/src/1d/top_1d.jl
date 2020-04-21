# Allocation of our arrays.
function alloc_1d()
    num = div(TOTALSIZE, ARRAYSIZE)

    #####
    ##### Allocate the large arrays
    #####

    numelements = div(ARRAYSIZE, sizeof(Float32))
    arrays = [LockedCachedArray{Float32}(undef, numelements) for _ in 1:num]

    # Zero initialize the arrays.
    # Important to make sure the OS actually gives us the physical pages.
    Threads.@threads for A in arrays
        KernelBenchmarks.sequential_write(A, Val{16}())
    end
    return arrays
end

# Create `num` arrays of size `sz` bytes.
# Then, iterate through pairs in the array, copying each array to its neighbor.
function sequential_copy(arrays::Vector; iterations = 1)
    if !iszero(mod(length(arrays), 2))
        throw(ArgumentError("Argument `num` must be a multiple of 2!"))
    end

    for _ in 1:iterations
        @showprogress 1 for i in 1:2:length(arrays)
            CachedArrays.memcpy!(arrays[i+1], arrays[i])
        end
    end
    return nothing
end

# To element-wise accumulation.
function sequential_accumulation(arrays::Vector; iterations = 1)
    # Create a similar array to the input.
    s = similar(first(arrays))
    s .= zero(eltype(s))

    # Now, accumulate across all the arrays.
    for a in arrays
        s .+= a
    end
    return s
end

# This dirties the entire cache
function sequential_zero(arrays::Vector, iterations = 1)
    for a in arrays
        a .= zero(eltype(a))
    end
    return nothing
end

