# Allocation of our arrays.
function alloc_1d(manager, totalsize, arraysize)
    num = div(totalsize, arraysize)

    #####
    ##### Allocate the large arrays
    #####

    numelements = div(arraysize, sizeof(Float32))
    arrays = [LockedCachedArray{Float32}(undef, manager, numelements) for _ in 1:num]

    # Zero initialize the arrays.
    # Important to make sure the OS actually gives us the physical pages.
    for A in arrays
        _A = CachedArrays.unlock(A)
        Threads.@threads for i in eachindex(_A)
            _A[i] = zero(eltype(_A))
        end
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
    s = CachedArrays.unlock(similar(first(arrays)))
    s .= zero(eltype(s))

    # Now, accumulate across all the arrays.
    for a in arrays
        Threads.@threads for i in eachindex(a)
            s[i] = s[i] + a[i]
        end
    end
    return s
end

# This dirties the entire cache
function sequential_zero(arrays::Vector; iterations = 1)
    for A in arrays
        CachedArrays.prefetch!(A)
        _A = CachedArrays.unlock(A)
        Threads.@threads for i in eachindex(_A)
            _A[i] = zero(eltype(A))
        end
    end
    return nothing
end

