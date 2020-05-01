# Allocation of our arrays.
function _alloc(manager, totalsize, arraysize)
    num = div(totalsize, arraysize)

    #####
    ##### Allocate the large arrays
    #####

    numelements = div(arraysize, sizeof(Float32))
    return [LockedCachedArray{Float32}(undef, manager, numelements) for _ in 1:num]
end

function alloc_1d(manager, totalsize, arraysize)
    arrays = _alloc(manager, totalsize, arraysize)

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

function sequential_bump(arrays::Vector)
    for a in arrays
        CachedArrays.prefetch!(a)
        _a = CachedArrays.unlock(a)
        Threads.@threads for i in eachindex(_a)
            _a[i] += one(eltype(_a))
        end
    end
    return nothing
end

# This dirties the entire cache
function sequential_zero(arrays::Vector; iterations = 1)
    for A in arrays
        CachedArrays.shallow_fetch!(A)
        _A = CachedArrays.unlock(A)
        Threads.@threads for i in eachindex(_A)
            _A[i] = zero(eltype(A))
        end
    end
    return nothing
end

__set(A::LockedCachedArray, v) = __set(CachedArrays.unlock(A), v)
function __set(A::CachedArray, v)
    Threads.@threads for i in eachindex(A)
        @inbounds A[i] = v
    end
end

function __add!(A::AbstractArray{T}, B::AbstractArray{T}, C::AbstractArray{T}) where {T}
    @assert length(A) == length(B) == length(C)
    Threads.@threads for i in eachindex(A)
        @inbounds A[i] = B[i] + C[i]
    end
end

function alloc_and_dealloc(manager, totalsize, arraysize)
    numarrays = div(totalsize, arraysize)
    numelements = div(arraysize, sizeof(Float32))

    # Populate going forward.
    p = ProgressMeter.Progress(numarrays, 1, "Filling Arrays")
    count = zero(Float32)
    arrays = map(1:numarrays) do i
        A = LockedCachedArray{Float32}(undef, manager, numelements)

        # Set a function boundary here because otherwise, inference fails and makes the
        # threaded loop SUPER slow
        __set(A, count)

        ProgressMeter.next!(p)
        count += 1
        return A
    end

    # Now go the other direction.
    s = CachedArray{Float32}(undef, manager, numelements)
    p = ProgressMeter.Progress(numarrays, 1, "Popping Arrays")
    while !isempty(arrays)
        # Should touch previously freed arrays, resulting in a completely dirty cache,
        # but we can elide the writebacks if we set the manager to run GC before allocating.

        # Prefetch in case the random eviction policy decides to screw us.
        CachedArrays.prefetch!(s)

        # Try with both prefetching and without prefetching ... with all threads reading,
        # prefetching will probably be faster.
        a = pop!(arrays)
        u = CachedArray{Float32}(undef, manager, numelements)
        __add!(u, a, s)

        ProgressMeter.next!(p)

        # Rebind "s"
        s = u
    end
    return sum(s)
end

