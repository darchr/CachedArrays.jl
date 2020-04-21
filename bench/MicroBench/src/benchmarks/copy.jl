#####
##### Sequential Mutation
#####

# Create `num` arrays of size `sz` bytes.
# Then, iterate through pairs in the array, copying each array to its neighbor.
#
# argument `f` - Mutator to apply to each array. Provides a hook for turning each of them
# into a `LockedCachedArray`.
function sequential_mutation(arrays::Vector; iterations = 1)
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

# For now, only run this once at startup.
function sequential_mutation_setup()
    if DEBUG
        localsize = 10_000_000_000
        remotesize = 30_000_000_000
    # if in 2LM mode, all of the temporary arrays should go into local memory
    elseif IS_2LM
        localsize = 500_000_000_000
        remotesize = 0
    # Size local mamory to be a little smaller than the DRAM cache to allow for other
    # program activities that don't belong to our heap.
    else
        localsize = 180_000_000_000
        remotesize = 1_000_000_000_000
    end

    # Resize the CacheManager
    manager = CachedArrays.GlobalManager[]
    manager.gc_before_evict = false

    # Allocate 180 GB for local memory, using 2GiB for the maximum allocation.
    resize!(manager, localsize; maxallocation = 2^31)

    # Allocate 1T for remote memory ... mostly just because we can haha.
    CachedArrays.resize_remote!(manager, remotesize; maxallocation = 2^31)
end

function sequential_mutation_alloc()
    sz = 1_000_000_000
    num = DEBUG ? 20 : div(ARRAYSIZE, sz)

    #####
    ##### Allocate the large arrays
    #####
    numelements = div(sz, sizeof(Float32))
    arrays = [LockedCachedArray{Float32}(undef, numelements) for _ in 1:num]

    # Zero initialize the arrays.
    # Important to make sure the OS actually gives us the physical pages.
    Threads.@threads for A in arrays
        KernelBenchmarks.sequential_write(A, Val{16}())
    end
    return arrays
end

