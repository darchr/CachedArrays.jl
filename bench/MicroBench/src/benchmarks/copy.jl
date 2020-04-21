#####
##### Sequential Mutation
#####

# Create `num` arrays of size `sz` bytes.
# Then, iterate through pairs in the array, copying each array to its neighbor.
#
# argument `f` - Mutator to apply to each array. Provides a hook for turning each of them
# into a `LockedCachedArray`.
function sequential_mutation(sz, num, f = identity; iterations = 2)
    numelements = div(sz, sizeof(Float32))
    if !iszero(mod(num, 2))
        throw(ArgumentError("Argument `num` must be a multiple of 2!"))
    end

    # Allocate a TON of memory.
    #
    # Initializing this with `ones` should ensure that all of the memory is initialized
    # by the OS.
    arrays = [f(ones(Float32, numelements)) for _ in 1:num]

    for _ in 1:iterations
        @showprogress 1 for i in 1:2:length(arrays)
            CachedArrays.memcpy!(arrays[i+1], arrays[i])
        end
    end
    return nothing
end

function sequential_mutation_runner(test = false)
    localsize = test ? 10_000_000_000 : 180_000_000_000
    remotesize = test ? 100_000_000_000 : 1_000_000_000_000
    num = test ? 20 : 400

    # Resize the CacheManager
    manager = CachedArrays.GlobalManager[]

    # Allocate 180 GB for local memory, using 2GiB for the maximum allocation.
    resize!(manager, localsize; maxallocation = 2^31)

    # Allocate 1T for remote memory ... mostly just because we can haha.
    CachedArrays.resize_remote!(manager, remotesize; maxallocation = 2^31)

    sz = 1_000_000_000
    sequential_mutation(sz, num, LockedCachedArray)
    return nothing
end

