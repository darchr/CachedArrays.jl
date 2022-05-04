mutable struct ArrayPairWithLifetime
    turns_left::Int
    cached_array::CachedArray
    reference::Array
end

function epoch(
    manager::CachedArrays.CacheManager,
    arrays::Set{ArrayPairWithLifetime},
    generator::F;
    allocate = true,
    prefetch_percent = 0.20,
    evict_percent = 0.02,
    defrag_percent = 0.01,
) where {F}
    if allocate
        size, lifetime = generator()
        nelements = ceil(Int, size / sizeof(Float32))
        reference = randn(Float32, nelements)
        cached_array = CachedArrays.CachedArray{Float32}(undef, manager, nelements)
        CachedArrays.writable(cached_array) .= reference
        push!(arrays, ArrayPairWithLifetime(lifetime, cached_array, reference))
    end

    # Go through each element in the "arrays" set.
    # Decrement lifetime by 1.
    # If lifetime is at zero, check that the cached array is still the same as the reference
    # array and pop it from the `arrays` set - making it available for garbage collection.
    #
    # If it's not time to free the array, possibly call "evict" of "prefetch".
    arrays_to_delete = []
    for pair in arrays
        turns_left = pair.turns_left
        if iszero(turns_left)
            @test CachedArrays.readable(pair.cached_array) == pair.reference
            push!(arrays_to_delete, pair)
        else
            pair.turns_left -= 1

            # Try to prefetch or evict.
            if rand() < prefetch_percent
                CachedArrays.prefetch!(pair.cached_array)
            end

            if rand() < evict_percent
                CachedArrays.evict!(pair.cached_array)
            end
        end
    end

    # Maybe Defrag.
    if rand() < defrag_percent
        array_evicted = false
        for pair in arrays
            if rand() < evict_percent
                CachedArrays.evict!(pair.cached_array)
                array_evicted = true
            end
        end
        if array_evicted
            CachedArrays.defrag!(manager)
        end
    end

    for array in arrays_to_delete
        delete!(arrays, array)
    end
end

@testset "Performing Stress Test 1" begin
    target_allocations = 100_000
    target_mean_lifetime = 200
    target_mean_allocation = 8192 # bytes
    allocation_range = 8191 # bytes

    manager = CachedArrays.CacheManager(
        CachedArrays.AlignedAllocator(),
        CachedArrays.AlignedAllocator();
        localsize = 100 * target_mean_allocation,
        # Make the remote size pretty large so we don't need to worry about running out
        # of memory.
        remotesize = ceil(Int, target_allocations * target_mean_allocation),
        minallocation = 12,
    )

    # Create the generator function.
    Random.seed!(12345)
    exp_distribution = Exponential(1 / target_mean_lifetime)
    function generator()
        _size = target_mean_allocation + rand(-allocation_range:allocation_range)
        _lifetime = clamp(rand(exp_distribution), 2, 1000)
        return _size, _lifetime
    end

    @show CachedArrays.gettimer()
    CachedArrays.reset_timer!()
    arrays = Set{ArrayPairWithLifetime}()
    println("Beginning Stress Test")
    @time ProgressMeter.@showprogress 1 for _i in 1:target_allocations
        epoch(manager, arrays, generator; allocate = true)
        CachedArrays.check(manager) || error()
    end

    println("Cleaning Up Stress Test")
    @time while !isempty(arrays)
        epoch(manager, arrays, generator; allocate = false)
        CachedArrays.check(manager) || error()
    end

    @show manager
    @show CachedArrays.gettimer()
    (; manager_time, movement_time) = manager
    @show manager_time, movement_time
    id = CachedArrays.readid(manager)
    @show id, (manager_time - movement_time) / id
end
