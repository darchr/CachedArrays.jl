# NOTE: Much of this is based initially off the Binned pool in CuArrays.jl
#
# Keep memory pools in Bins based on size.
# Include history heuristics to know when to return memory to the allocator.

# Tunable Paramters
#
# Objects below the minimum size will all be served by the minimum-sized pool
# Objects above the maximum size will have to be dynamically allocated.
const BINNED_MIN_POOL_SIZE = 2^10 # 1 KiB
const BINNED_MAX_POOL_SIZE = 2^32 # 4 GiB

# Usage history to track
const HISTORY_WINDOW = 3
const INITIAL_USAGE = ntuple(_ -> Float32(1), HISTORY_WINDOW)

# Don't trust constant propagation
const LOG2_BINNED_MIN_POOL_SIZE = ceil(Int, log2(BINNED_MIN_POOL_SIZE))


# Functions mapping a requested size to the index of a pool it can be serviced from.
function binindex(n)
    # Clamp this up to the smallest pool size.
    n = max(n, BINNED_MIN_POOL_SIZE) - BINNED_MIN_POOL_SIZE
    # Do some log2 schenanigans
    return ceil(Int, log2(n)) - LOG2_BINNED_MIN_POOL_SIZE + 1
end

# This is the inverse of the above function.
binsize(idx) = BINNED_MIN_POOL_SIZE * 2^(idx - 1)

struct BinnedPool{T <: AbstractAllocator} <: AbstractPool{T}
    allocator::T
    allocated::Dict{Ptr{Nothing},Block}
    # Keep track of blocks that are currently live and being serviced from this pool.
    pools_active::Vector{Set{Block}}
    # Free pools that can be used to service future requests.
    pools_available::Vector{Vector{Block}}

    # Tracking data structues to monitor the current and past utilization of the bins.
    current_utilization::Vector{Float32}
    past_utilization::Vector{NTuple{HISTORY_WINDOW,Float32}}

    # Some heuristics for how often to run reclamation.
    last_reclamation::Ref{Dates.DateTime}
    reclamation_timeout::Ref{Dates.Millisecond}
end

function BinnedAllocator(allocator::T, timeout = Dates.Millisecond(5000)) where {T}
    allocated = Dict{Ptr{Nothing},Block}()

    # Compute the number of bins.
    num_bins = binindex(BINNED_MAX_POOL_SIZE) - binindex(BINNED_MIN_POOL_SIZE) + 1

    pools_active = [Set{Block}() for _ in 1:num_bins]
    pools_available = [Block[] for _ in 1:num_bins]
    current_utilization = [Float32(1) for _ in 1:num_bins]
    past_utilization = [INITIAL_USAGE for _ in 1:num_bins]

    last_reclamation = Ref(now())
    reclamation_timeout = Ref(timeoute)

    # Construct and return.
    return BinnedPool{T}(
        allocator,
        allocated,
        pools_active,
        pools_available,
        current_utilization,
        past_utilization,
        last_reclamation,
        reclamation_timeout,
    )
end

@inline numbins(pool::BinnedPool) = length(pool.pools_active)

# Scan through each bin in the pool.
# Update the current utilization of each bin as well as the historical usage.
function update_history!(pool::BinnedPool)
    # Trigger incremental GC
    GC.gc(false)
    active = false
    @inbounds for bin in 1:numbins(pool)
        num_active = length(pool.pools_active[bin])
        num_available = length(pool.pools_available[bin])

        # Only update history if there's something in this bin.
        if num_active + num_available > 0
            utilization = pool.current_utilization[bin]
            current_utilization = num_active / (num_active + num_available)

            # Shift the history window with this update.
            pool.past_utilization[bin] = (Base.tail(pool.past_utilization[bin])..., utilization)

            # Update the tracked utilizaiton
            pool.current_utilization = current_utilization
            if current_utilization != utilization
                active = true
            end
        else
            pool.current_utilization[bin] = 1
            pool.past_utilization[bin] = INITIAL_USAGE
        end
    end
    return active
end

# Return unused Blocks to the allocator.
#
# If `full == true` return all available pools.
# If `full == false`, only return pools as dictated by the previous usage.
function reclaim!(pool::BinnedPool, target_bytes::Int = typemax(Int); full::Bool = true)
    # Scanning Step
    num_inactive_blocks = Vector{Int}(undef, numbins(pool))
    if full
        for (bin, num_available) in enumerate(pool.pools_available)
            num_inactive_blocks[bin] = num_available
        end
    else
        @inbounds for bin in 1:numbins(pool)
            num_active = length(pool.pools_active[bin])
            num_available = length(pool.pools_available[bin])

            if num_available > 0
                max_utilization = max(pool.current_utilization[bin], maximum(pool.past_utilization[bin]))
                reclaimable = floor(Int, (1 - max_utilization) * (num_active + num_available))
                num_inactive_blocks[bin] = reclaimable
            else
                num_inactive_blocks[bin] = 0
            end
        end
    end

    # Reclamation step.
    freed_bytes = 0
    for bin in numbins(pool):-1:1
        bytes = poolsize(pid)
        available = pool.pools_available[bin]
        reclaimable = num_inactive_blocks[bin]
        for i in 1:reclaimable
            block = pop!(available)
            __free(pool, block)
            freed_bytes += bytes

            if freed_bytes >= target_bytes
                return freed_bytes
            end
        end
    end
    return freed_bytes
end

#####
##### Allocation Steps
#####

function _alloc(pool::BinnedPool, bytes, bin = nothing)
    block = nothing
    # Fast path, if a bin has been identified
    if !isnothing(bin) && !isempty(pool.pools_available[bin])
        block = pop!(pool.pools_available[bin])
    end

    # Otherwise, try an actual allocation.
    if isnothing(block)
        block = __alloc(pool, bytes)

        # Now is a good time to check if we need to run a collection.
        # Run a full garbage collection ...
        update_history!(pool)
        GC.gc(true)
        reclaim!(pool)
    end

    # Update some usage statistics
    if !isnothing(block) && !isnothing(bin)
        @inbounds active = pool.pools_active[bin]
        @inbounds available = pool.pools_available[bin]

        # Mark the block as used.
        push!(active, block)

        # Update pool usage.
        current_usage = length(active) / (length(active) + length(available))
        pool.current_utilization[bin] = max(pool.current_utilization[bin], current_usage)
    end
    return block
end

function alloc(pool::BinnedPool, bytes)
    block = if bytes <= BINNED_MIN_POOL_SIZE
        bin = binindex(bytes)
        _alloc(pool, binsize(bin), bin)
    else
        _alloc(pool, bytes)
    end

    ptr = pointer(block)
    pool.allocated[ptr] = block
    return ptr
end

function free(pool::BinnedPool, ptr::Ptr{Nothing})
    block = pool.allocated[ptr]
    delete!(pool.allocated, ptr)

    bytes = sizeof(block)
    if bytes > BINNED_MAX_POOL_SIZE
        __free(pool, pointer(block))
    else
        bin = binindex(sizeof(block))
        push!(pool.pools_available[bin], block)
    end
    return nothing
end
