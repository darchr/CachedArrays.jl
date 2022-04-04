# Simple policy that always allocates to the local pool.
struct LocalTracker
    threshold::Float64
end
LocalTracker() = LocalTracker(0.8)

# Ignore all hints.
Base.push!(::LocalTracker, block, x...) = nothing
Base.delete!(::LocalTracker, block) = nothing
update(::LocalTracker, _) = nothing
softevict!(::LocalTracker, manager, block) = nothing
prefetch!(::Block, ::LocalTracker, manager; kw...) = nothing
evict!(::Any, ::LocalTracker, manager) = nothing

# Only allocate from local pool.
function policy_new_alloc(t::LocalTracker, manager, bytes, id, priority::AllocationPriority)
    if priority == ForceRemote
        @return_if_exists unsafe_alloc_direct(RemotePool(), manager, bytes, id)
    end

    # See if it's time to GC.
    allocated, total = getstate(getheap(manager, LocalPool()))
    if allocated / total > t.threshold
        println("Manually invoking the garbage collector!")
        @show manager
        GC.gc(true)
    end
    @return_if_exists unsafe_alloc_direct(LocalPool(), manager, bytes, id)
    return nothing
end

function defrag!(manager, policy::LocalTracker)
    cb = () -> unsafe_cleanup!(manager)
    defrag!(getheap(manager, LocalPool()); queued_callback = cb) do _, newblock, oldblock
        primary = getprimary(manager, oldblock)
        @assert primary === oldblock
        _ = unsafe_setprimary!(manager, oldblock, newblock; unsafe = true)
        return nothing
    end
    return nothing
end
