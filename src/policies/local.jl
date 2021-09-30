# S
#imple policy that always allocates to the local pool.
struct LocalTracker end

# Ignore all hints.
Base.push!(::LocalTracker, block, x...) = nothing
Base.delete!(::LocalTracker, block) = nothing
update(::LocalTracker, _) = nothing
softevict!(::LocalTracker, manager, block) = nothing
prefetch!(::Block, ::LocalTracker, manager; kw...) = nothing
evict!(A, ::LocalTracker, manager) = nothing

# Only allocate from local pool.
function policy_new_alloc(::LocalTracker, manager, bytes, id, _::AllocationPriority)
    # See if it's time to GC.
    allocated, total = getstate(getheap(manager, LocalPool()))
    if allocated / total > 0.8
        GC.gc(true)
    end
    @return_if_exists unsafe_alloc_direct(LocalPool(), manager, bytes, id)
    return nothing
end
