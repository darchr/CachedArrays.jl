struct FirstTouch{AllowsCleanup}
    threshold::Float64
end
FirstTouch{AllowsCleanup}() where {AllowsCleanup} = FirstTouch{AllowsCleanup}(0.8)

# Ignore hints
Base.push!(::FirstTouch, args...) = nothing
Base.delete!(::FirstTouch, args...) = nothing
update(::FirstTouch, _) = nothing
softevict!(::FirstTouch, args...) = nothing
prefetch!(::Block, ::FirstTouch, args...; kw...) = nothing
evict!(::Block, ::FirstTouch, args...) = nothing

function policy_new_alloc(
    policy::FirstTouch, manager, bytes, id, priority::AllocationPriority
)
    allocated, total = getstate(getheap(manager, RemotePool()))
    if allocated / total > policy.threshold
        GC.gc(true)
    end

    # Check if it's even worth trying to allocate locally
    if priority != ForceRemote
        allocated, total = getstate(getheap(manager, LocalPool()))
        if total - allocated > bytes
            @return_if_exists unsafe_alloc_direct(LocalPool(), manager, bytes, id)
        end
    end
    # Fallback to allocating remotely.
    @return_if_exists unsafe_alloc_direct(RemotePool(), manager, bytes, id)
    return nothing
end

function defrag!(manager, ::FirstTouch)
    cb = () -> unsafe_cleanup!(manager)
    defrag!(getheap(manager, LocalPool()); queued_callback = cb) do _, newblock, oldblock
        primary = getprimary(manager, oldblock)
        @assert primary === oldblock
        _ = unsafe_setprimary!(manager, oldblock, newblock; unsafe = true)
        return nothing
    end
    return nothing
end

#####
##### API Disabling Exentions
#####

function noescape(
    ::CacheManager{FirstTouch{false}}, ::Val, f::F, args::Vararg{Any,N}; kw...
) where {F,N}
    return f(args...; kw...)
end

function unsafe_free(::Object{<:CacheManager{FirstTouch{false}}}, ::CleanupContext)
    return nothing
end
