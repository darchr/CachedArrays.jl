# Dispatch Token
struct Cacheable end

#####
##### Metadata
#####

abstract type AbstractMetadata end
struct NoMeta <: AbstractMetadata end

metastyle(_) = NoMeta()
metadata(x) = metadata(x, metastyle(x))
metadata(x, ::NoMeta) = error("Cannot get metadata for objects of type $(typeof(x))")

# Metadata API
# TODO: rename to "local" setdirty?
setdirty!(::AbstractMetadata, ::Bool) = error("Implement `setdirty!")
isdirty(::AbstractMetadata) = error("Implement `isdirty`!")

getid(::AbstractMetadata) = error("Implement `getid`!")
getpool(::AbstractMetadata) = error("Implement `getpool`!")

"""
    getsibling(A::T) where T <: AbstractMeta

Return either `nothing` or an instance ot `T` the sibling of `A`.
"""
getsibling(::AbstractMetadata) = error("Implement `getsibling`!")
setsibling!(::T, ::T) where {T <: AbstractMetadata} = error("Implement `setsibling!`")

#####
##### Default Definitions
#####

function setdirty!(x, flag = true)
    meta = metadata(x)
    # Mark dirty with manager - then mark the metadata.
    setdirty!(manager(x), meta, flag)
    setdirty!(meta, flag)
    return nothing
end

update!(x) = update!(manager(x), metadata(x))

#####
##### Cacheable API
#####

isdirty(A) = isdirty(metadata(A))
id(A) = getid(metadata(A))
pool(A) = getpool(metadata(A))

function prefetch!(::Cacheable, A; kw...)
    _manager = manager(A)
    @spinlock alloc_lock(_manager) begin
        block = metadata(A)
        # Possibility that `maybe_cleanup!` cleans up this block.
        # Presumeably, this can't happen since we have `A` and `A` is holding onto the
        # object that owns this block ... so it should never have been garbage collected
        # in the first place.
        if maybe_cleanup!(_manager, getid(block)) == false
            prefetch!(block, _manager.policy, _manager; readonly = isreadonly(A))
        end
    end
end

function evict!(::Cacheable, A; kw...)
    _manager = manager(A)
    @spinlock alloc_lock(_manager) begin
        maybe_cleanup!(_manager)
        evict!(A, _manager.policy, _manager)
    end
    return nothing
end

function softevict!(::Cacheable, A)
    _manager = manager(A)
    @spinlock alloc_lock(_manager) softevict!(_manager.policy, _manager, metadata(A))
end

# TODO: This is such a hack ...
unsafe_free(::Cacheable, A) = unsafe_free(A.object)
manager(x) = error("Implement `manager` for $(typeof(x))")

