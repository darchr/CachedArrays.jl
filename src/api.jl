#####
##### Metadata
#####

abstract type AbstractMetadata end
struct NoMeta <: AbstractMetadata end

metastyle(x) = NoMeta()
metadata(x) = metadata(x, metastyle(x))
metadata(x, ::NoMeta) = error("Cannot get metadata for objects of type $(typeof(x))")

# Metadata API
# TODO: rename to "local" setdirty?
setdirty!(A::AbstractMetadata, ::Bool) = error("Implement `setdirty!")
isdirty(A::AbstractMetadata) = error("Implement `isdirty`!")

getid(A::AbstractMetadata) = error("Implement `getid`!")
getpool(A::AbstractMetadata) = error("Implement `getpool`!")

"""
    getsibling(A::T) where T <: AbstractMeta

Return either `nothing` or an instance ot `T` the sibling of `A`.
"""
getsibling(A::AbstractMetadata) = error("Implement `getsibling`!")
setsibling!(A::T, B::T) where {T <: AbstractMetadata} = error("Implement `setsibling!`")

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

function prefetch!(A; kw...)
    _manager = manager(A)
    @spinlock alloc_lock(_manager) remove_lock(_manager.freebuffer) begin
        actuate!(
            LocalPool(),
            A,
            _manager;
            copydata = true,
            updatebackedge = true,
            freeblock = false,
            kw...,
        )
    end
end

function shallowfetch!(A; kw...)
    _manager = manager(A)
    @spinlock alloc_lock(_manager) remove_lock(_manager.freebuffer) begin
        actuate!(
            LocalPool(),
            A,
            _manager;
            copydata = false,
            updatebackedge = true,
            freeblock = false,
            kw...,
        )
    end
end

function evict!(A; kw...)
    _manager = manager(A)
    @spinlock alloc_lock(_manager) remove_lock(_manager.freebuffer) begin
        actuate!(
            RemotePool(),
            A,
            _manager;
            copydata = true,
            updatebackedge = true,
            freeblock = true,
            kw...,
        )
    end
end

manager(x) = error("Implement `manager` for $(typeof(x))")

