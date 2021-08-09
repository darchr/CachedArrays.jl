# Dispatch Token
struct Cacheable end

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

function prefetch!(::Cacheable, A; kw...)
    _manager = manager(A)
    @spinlock alloc_lock(_manager) begin
        maybe_cleanup!(_manager)
        prefetch!(metadata(A), _manager.policy, _manager; readonly = isreadonly(A))
    end
end

# function shallowfetch!(::Cacheable, A; kw...)
#     _manager = manager(A)
#     @spinlock alloc_lock(_manager) remove_lock(_manager.freebuffer) begin
#         actuate!(
#             LocalPool(),
#             A,
#             _manager;
#             copydata = false,
#             updatebackedge = true,
#             freeblock = false,
#             kw...,
#         )
#     end
# end

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

# function _unsafe_track!(manager)
#     token = @spinlock alloc_lock(manager) begin
#         _unsafe_track!(manager.policy)
#     end
#     return token
# end
#
# function _unsafe_untrack!(manager, token, return_value)
#     # Optimization - check before acquiring the lock
#     token == false && return nothing
#     @spinlock alloc_lock(manager) begin
#         _unsafe_untrack!(manager, manager.policy, token, return_value)
#     end
#     return nothing
# end

# TODO: This is such a hack ...
unsafe_free(::Cacheable, A) = unsafe_free(A.region)
manager(x) = error("Implement `manager` for $(typeof(x))")

