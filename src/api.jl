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

