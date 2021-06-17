# Interface for object metadata.
abstract type AbstractMetadata end
metastyle(x) = nothing

metadata(x) = metadata(x, metastyle(x))
metadata(x, ::Nothing) = error("Cannot get metadata for objects of type $(typeof(x))")

# Metadata API
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
