#####
##### Object
#####

# Not to be confused with the "objects" in the `ObjectCache` above.
mutable struct Object{T}
    ptr::Ptr{Nothing}
    manager::T

    # Inner constructor to ensure finalizers are attached>
    function Object(ptr::Ptr{Nothing}, manager::T) where {T}
        object = new{T}(ptr, manager)
        if !isnull(ptr)
            unsafe_register!(manager, object)
            finalizer(free, object)
        end
        return object
    end
end

Base.pointer(object::Object) = object.ptr
unsafe_pointer(object::Object) = object.ptr
blockpointer(object::Object) = pointer_from_objref(object)

free(object::Object) = free(manager(object), unsafe_pointer(object))
metastyle(::Object) = BlockMeta()
manager(object::Object) = object.manager
getid(o::Object) = getid(metadata(o))
prefetch!(o::Object; kw...) = prefetch!(Cacheable(), o; kw...)

"""
$(TYPEDSIGNATURES)

Allocate `bytes` from `objects`'s manager.
If `id` is not given, it will be selected automatically.
"""
function alloc(
    object::Object,
    bytes::Integer,
    priority::AllocationPriority = PreferLocal,
    id = getid(object.manager),
)
    return alloc(manager(object), bytes, priority, id)
end

