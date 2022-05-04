# Must be mutable to swap the "add" and "remove" buffers.
mutable struct FreeBuffer{T}
    add::Vector{T}
    remove::Vector{T}
    add_lock::Base.Threads.SpinLock
end

FreeBuffer{T}() where {T} = FreeBuffer{T}(T[], T[], Threads.SpinLock())

add_lock(buf::FreeBuffer) = buf.add_lock
@inline candrain(buf::FreeBuffer) = !isempty(buf.add)

function Base.push!(buf::FreeBuffer, x)
    @spinlock add_lock(buf) begin
        @check !isqueued(x)
        # Indicate that this object has been queued for freeing.
        # Helps with eviction logic.
        markqueued!(x)
        push!(buf.add, x)
    end
end

# Assumes that the "remove" lock is already held.
function unsafe_swap!(buf::FreeBuffer)
    @spinlock add_lock(buf) begin
        (buf.add, buf.remove) = (buf.remove, buf.add)
    end
end

# Assumes that the "remove" lock is already held.
function unsafe_get(buf::FreeBuffer)
    return buf.remove
end
