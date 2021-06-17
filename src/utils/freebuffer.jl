# Must be mutable to swap the "add" and "remove" buffers.
mutable struct FreeBuffer{T}
    add::Vector{T}
    remove::Vector{T}
    add_lock::Base.Threads.SpinLock
    remove_lock::Base.Threads.SpinLock
end

FreeBuffer{T}() where {T} = FreeBuffer{T}(T[], T[], Threads.SpinLock(), Threads.SpinLock())

add_lock(buf::FreeBuffer) = buf.add_lock
remove_lock(buf::FreeBuffer) = buf.remove_lock

candrain(buf::FreeBuffer) = !isempty(buf.add)

function Base.push!(buf::FreeBuffer, x)
    @spinlock add_lock(buf) begin
        # Indicate that this object has been queued for freeing.
        # Helps with eviction logic.
        markqueued!(x)
        push!(buf.add, x)
    end
end

# Special Case Pointers
function Base.push!(buf::FreeBuffer{Ptr{Nothing}}, x::Ptr{Nothing})
    @spinlock add_lock(buf) push!(buf.add, x)
end

# Assumes that the "remove" lock is already held.
function unsafe_swap!(buf::FreeBuffer)
    @requires remove_lock(buf)
    @spinlock add_lock(buf) begin
        (buf.add, buf.remove) = (buf.remove, buf.add)
    end
end

# Assumes that the "remove" lock is already held.
function unsafe_get(buf::FreeBuffer)
    @requires remove_lock(buf)
    return buf.remove
end
