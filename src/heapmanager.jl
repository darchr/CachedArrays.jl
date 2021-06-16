# Wrapper around a single heap.
# Provides functionality similar to a CacheManager but only manages a single heap,
# so is conceptually much simpler.
mutable struct HeapRegion{T}
    # TODO: Store the allocation function in the block header to avoid taking space in
    # the region struct?
    ptr::Ptr{Nothing}
    manager::T

    function HeapRegion(ptr::Ptr{Nothing}, manager::T) where {T}
        region = new{T}(ptr, manager)
        finalizer(cleanup, region)
        return region
    end
end

Base.pointer(region::HeapRegion) = region.ptr
cleanup(region::HeapRegion) = cleanup(manager(region), pointer(region))
manager(region::HeapRegion) = region.manager

function alloc(region::HeapRegion, bytes::Integer)
    ptr = @spinlock alloc_lock(manager(region)) unsafe_alloc(manager(region), bytes)
    return HeapRegion(ptr, manager(region))
end

#####
##### HeapManager
#####

mutable struct HeapManager
    heap::CompactHeap{AlignedAllocator}
    alloc_lock::Base.Threads.SpinLock
    freebuffer::FreeBuffer{Ptr{Nothing}}
    bytes_allocated::Int
    gc_when_over::Int
end

alloc_lock(manager::HeapManager) = manager.alloc_lock

function HeapManager(maxsize::Integer; minallocation = 10, gc_when_over = 0.95)
    heap = CompactHeap(
        AlignedAllocator(),
        convert(Int, maxsize);
        minallocation = minallocation,
    )

    manager = HeapManager(
        heap,
        Threads.SpinLock(),
        FreeBuffer{Ptr{Nothing}}(),
        0,
        floor(Int, sizeof(heap) * gc_when_over),
    )
    push!(GlobalHeaps, manager)
    return manager
end

# Cleanup
cleanup(manager::HeapManager, ptr::Ptr) = push!(manager.freebuffer, ptr)
function cleanup(manager::HeapManager)
    @spinlock remove_lock(manager.freebuffer) begin
        prepare_cleanup!(manager)
        unsafe_cleanup!(manager)
    end
end

prepare_cleanup!(manager::HeapManager) = unsafe_swap!(manager.freebuffer)

function unsafe_cleanup!(M::HeapManager)
    @requires alloc_lock(M) remove_lock(M.freebuffer)
    heap = M.heap

    while true
        cleanlist = unsafe_get(M.freebuffer)

        # Free all pointers in the cleanlist
        for ptr in cleanlist
            M.bytes_allocated -= length(unsafe_block(ptr))
            free(heap, ptr)
        end
        empty!(cleanlist)

        # Process other buffer as well.
        # Safe to do since we already hold the "remove_lock".
        # The next trip around the loop will be with the new buffer.
        if candrain(M.freebuffer)
            unsafe_swap!(M.freebuffer)
        else
            break
        end
    end
    return nothing
end

#####
##### Allocation
#####

function alloc(manager::HeapManager, bytes::Integer)
    ptr = @spinlock alloc_lock(manager) unsafe_alloc(manager, convert(Int, bytes))
    return HeapRegion(ptr, manager)
end

function unsafe_alloc(manager::HeapManager, bytes::Int)
    heap = manager.heap
    candrain(manager.freebuffer) && cleanup(manager)

    # See if we should trigger an incremental GC
    already_gc = false
    if manager.bytes_allocated >= manager.gc_when_over
        GC.gc(false)
        already_gc = true
    end

    ptr = alloc(heap, bytes)
    # Try incremental GC if we're out of room.
    if ptr === nothing
        already_gc || GC.gc(false)
        candrain(manger.free_buffer) && cleanup(manager)
        ptr = alloc(heap, bytes)
    end

    manager.bytes_allocated += length(unsafe_block(ptr))
    return ptr
end
