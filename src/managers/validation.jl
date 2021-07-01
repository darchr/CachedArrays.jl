#####
##### Validation
#####

# Check invariants between the CacheManager and the managed heaps.
function check(manager::CacheManager)
    passed = true
    if !check(manager.remote_heap)
        println("Remote Heap failed!")
        passed = false
    end

    if !check(manager.local_heap)
        println("Local Heap failed!")
        passed = false
    end

    # Now - check if the manager's recorded stats align with the each of the heaps.
    # Remote Heap
    seen_ids = Set{UInt64}()
    size_allocated = 0
    for block in manager.remote_heap
        if !isfree(block)
            push!(seen_ids, CachedArrays.getid(block))
            size_allocated += length(block)
        end
    end
    if getsize(manager.remotemap) != size_allocated
        println(
            """
            Manager and heap Remote objects size mismatch.
            Manager sees: $(getsize(manager.remotemap)). Heap sees: $size_allocated.
            """
        )
        println(
            "    Difference: $(Int.(sort(collect(setdiff(seen_ids, keys(manager.remotemap))))))",
        )
        passed = false
    end

    issubset(seen_ids, keys(manager.remotemap)) || (passed = false)
    issubset(keys(manager.remotemap), seen_ids) || (passed = false)

    # Local Heap
    seen_ids = Set{UInt64}()
    size_allocated = 0
    for block in manager.local_heap
        if !isfree(block)
            push!(seen_ids, CachedArrays.getid(block))
            size_allocated += length(block)
        end
    end
    if getsize(manager.localmap) != size_allocated
        println(
            "Manager and heap Local objects size mismatch. Manager sees: $(getsize(manager.localmap)). Heap sees: $size_allocated.",
        )
        println("    Manager IDS: $(Int.(sort(collect(keys(manager.local_objects)))))")
        println("    Heap IDS: $(Int.(sort(collect(seen_ids))))")
        passed = false
    end

    if !issubset(seen_ids, keys(manager.localmap))
        println(
            "Manager sees $(length(manager.localmap)) in Local. Heap sees $(length(seen_ids))",
        )
        passed = false
    end
    if !issubset(keys(manager.localmap), seen_ids)
        println(
            "Manager sees $(length(manager.localmap)) in Local. Heap sees $(length(seen_ids))",
        )
        passed = false
    end

    return passed
end
