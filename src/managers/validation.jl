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

    #####
    ##### Blocks allocated by heaps
    #####

    remote_ids = Set{UInt64}()
    remote_size_allocated = 0
    for block in manager.remote_heap
        if !isfree(block)
            push!(remote_ids, CachedArrays.getid(block))
            remote_size_allocated += length(block)
        end
    end

    local_ids = Set{UInt64}()
    local_size_allocated = 0
    for block in manager.local_heap
        if !isfree(block)
            push!(local_ids, CachedArrays.getid(block))
            local_size_allocated += length(block)
        end
    end

    #####
    ##### Blocks visible from the manager
    #####

    manager_local_ids = Set{UInt64}()
    manager_remote_ids = Set{UInt64}()

    for (id, backedge) in manager.map.dict
        block = unsafe_block(unsafe_load(backedge))
        id = getid(block)
        pool = getpool(block)

        set = pool == Local ? manager_local_ids : manager_remote_ids
        other = pool == Local ? manager_remote_ids : manager_local_ids
        push!(set, id)
        sibling = getsibling(block)
        if sibling !== nothing
            # Sanity checks.
            if getsibling(sibling) !== block
                println("Non-symmetrical linking between the following siblings:")
                println(block)
                println(sibling)
                passed = false
            end

            if getid(sibling) != id
                println("Non-symmetrical id between the following siblings:")
                println(block)
                println(sibling)
                passed = false
            end

            if getpool(sibling) == pool
                println("Non-symmetrical pool between the following siblings:")
                println(block)
                println(sibling)
                passed = false
            end

            if length(sibling) != length(block)
                println("Non-symmetrical length between the following siblings:")
                println(block)
                println(sibling)
                passed = false
            end
            push!(other, id)
        end
    end

    if !issubset(manager_local_ids, local_ids)
        println("Heap local IDs not a subset of Manager local ids!")
        passed = false
    end

    if !issubset(local_ids, manager_local_ids)
        println("Manager local ids not a subset of heap ids!")
        passed = false
    end

    if !issubset(manager_remote_ids, remote_ids)
        println("Heap remote IDs not a subset of Manager remote ids!")
        passed = false
    end

    if !issubset(remote_ids, manager_remote_ids)
        println("Manager remote ids not a subset of heap ids!")
        @show setdiff(remote_ids, manager_remote_ids)
        @show setdiff(manager_remote_ids, remote_ids)
        passed = false
    end

    return passed
end
