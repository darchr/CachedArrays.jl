function memset!(_ptr::Ptr, len, _v)
    ptr = Ptr{UInt8}(_ptr)
    v = UInt8(_v)
    for i in Base.OneTo(len)
        unsafe_store!(ptr, v, i)
    end
end

function memcmp(_ptrA::Ptr, _ptrB::Ptr, len)
    ptrA = Ptr{UInt8}(_ptrA)
    ptrB = Ptr{UInt8}(_ptrB)
    for i in Base.OneTo(len)
        (unsafe_load(ptrA, i) != unsafe_load(ptrB, i)) && return false
    end
    return true
end

@testset "Testing Operation of Cache Manager" begin
    Local = CachedArrays.Local
    Remote = CachedArrays.Remote
    PoolType = CachedArrays.PoolType
    getid = CachedArrays.getid

    GC.gc(true)
    minallocation = 12
    manager = CachedArrays.CacheManager(
        CachedArrays.AlignedAllocator(),
        CachedArrays.AlignedAllocator();
        localsize = 2^20,
        remotesize = 2^22,
        minallocation = minallocation,
    )

    # Notes:
    # 2^12 == 4096
    # 2^20 == 1_048_576
    # 2^22 == 4_194_304

    @test length(manager.map) == 0
    @test !haskey(manager.map, 1)
    A = CachedArrays.direct_alloc(PoolType{Local}(), manager, 500_000, getid(manager))
    @test isa(A, CachedArrays.Object)
    @test CachedArrays.pool(A) == Local
    @test getid(CachedArrays.metadata(A)) == 1
    @test haskey(manager.map, 1)

    # Allocation of B - set the data region of B
    B = CachedArrays.direct_alloc(PoolType{Local}(), manager, 500_000, getid(manager))
    @test isa(B, CachedArrays.Object)
    @test CachedArrays.pool(B) == Local
    @test getid(CachedArrays.metadata(B)) == 2
    @test haskey(manager.map, 2)

    # Length of the underlying block should be larger than or equal the requested size.
    len = length(CachedArrays.metadata(B))
    @test len >= 500_000
    @test iszero(mod(sizeof(CachedArrays.metadata(B)), 2^minallocation))
    memset!(pointer(B), length(CachedArrays.metadata(B)), 0xff)
    all_0xff = fill(0xff, len)
    @test memcmp(pointer(B), pointer(all_0xff), len)

    # Local pool should be full, further allocations should fail.
    id = getid(manager)
    @test_throws CachedArrays.AllocationException CachedArrays.direct_alloc(
        PoolType{Local}(),
        manager,
        500_000,
        id,
    )
    @test CachedArrays.check(manager)

    lock(CachedArrays.alloc_lock(manager)) do
        ptr = CachedArrays.unsafe_alloc_direct(PoolType{Local}(), manager, 500_000, id)
        @test ptr === nothing
    end
    @test CachedArrays.check(manager)

    lock(CachedArrays.alloc_lock(manager)) do
        # Okay, now we allocate a remote block for B, link as siblings, and set the
        # primary to the remote.
        block_local = CachedArrays.metadata(B)
        ptr_remote = CachedArrays.unsafe_alloc_direct(
            PoolType{Remote}(),
            manager,
            length(block_local),
            getid(block_local),
        )
        block_remote = CachedArrays.unsafe_block(ptr_remote)
        @test getid(block_remote) == getid(block_local)

        # Test that `copyto!` works correctly.
        @test false == memcmp(
            ptr_remote,
            CachedArrays.datapointer(block_local),
            length(block_local),
        )
        CachedArrays.copyto!(block_remote, block_local, manager)
        @test true == memcmp(
            ptr_remote,
            CachedArrays.datapointer(block_local),
            length(block_local),
        )

        CachedArrays.link!(block_remote, block_local)
        @test CachedArrays.getsibling(block_remote) === block_local
        @test CachedArrays.getsibling(block_local) === block_remote
        old_ptr = CachedArrays.unsafe_setprimary!(manager, block_local, block_remote)
        @test CachedArrays.unsafe_block(old_ptr) === block_local

        # Load the updated block from the backedge to ensure that we did in fact
        # update the backedge.
        map = CachedArrays.getmap(manager)
        new_block = CachedArrays.unsafe_block(unsafe_load(last(map[getid(block_remote)])))
        @test block_remote === new_block
    end
    @test CachedArrays.check(manager)

    # unsafe_free and marking.
    lock(CachedArrays.alloc_lock(manager)) do
        block = CachedArrays.metadata(A)
        CachedArrays.unsafe_free(A)
        @test CachedArrays.isnull(CachedArrays.unsafe_pointer(A))
        @test CachedArrays.candrain(manager)
        @test CachedArrays.isqueued(block)

        # See that `setprimary!` fails.
        ptr_remote = CachedArrays.unsafe_alloc_direct(
            PoolType{Remote}(),
            manager,
            length(block),
            getid(block),
        )
        block_remote = CachedArrays.unsafe_block(ptr_remote)
        CachedArrays.link!(block, block_remote)
        @test CachedArrays.unsafe_setprimary!(manager, block, block_remote) === nothing

        # Now, if we drain the manager, both the local and remote blocks should be
        # cleaned because we linked them.
        cleaned = CachedArrays.unsafe_cleanup!(manager, CachedArrays.getid(block))
        @test cleaned == true
        @test CachedArrays.isfree(block)
        @test CachedArrays.isfree(block_remote)
    end
    @test CachedArrays.check(manager)
end

@testset "Testing Manager Cleanup 1" begin
    GC.gc(true)
    @test length(CachedArrays.GlobalManagers) == 1
    manager = only(CachedArrays.GlobalManagers)
    @test CachedArrays.cangc(manager)
    @test CachedArrays.check(manager)
end

@testset "Testing Manager Cleanup 2" begin
    v = CachedArrays.gc_managers()
    @test v == 1
end
