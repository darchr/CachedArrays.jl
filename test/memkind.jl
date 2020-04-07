@testset "Testing LibMemKind" begin
    # Try to get a `Kind` object for allocating pmem.
    # For now, just allocate to a local file, we'll verify the kind.
    dir = @__DIR__
    MemKind = CachedArrays.MemKind

    # Create a temporary local file.
    #
    # To start with, pass no maximum size.
    kind = MemKind.create_pmem(dir)

    # Allocate 8 bytes.
    ptr = convert(Ptr{Int}, MemKind.malloc(kind, sizeof(Int)))

    i = 1000
    unsafe_store!(ptr, i)
    @test unsafe_load(ptr) == i

    # Store and load
    @test pointer(MemKind.detect_kind(ptr)) == pointer(kind)
    MemKind.free(kind, ptr)

    # Now, create another pmem kind.
    # We perform some sanity checks on this object.
    maxsize = 2^26
    kind = MemKind.create_pmem(dir, maxsize)

    # Allocate and free a pointer a bunch of times, make sure that the heap management
    # is actually working correctly.
    @time for _ in 1:1000
        ptr = MemKind.malloc(kind, sizeof(Int))
        @test ptr != C_NULL
        MemKind.free(kind, ptr)
    end
end
