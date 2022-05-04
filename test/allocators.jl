@testset "Testing Allocators" begin
    verify_alignment(p) = @test iszero(mod(convert(UInt, p), 64))

    #####
    ##### Aligned Allocator
    #####

    allocator = CachedArrays.AlignedAllocator()

    for _ in 1:10000
        ptr, token = CachedArrays.allocate(allocator, rand(1:1_000_000))
        verify_alignment(ptr)
        CachedArrays.free(ptr, token)
    end

    #####
    ##### MmapAllocator
    #####

    # Normal Mmap Allocator
    allocator = CachedArrays.MmapAllocator(@__DIR__)
    ptr, token = CachedArrays.allocate(allocator, 1_000_000)
    @test isa(ptr, Ptr{Nothing})
    function rw(ptr, len; write = true)
        if write
            for i in 1:len
                unsafe_store!(ptr, UInt8(i % 256), i)
            end
        end

        passed = true
        for i in 1:len
            val = unsafe_load(ptr, i)
            val == UInt8(i % 256) || (passed = false)
        end
        @test passed
    end

    rw(Ptr{UInt8}(ptr), 1_000_000)
    CachedArrays.free(ptr, token)

    # Persistent Mmap Allocator
    path = joinpath(@__DIR__, "temp.bin")
    ispath(path) && rm(path)
    allocator = CachedArrays.PersistentMmapAllocator(path)
    ptr, token = CachedArrays.allocate(allocator, 1_000_000)
    @test isa(ptr, Ptr{Nothing})
    rw(Ptr{UInt8}(ptr), 1_000_000)
    CachedArrays.free(ptr, token)

    # Make sure it's persistent by reallocating
    ptr, token = CachedArrays.allocate(allocator, 1_000_000)
    rw(Ptr{UInt8}(ptr), 1_000_000; write = false)
    CachedArrays.free(ptr, token)
end
