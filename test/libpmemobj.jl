@testset "Testing Libpmemobj" begin
    # Test out the functionality of libpmemobj.
    # Since we can't guarentee that a test-machine will have NVDIMMs, we use a normal
    # system file for this testing.
    #
    # This should work since libpmem/libpmemobj are designed to do this for testing, and
    # all that we're testing here is the interface between Julia and the library.

    # Make this definition for convenience.
    PmemObj = CachedArrays.PmemObj

    # Create a file for testing purposes
    testdir = joinpath(@__DIR__, "temp")
    !ispath(testdir) && mkdir(testdir)

    poolfile = joinpath(testdir, "pool")
end
