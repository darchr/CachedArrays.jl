# Sweep key environmental variables.
#
# First - make everything fit in the cache

totalsizes = (150_000_000_000, 240_000_000_000, 400_000_000_000)

# 1D tests
cmd = "using MicroBench; MicroBench.tests_1d()"
jl = joinpath(homedir(), "projects", "juliamkl", "julia", "julia")
runcmd = `numactl --physcpubind=24-47 --membind=1 $jl --color=yes --project -e $cmd`
# for totalsize in totalsizes
#     for arraysize in (1_000_000_000,)
#         ENV["JULIA_MICROBENCH_ARRAYSIZE"] = arraysize
#         ENV["JULIA_MICROBENCH_TOTALSIZE"] = totalsize
#         run(runcmd)
#     end
# end

# do the 2D tests
cmd = "using MicroBench; MicroBench.tests_2d()"
runcmd = `numactl --physcpubind=24-47 --membind=1 $jl --color=yes --project -e $cmd`
for totalsize in (150_000_000_000,  240_000_000_000, 400_000_000_000)
    for arraysize in (100_000_000, 500_000_000, 1_000_000_000)
        ENV["JULIA_MICROBENCH_ARRAYSIZE"] = arraysize
        ENV["JULIA_MICROBENCH_TOTALSIZE"] = totalsize
        run(runcmd)
    end
end

