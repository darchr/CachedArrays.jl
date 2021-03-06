#####
##### Main Setup
#####

is2lm = parse(Bool, ENV["JULIA_IS_2LM"])

# In 2LM, we don't need to worry about tinkering with the eviction policies
if is2lm
    flushpercents = [1.0]
    policies = ["CachedArrays.LRU{CachedArrays.Block}()"]
else
    flushpercents = [0.8]
    #flushpercents = [1.0]
    policies = [
        #"CachedArrays.LRU{CachedArrays.Block}()",
        "CachedArrays.RandomPolicy{CachedArrays.Block}()",
    ]
end

function __run(cmd)
    jl = joinpath(homedir(), "projects", "juliamkl", "julia", "julia")
    runcmd = `numactl --physcpubind=24-47 --membind=1 $jl --color=yes --project -e $cmd`
    run(runcmd)
    return nothing
end

#####
##### 1D tests
#####

totalsizes = [150_000_000_000, 240_000_000_000, 400_000_000_000]
arraysizes = [1_000_000_000]

iter = Iterators.product(
    totalsizes,
    arraysizes,
    flushpercents,
    policies,
)

# Run loop
# for (totalsize, arraysize, fp, policy) in iter
#     # 1D tests
#     cmd = """
#         using MicroBench, CachedArrays;
#         MicroBench.tests_1d($totalsize, $arraysize, $fp, $policy)
#     """
#     __run(cmd)
#
#     # Alloc and Dealloc
#     cmd = """
#         using MicroBench, CachedArrays;
#         MicroBench.alloc_tests($totalsize, $arraysize, $fp, $policy)
#     """
#     __run(cmd)
# end

#####
##### 2D Tests
#####

totalsizes = [150_000_000_000, 240_000_000_000, 400_000_000_000]
arraysizes = [100_000_000, 1_000_000_000]

iter = Iterators.product(
    totalsizes,
    arraysizes,
    flushpercents,
    policies,
)

for (totalsize, arraysize, fp, policy) in iter
    cmd = """
        using MicroBench, CachedArrays;
        MicroBench.tests_2d($totalsize, $arraysize, $fp, $policy)
    """
    __run(cmd)
end

