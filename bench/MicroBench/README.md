# MicroBench

Ideas: Multiple Kernels with different computational complexities.

1. Element Wise Addition (compute per bytes low)
2. Matrix Mutliplication
3. [Convolution]

Access Patterns

1. Sequential Forward and Back
2. Sequential Forward and back leaving a wake of dirty data.
3. LFSR full walk random.
4. Full sudo-random.

## Environment Variables

Resizing the local cache isn't too slick at the moment.
To control aspects of each run, I use a collection of envionment variables.
Default settings can be found in `env_1lm.sh` and `env_2lm.sh`

* `JULIA_IS_2LM`: Boolean - set to `true` if we're in 2LM mode
* `JULIA_MICROBENCH_ARRAYSIZE`: Int - The aggregate size of the main array to test.
    n general, it's nice to be able to configure this to be various multiples of the DRAM
    cache size.
* `JULIA_NUM_THREADS`: Number of threads to use
* `JULIA_PMEM_PATH`: Path to where PMMs are mounted
