# CachedArrays

Julia arrays that may be transparently backed by persistent memory NVDIMMs.

The API and functionality of these things is still being worked on - check back later :)

**Notes**

To manage a heap on NVDIMMs, the library `memkind` <https://github.com/memkind/memkind> is used.
This can potentially be used to generalize this idea to other kinds of backing stores.
I'm trying to leave enough flexibility in the code that this can be supported inthe future if desired.

### Benchmarking Copy

Big Idea: Our fast `memcpy` from DRAM to PMM and back can be multithreaded to get as much bandwidth as possible.
However, for arrays that are small enough, the overhead of threading might not be worth it.

The goal here is to find the cross over point in terms of array size for when it is faster to multithread the copy versus using a single thread.
My intuition says that this should be different when going from DRAM to PMM versus going from PMM to DRAM.

#### Getting the Source Code

Clone this repo into directory of your chosing with
```sh
git clone https://github.com/darchr/CachedArrays.jl CachedArrays
```

#### Environment Setup

To ensure we are on the same page regarding setup.
Make sure you do this in every new terminal session you are working with (I still forget to do this from time to time)
Export the following environment variables before starting Julia:
```sh
export JULIA_NUM_THREADS=24
export JULIA_PMEM_PATH="/mnt/public"
```
Finally, launch Julia under `numactl` with
```sh
numactl --physcpubind=24-47 --membind=1 <path/to/julia> [--project]
```

#### Creating a reproducible script

Make a new directory in the `/bench/` directory for your test script.
You can make a Julia `Project.toml` and `Manifest.toml` file by starting Julia in this new directory and running
```julia
]activate .
```
You can look at the `MatMul` project as an example of how to structure your code.
The idea with `MatMul` is to navigate to the `MatMul` repo, start Julia with `--project` and go
```julia
# Note, I'm using `includet` because I'm using Revise.jl - which I can't recommend highly enough.
includet("matmul.jl")

# Note the "." in front of MatMul
using CachedArrays, .MatMul

# Run stuff in MatMul
MatMul.go(10, 10)
```
Putting script code inside its own module is not necessary, just something I do from habbit.

#### Outline of Code

The meat and potatoes of this experiment is measuring bandwidth from PMM to DRAM and back.
Using CachedArrays.jl, you can get a normal Array in DRAM using
```julia
# Create an array with 1000000 Float32's
dram = zeros(Float32, 1000000)

# The size of the array in bytes can be queried using `sizeof`
sizeof(dram)
```
You can create an Array in PMM using
```julia
# Create an array with 1000000 Float32's in PMM
pmm = CachedArrays.remote_alloc(Array{Float32,1}, CachedArrays.GlobalManager[], (1000000,)

# Ensure pmm is initialized
pmm .= zero(eltype(pmm))
```
The resulting object will be indistinguishable from a normal array, but will live in PMM (assuming CachedArrays.jl didn't give you a warning on startup ...)

You can then time how long it takes to copy data from PMM to DRAM using
```julia
using BenchmarkTools

# Using all 24 threads
@benchmark CachedArrays.memcpy!(dram, pmm)

# Using only 1 thread
@benchmark CachedArrays.memcpy!(dram, pmm; forcesingle = true)
```
When moving from DRAM to PMM, be sure to pass the `toremote` argument if not using `forcesingle`:
```julia
@benchmark CachedArrays.memcpy!(pmm, dram, true)
```

Repeat this process for different sizes of `dram` and `pmm` and time when `forcesingle = true` becomes slower than the default multithreaded case.

#### Packages to checkout

- BenchmarkTools.jl <https://github.com/JuliaCI/BenchmarkTools.jl>
    This is a helpful package for making sure you aren't measuring compiler overhead and
    helps when running small functions that take a short amount of time.

- Revise.jl <https://github.com/timholy/Revise.jl>
    Automatically updates the RELP when code changes. Super useful!



