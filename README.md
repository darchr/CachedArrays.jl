# CachedArrays

Julia arrays that can live and be moved between different memory pools, including but not limited to DRAM and Persistent Memory (in the form of Optane NVDIMMs).
This readme will discuss some of the high-level usage and functionality of this package.
However, it does not go too in depth.
**WARNING**: This repo is experimental!!

## How it Works

CachedArrays works by pre-allocating application memory into two memory heaps, a "local" heap (typically referring to faster memory) and a "remote" heap (typically slower memory).
The `CacheManager` is responsible for allocating new arrays (i.e., `CachedArrays`) from the memory heaps and migrating existing arrays between heaps.
Decisions about when and how to migrate arrays is delegated to the `policy`, which can be customized on a per-application basis.
A `CachedArray` works just like a standard Julia array and is inter-operable with normal Julia arrays.

### Example

```julia
# Construct a CacheManager.
julia> manager = CachedArrays.CacheManager(
   CachedArrays.AlignedAllocator(),         # Allocator for Local Memory
   CachedArrays.MmapAllocator(pwd());       # Allocator for Remote Memory
   localsize = 2_000_000_000,               # Size of Local Memory Pool in Bytes
   remotesize = 1_000_000_000,              # Size of Remote Memory Pool in Bytes
)
Cache Manager
    0 Objects
    0.0 GB Memory Used.
    Local Heap utilization: 0.0 of 2.0 (0.0 %)
    Remote Heap utilization: 0.0 of 0.999999488 (0.0 %)
    Manager Time (s): 0.0
    Movement Time (s): 0.0

# Construct an CachedArray
julia> x = CachedArray{Float32}(undef,
1000×1000 CachedArray{Float32, 2, ...}:
...

# Randomly Initialize the array
julia> foreach(i -> x[i] = randn(Float32), eachindex(x));

# Normal Julia operations work
julia> y = x .+ 1;

# BLAS operations work
julia> z = x * y;

# More importantly, there should not be any performance penalty versus standard arrays.
julia> a = randn(Float32, 1000, 1000); b = randn(Float32, 1000, 1000);

julia> using BenchmarkTools

julia> @btime $x * $y;
  1.957 ms (1 allocation: 32 bytes)

julia> @btime $a * $b;
  1.956 ms (2 allocations: 3.81 MiB)

julia> @btime $x .+ $y
  508.057 μs (1 allocation: 32 bytes)

julia> @btime $a .+ $b
  500.365 μs (2 allocations: 3.81 MiB)
```
Moving arrays between memory pools is done via the functions `prefetch!` and `evict!`:
```julia
julia> x = CachedArray{Float32}(undef, manager, 1000, 1000);

# We can use the function `metadata` to query state about the allocated object.
# In this case, we can see that the array `x` is in the local memory pool.
julia> CachedArrays.metadata(x)
Block
    Address: 0x7ff14625c040
    Size: 4000768
    Backsize: 0
    Availability: Taken
    Pool: Local
    ID: 9674
    Dirty: true
    Evicting: false
    Orphaned: false
    Sibling: 0x0

# If we want the array in remote memory, we can use `evict!`:
julia> CachedArrays.evict!(x);

julia> CachedArrays.metadata(x);
Block
    Address: 0x7ff1be4cb800
    Size: 4000768
    Backsize: 4000768
    Availability: Taken
    Pool: Remote
    ID: 9674
    Dirty: true
    Evicting: false
    Orphaned: false
    Sibling: 0x0
```
Depending on the policy used, existing arrays can be preemptively evicted from local memory to remote memory.
To lazily prioritize arrays for eviction, the function `softevict!`.

## Support for Wrapper Types

If you want your own types that contain `CachedArrays` to support operations like `prefetch!` and `evict!`, you can use the `CachedArrays.@wrapper` macro.
```julia
julia> using CachedArrays

julia> julia> manager = CachedArrays.CacheManager(
   CachedArrays.AlignedAllocator(),
   CachedArrays.MmapAllocator(pwd());
   localsize = 2_000_000_000,
   remotesize = 1_000_000_000,
);

julia> struct MyWrapper{A,B}
   a::A
   b::B
end

# Mark the fields `a` and `b` as potentially containing CachedArrays
julia> CachedArrays.@wrapper MyWrapper a b

julia> x = CachedArray{Float32}(undef, manager, 1000, 1000);

julia> y = rand(1:10);

julia> wrapper = MyWrapper(y, x);

julia> CachedArrays.getpool(CachedArrays.metadata(wrapper.b))
Local::Pool = 0x0000000000000000

julia> CachedArrays.evict!(wrapper);

# The sub CachedArray is evicted to remote memory.
julia> CachedArrays.getpool(CachedArrays.metadata(wrapper.b))
Remote::Pool = 0x0000000000000001
```

## Escape Analysis

Julia's GC uses different heuristics for allocated `CachedArrays` than it does for the built in arrays.
This is because `CachedArrays` only involve small allocations for the handle to the data and thus Julia's GC isn't aware of exactly how much space these arrays are occupying.
Therefore, the GC doesn't clean up temporary `CachedArrays` as often as one would like.

To help with this, you can use the `@noescape` macro:
```julia
julia> using CachedArrays

julia> manager = CachedArrays.CacheManager(
   CachedArrays.AlignedAllocator(),
   CachedArrays.MmapAllocator(pwd());
   localsize = 2_000_000_000,
   remotesize = 1_000_000_000,
);

julia> function repeatedly_allocates(x; num_times = 200)
    for i in 1:num_times
        x = copy(x)
    end
    return x
end

julia> x = CachedArray{Float32}(undef, manager, 1000);

# Displaying the manager shows that we only have one array allocated (as expected)
julia> display(manager);
Cache Manager
    1 Objects
    4.032e-6 GB Memory Used.
    Local Heap utilization: 4.096e-6 of 2.0 (2.048e-6 %)
    Remote Heap utilization: 0.0 of 0.999999488 (0.0 %)
    Manager Time (s): 9.857e-6
    Movement Time (s): 0.0

# Makes 200 copies of `x` that are strictly local to the function body.
julia> repeatedly_allocates(x);

# We now see 201 object allocated, one for each trip of the loop.
julia> display(manager);
Cache Manager
    201 Objects
    0.000810432 GB Memory Used.
    Local Heap utilization: 0.000823296 of 2.0 (0.000411648 %)
    Remote Heap utilization: 0.0 of 0.999999488 (0.0 %)
    Manager Time (s): 0.000304103
    Movement Time (s): 0.0

# Invoke the garbage collector to clean up the previously allocated arrays.
# After running the GC, all we have is the originally allocated object.
julia> GC.gc(true); CachedArrays.gc_managers(); display(manager);
Cache Manager
    1 Objects
    4.032e-6 GB Memory Used.
    Local Heap utilization: 4.096e-6 of 2.0 (2.048e-6 %)
    Remote Heap utilization: 0.0 of 0.999999488 (0.0 %)
    Manager Time (s): 0.000304103
    Movement Time (s): 0.0

# Run the looping function again, but this time use the `noescape` macro.
# This will analyze the arguments and return result of `repeatedly_allocated` and free
# any intermediate array that doesn't escape from the function call.
julia> CachedArrays.@noescape manager repeatedly_allocates(x);

# Only two objects are alive: the original `x` the the final array produced by
# `repeatedly_allocates`.
julia> display(manager);
```

# API List

## Low-Level Allocator API

Memory comes from subtypes of `CachedArrays.AbstractAllocator`.
Valid sub-types must implement the following functions:

"""
allocate(allocator, bytes::Integer) -> (Ptr{Nothing}, Token)
"""
Allocate `bytes` from `allocator`. Return type is a a tuple whose first element is a
pointer with type `Ptr{Nothing}` to the beginning of the allocated data and whose second

element is some token that can be passed to `free`.
"""
free(ptr::Ptr{Nothing}, token)
"""
Complement to `allocate`: Takes the returned pointer and token and takes any steps
necessary to free the data.
In otherwords, the following should and clean up all allocated resources:
"""
free(allocate(allocator, bytes)...)
"""
**Note**: If `token` will automatically handle resource reclamation using a finalizer, then the type `CachedArrays.NoopWrapper` can be wrapped around the token to turn `free` into a no-op.

