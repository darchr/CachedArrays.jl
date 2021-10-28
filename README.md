# CachedArrays

Julia arrays that can live and be moved between different memory pools, including but not
limited to DRAM and Persistent Memory (in the form of NVDIMMs).

## General Usage

All `CachedArrays` are backed by a memory allocator called the `CacheManager`.
The manager holds onto the memory backing the array and also provides utilities for moving the array between different memory pools.
A `CacheManager` is composed of three things:

1. A `local` memory pool (conceptually, think of this as a smaller "fast" memory).
2. A `remote` memory pool (a larger "slow" memory).
3. A `policy`, or program logic that controls how and if allocated `CachedArrays` migrate between these pools.
