# CachedArrays

Julia arrays that may be transparently backed by persistent memory NVDIMMs.

The API and functionality of these things is still being worked on - check back later :)

**Notes**

To manage a heap on NVDIMMs, the library `memkind` <https://github.com/memkind/memkind> is used.
This can potentially be used to generalize this idea to other kinds of backing stores.
I'm trying to leave enough flexibility in the code that this can be supported inthe future if desired.

## TODO List

### Fast Memcpy

Road map towards fast memcpy. `src/mempcy.jl`.

- [X] Generate low-level unrolled loops. This is done via `CachedArrays._mov`.
    See <https://docs.julialang.org/en/v1/manual/metaprogramming/#Generated-functions-1>
    for details on `@generated` functions and `https://github.com/eschnett/SIMD.jl` for
    SIMD intrinsics.

- [ ] Reference <https://github.com/pmem/pmdk/tree/master/src/libpmem2/x86_64/memcpy>, particularly
    <https://github.com/pmem/pmdk/blob/master/src/libpmem2/x86_64/memcpy/memcpy_nt_avx512f.c> for
    how to tie together multiple unrollings.

    Steps that they take:
    * Work until the number of bytes to move is a multiple of 64
    * Do the 32 unrolled loop as many times as possible.
    * Use smaller unrolling to finish up stragglers.
    * Break down forward copy verses backward copy depending on relative locations of input pointers.

- [ ] Need an `sfence` at the end to make sure nothing breaks.
    Talk to me when you get here.

- [ ] Function Signature
    ```julia
        fast_memcpy(dst::AbstractArray, src::AbstractArray)
    ```
    Make sure to check at the beginning that arrays are 64B aligned.
    ```julia
    if !iszero(mod(convert(Int, pointer(x)), 64))
        throw(ArgumentError("Arrays should be 64 Byte aligned you dummy!"))
    end
    ```

- [ ] Write Some tests. `test/memcpy.jl`


### Allocations
Track down a couple of extra allocations.
Each time an array is instantiatiated, a couple extra allocations occur.
These extra allocations cause extra GC pressure.
See if there is a way to cut down on this.
