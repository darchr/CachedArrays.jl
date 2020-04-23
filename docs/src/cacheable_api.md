# Cacheable API

For an object of type `T` to be `Cacheable`, it needs the following:

Define `cacheable(::T) = Cacheable()` and define `pointer(x::T)`.
This will trigger dispatch plumbing to construct a `Block` from an instance of `T`.
NOTE: In order for this to be valid, the space for `T` must be allocated from the `CacheManager`, and thus have a metadata Block associated with it.
Otherwise, things are going to break hilariously badly.
This metadata `Block` is a 64-bytes block located just before the `pointer` that contains a bunch of metadata about the data, including state of dirtiness, unique ID, sibling's etc.
Furthermore, `T` must imeplement `manager(x::T) -> CacheManager`, which returns the `CacheManager` that is currently managing `x`.

Other required components are `arraytype(x::T)`, which returns the equivalent dense type `Array{T,N}` for `x`, and `replace!(x::T, A::arraytype(x))` which **MUST** swap the backing store for
`x` with `A`.

Finally, whenever an instance `x` of `T` is created, it must be registered by calling `register!(x)`.

By defining the above, the following methods become available.
```
isdirty
setdirty!
id
pool
prefetch!
evict!
```
