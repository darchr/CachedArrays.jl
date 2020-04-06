# CachedArrays

Julia arrays that may be transparently backed by persistent memory NVDIMMs.

The API and functionality of these things is still being worked on - check back later :)

**Notes**

To manage a heap on NVDIMMs, the library `memkind` <https://github.com/memkind/memkind> is used.
This can potentially be used to generalize this idea to other kinds of backing stores.
I'm trying to leave enough flexibility in the code that this can be supported inthe future if desired.
