module CachedArrays

#####
##### Exports
#####

export CachedArray, ReadableCachedArray, UnreadableCachedArray, WritableCachedArray, UnwritableCachedArray
export @annotate, tocached
export onobjects, @blockobjects, findobjects, @noescape

export prefetch!, evict!, softevict!

# state transitions
export readable, writable, release

#####
##### Deps
#####

# base
import Base: @lock

# stdlib
import Dates
import LinearAlgebra
import Mmap
import Random

# Dependencies
# Import "constructorof" to allow external modules to extend "constructorof" without
# directly invoking "ConstructablesBase'.
# Of course, that's what's ultimately happening, but this is just cleaner.
import ArrayInterface
import ChainRulesCore
import ConstructionBase: ConstructionBase, constructorof
import DataStructures
import JSON
import Polyester
import SIMD
import MacroTools
import TimerOutputs

using DocStringExtensions

const DEBUG = true              # Enable asserts
const VERBOSE = false           # Diagnostic printing
const ENABLETIMING = false       # Debug timing
const MANAGERTIMING = true
# const ALLOW_UNSAFE_FREE = true  # Enable the `unsafe_free` function.
include("options.jl")

#####
##### includes
#####

@enum AllocationPriority ForceLocal PreferLocal ForceRemote
abstract type AbstractCachedArray{T,N} <: DenseArray{T,N} end

# Bootstrap Utilities
include("utils/utils.jl")
include("utils/findnexttree.jl")
include("utils/freebuffer.jl")
include("utils/lru.jl")
include("object.jl")
include("allocators.jl")

# Heap implementations
include("memory/memory.jl")

# Implementation of the arrays and cache manager
include("manager.jl")
include("validation.jl")
include("api.jl")

# Cache eviction policies
include("policy.jl")
include("policies/local.jl")

# Array Implementstions
include("llvm.jl")
using .LoadStore: LoadStore

include("array.jl")
include("telemetry.jl")
include("memcpy.jl")
include("lib.jl")

#####
##### Keep track of Managers
#####

# When Managers get created - hold them in a global array to keep them from getting
# GC'd before the arrays they track.
#
# We can periodically GC the mangers to see if they are no longer holding onto anything,
# at which point we're free to reclaim their resources.
const GlobalManagers = CacheManager[]
function gc_managers()
    # Find all the managers that can be garbage collected.
    #
    # Trigger a full garbage collection before to clean up anything that may be holding
    # onto a manager
    GC.gc(true)

    # Now, find all the managers that have been completely cleaned up.
    i = findall(cangc, GlobalManagers)
    i === nothing && return 0

    # Remove the managers from this list and then run a full garbage collection to make sure
    # they're well and completely gone.
    deleteat!(GlobalManagers, i)
    GC.gc(true)

    # Return the number of managers we removed
    return length(i)
end

end # module
