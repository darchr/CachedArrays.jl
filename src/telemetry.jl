@inline telemetry_alloc(::NoTelemetry, args...) = nothing

#####
##### Telemetry
#####

@enum TelemetryAction::UInt begin
    # Allocation
    AllocFast
    AllocSlow

    # Dellocations
    DeallocFast
    DeallocSlow

    # Transitions
    ReadTransition
    WriteTransition
    ReleaseTransition
    TransitionUnknown

    # Movement
    PrimaryInLocal
    PrimaryInRemote

    # Cleanup
    UnsafeFreed
    GarbageCollected
end

struct TelemetryRecord
    accesstime::UInt
    # Index into a global backtrace buffer.
    backtrace_key::Int
    action::TelemetryAction
end

struct AllocationRecord
    id::UInt
    size::Int
    basepointer::UInt
end

const BacktraceType = Vector{Union{Ptr{Nothing},Base.InterpreterIP}}

struct Telemetry
    # Map allocated IDs to their sequence of actions.
    logs::Dict{UInt, Vector{TelemetryRecord}}
    objects::Dict{UInt, AllocationRecord}
    backtraces::Dict{BacktraceType,Int}
    lock::Base.Threads.SpinLock
end

function Telemetry()
    return Telemetry(
        Dict{UInt, Vector{TelemetryRecord}}(),
        Dict{UInt, AllocationRecord}(),
        Dict{BacktraceType,Int}(),
        Base.Threads.SpinLock(),
    )
end

#####
##### Accessors
#####

ids(telemetry::Telemetry) = sort(collect(keys(telemetry.logs)))
getlog(telemetry::Telemetry, id) = telemetry.logs[id]

function getkey(telemetry::Telemetry)
    bt = backtrace()
    backtraces = telemetry.backtraces
    return get!(backtraces, bt, length(backtraces))
end

function Base.stacktrace(telemetry::Telemetry, record::TelemetryRecord)
    key = record.backtrace_key
    if key > 0
        return stacktrace(telemetry.backtraces[key])
    end
    return nothing
end

function Base.stacktrace(telemetry::Telemetry, key::Integer)
    bt = get(telemetry.backtraces, key, nothing)
    if bt !== nothing
        return stacktrace(bt)
    end
    return nothing
end

#####
##### Hooks
#####

function telemetry_alloc(telemetry::Telemetry, bytes, id, ptr, location)
    # Get the current time.
    now = time_ns()
    action = (location == Local) ? AllocFast : AllocSlow
    # Find where we are on the stack.
    # If this is a new location, mark it as such.
    allocation_record = AllocationRecord(id, bytes, UInt(ptr))
    @spinlock telemetry.lock begin
        telemetry.objects[id] = allocation_record
        key = getkey(telemetry)
        v = get!(() -> TelemetryRecord[], telemetry.logs, id)
        push!(v, TelemetryRecord(now, key, action))
    end
    return nothing
end

function telemetry_dealloc(telemetry::Telemetry, id, location)
    now = time_ns()
    action = (location == Local) ? DeallocFast : DeallocSlow
    @spinlock telemetry.lock begin
        key = getkey(telemetry)
        v = telemetry.logs[id]
        push!(v, TelemetryRecord(now, key, action))
    end
end

function telemetry_unsafefree(telemetry::Telemetry, id)
    now = time_ns()
    @spinlock telemetry.lock begin
        key = getkey(telemetry)
        v = telemetry.logs[id]
        push!(v, TelemetryRecord(now, key, UnsafeFreed))
    end
end

function telemetry_gc(telemetry::Telemetry, id)
    now = time_ns()
    @spinlock telemetry.lock begin
        log = telemetry.logs[id]
        push!(log, TelemetryRecord(now, 0, GarbageCollected))
    end
    return nothing
end

function telemetry_change(telemetry::Telemetry, id, to)
    # Determine the state change
    now = time_ns()
    state = TransitionUnknown
    if to == :ReadOnly
        state = ReadTransition
    elseif to == :ReadWrite
        state = WriteTransition
    elseif to == :NotBusy
        state = ReleaseTransition
    end

    # Find where we are on the stack.
    # If this is a new location, mark it as such.
    @spinlock telemetry.lock begin
        key = getkey(telemetry)
        log = telemetry.logs[id]
        push!(log, TelemetryRecord(now, key, state))
    end
    return nothing
end

function telemetry_primary(telemetry::Telemetry, id, location)
    action = location == Local ? PrimaryInLocal : PrimaryInRemote
    record = TelemetryRecord(time_ns(), 0, action)
    @spinlock telemetry.lock begin
        push!(telemetry.logs[id], record)
    end
    return nothing
end

#####
##### Analysis Passes
#####

# Find the backtrace key for all allocation sites.
function allocation_ids(telemetry::Telemetry)
    # Keep track of both the keys as well as the earliest access time so we can
    # reconstruct the call graph in some kind of ordered manner.
    #
    # Though to be honest, this will probably be equivalent to just straight up sorting
    # the keys ...
    keys = Dict{Int,UInt}()
    for log in values(telemetry.logs)
        record = first(log)
        @assert in(record.action, (AllocFast, AllocSlow))
        key = record.backtrace_key
        accesstime = record.accesstime

        keys[key] = min(accesstime, get!(keys, key, accesstime))
    end
    return sort(collect(Base.keys(keys)); by = x -> keys[x])
end

function stacktraces_for(telemetry::Telemetry, keys)
    return stacktrace.(Ref(telemetry), keys)
end

function estimate_lifetime(library::Vector{TelemetryRecord})
    # If this hasn't been GC'd yet, than return 0 as a sentinel value.
    last(library).action != GarbageCollected && return 0

    # Otherwise, the lifetime will be the difference between the first and last access
    # times. We need to be a bit careful if there wasn't a state transition for some
    # reason. In this case, the length of the record should be 2.
    #
    # The best we can do is say that the object was alive from when it was allocated
    # to when it was garbage collected.
    if length(library) == 2
        return last(library).accesstime - first(library).accesstime
    else
        return library[end-1].accesstime - first(library).accesstime
    end
end

function create_timeline(actions::Dict{String,Any}, objects)
    timeline = Vector{NamedTuple{(:accesstime, :id, :size, :action),Tuple{Int,Int,Int,String}}}()
    for (id_str, logs) in actions
        sz = objects[id_str]["size"]::Int
        id = parse(Int, id_str)
        for log in logs
            nt = (
                accesstime = log["accesstime"]::Int,
                id = id,
                size = sz,
                action = log["action"]::String
            )
            push!(timeline, nt)
        end
    end
    sort!(timeline; by = x -> x.accesstime)
    return timeline
end

function heap_usage(timeline::AbstractVector{<:NamedTuple}; name = "Fast")
    alloc_str = "Alloc$name"
    dealloc_str = "Dealloc$name"

    usage = [(time = 0, utilization = 0)]
    for nt in timeline
        if (nt.action == alloc_str)
            utilization = usage[end].utilization + nt.size
            push!(usage, (time = nt.accesstime, utilization))
        elseif (nt.action == dealloc_str)
            utilization = usage[end].utilization - nt.size
            push!(usage, (time = nt.accesstime, utilization))
        end
    end
    popfirst!(usage)
    return usage
end

#####
##### Save Telemetry
#####

const GLOBAL_FILES = [
    "./boot.jl",
    "./client.jl",
    "./essentials.jl",
    "REPL/src/REPL.jl",
    "REPL[",
]

function prettify(stack::Vector{Base.StackTraces.StackFrame})
    # Remove all CachedArray files from the front of the trace.
    while true
        frame = first(stack)
        if occursin("CachedArrays/src", string(frame.file))
            popfirst!(stack)
        else
            break
        end
    end

    # Remove all REPL related stuff from the end of the trace.
    while any(x -> occursin(x, string(last(stack).file)), GLOBAL_FILES)
        pop!(stack)
    end
    return stack
end


# Custom JSON serialization for Telemetry
struct TelemetrySerialization <: JSON.CommonSerialization end
struct NoBacktraces{T}
    telemetry::T
end

function JSON.lower(x::Telemetry)
    # Reorder backtraces
    reverse_backtrace = Dict(v => k for (k,v) in x.backtraces)
    backtraces = [get(reverse_backtrace, i, "null") for i in Base.OneTo(length(reverse_backtrace))]

    return Dict(
        :logs => x.logs,
        :objects => x.objects,
        :backtraces => backtraces,
    )
end

function JSON.lower(x::NoBacktraces)
    telemetry = x.telemetry
    return Dict(
        :logs => telemetry.logs,
        :objects => telemetry.objects,
    )
end

const SC = JSON.StructuralContext

function JSON.show_json(io::SC, ::TelemetrySerialization, x::BacktraceType)
    # Step 1: Convert from a vector of pointers to a full on stacktrace.
    trace = prettify(stacktrace(x))
    return JSON.show_json(io, TelemetrySerialization(), trace)
end

function JSON.show_json(io::SC, ::TelemetrySerialization, x::Base.StackTraces.StackFrame)
    # Don't worry about macro expansions.
    if x.func == Symbol("macro expansion")
        return JSON.show_json(io, JSON.StandardSerialization(), "omitted")
    end

    if startswith(String(x.func), "top-level") || startswith(String(x.file), "REPL")
        return JSON.show_json(io, JSON.StandardSerialization(), "omitted")
    end

    dict = Dict(
        :func => x.func,
        :file => x.file,
        :line => x.line,
        :linfo => x.linfo
    )
    return JSON.show_json(io, TelemetrySerialization(), dict)
end

# Make type printing a little less heinous
tostring(x) = string(x)
function tostring(x::DataType)
    if isempty(x.parameters)
        return string(x)
    else
        inner = join(tostring.(x.parameters), ", ")
        return "$(x.name.module).$(x.name.name){$inner}"
    end
end

function tostring(::Type{NamedTuple{names,T}}) where {names,T}
    return "NamedTuple{$names, $(tostring(T))}"
end

tostring(x::Core.TypeofBottom) = "Union{}"
tostring(::Type{<:CachedArray{T,N}}) where {T,N} = "CachedArray{$T,$N}"
tostring(::Type{<:CacheManager}) = "CacheManager"

# Top level save functions
function JSON.show_json(io::SC, ::TelemetrySerialization, x::Core.MethodInstance)
    return JSON.show_json(io, JSON.StandardSerialization(), tostring.(x.specTypes.parameters))
end

function save_trace(file::AbstractString, x::Union{Telemetry, NoBacktraces})
    open(io -> save_trace(io, x), file; write = true)
end

function save_trace(io::IO, x::Union{Telemetry, NoBacktraces})
    return JSON.show_json(io, TelemetrySerialization(), x; indent = 4)
end

# #####
# ##### For mistakes
# #####
#
# just_comma(str) = match(r"^\s*,\s*$", str) !== nothing
# just_space(str) = all(isspace, str)
#
# function clean_json(dst::AbstractString, src::AbstractString)
#     ispath(dst) && rm(dst)
#     open(dst; write = true, create = true) do io
#         # Load up things into an IOBuffer
#         buf = IOBuffer()
#         last = ""
#         for ln in eachline(src)
#             last = clean_json(buf, ln, last)
#         end
#         println(buf, last)
#         seekstart(buf)
#
#         # Stream out remainders
#         last = ""
#         for ln in eachline(buf)
#             if match(r"^\s*{\s*", ln) !== nothing && endswith(last, "}")
#                 last = "$last,"
#             end
#             println(io, last)
#             last = ln
#         end
#         println(io, last)
#     end
#     return nothing
# end
#
# function clean_json(io::IO, str, last)
#     if just_comma(str)
#         last = replace(last, "," => "")
#     end
#     isempty(last) || println(io, last)
#
#     if just_comma(str) || just_space(str) || isempty(str)
#         return ""
#     end
#     return str
# end
