# TODO: Finalize API...
function telemetry_alloc end
function telemetry_gc end
function telemetry_change end
function telemetry_move end

#####
##### Telemetry
#####

@enum TelemetryAction::UInt begin
    AllocFast
    AllocSlow

    # Transitions
    ReadTransition
    WriteTransition
    ReleaseTransition
    TransitionUnknown

    # Movement
    MoveToLocal
    MoveToRemote

    # Cleanup
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
    backtraces::Vector{BacktraceType}
    lock::Base.Threads.SpinLock
end

function Telemetry()
    return Telemetry(
        Dict{UInt, Vector{TelemetryRecord}}(),
        Dict{UInt, AllocationRecord}(),
        Vector{BacktraceType}(),
        Base.Threads.SpinLock(),
    )
end

#####
##### Accessors
#####

ids(telemetry::Telemetry) = sort(collect(keys(telemetry.logs)))
getlog(telemetry::Telemetry, id) = telemetry.logs[id]

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

function telemetry_alloc(telemetry::Telemetry, manager, bytes, id, ptr, location)
    # Get the current time.
    now = time_ns()

    action = (location == Local) ? AllocFast : AllocSlow
    # Find where we are on the stack.
    # If this is a new location, mark it as such.
    bt = backtrace()
    backtraces = telemetry.backtraces
    allocation_record = AllocationRecord(id, bytes, UInt(ptr))
    @spinlock telemetry.lock begin
        telemetry.objects[id] = allocation_record
        key = findfirst(isequal(bt), backtraces)
        if key === nothing
            push!(backtraces, bt)
            key = length(backtraces)::Int
        end

        v = get!(() -> TelemetryRecord[], telemetry.logs, id)
        push!(v, TelemetryRecord(now, key, action))
    end
    return nothing
end

function telemetry_gc(telemetry::Telemetry, id)
    now = time_ns()
    @spinlock telemetry.lock begin
        log = @inbounds(telemetry.logs[id])
        push!(log, TelemetryRecord(now, 0, GarbageCollected))
    end
    return nothing
end

function telemetry_change(telemetry::Telemetry, id, from, to)
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
    bt = backtrace()
    backtraces = telemetry.backtraces
    @spinlock telemetry.lock begin
        key = findfirst(isequal(bt), backtraces)
        if key === nothing
            push!(backtraces, bt)
            key = length(backtraces)::Int
        end

        log = telemetry.logs[id]
        push!(log, TelemetryRecord(now, key, state))
    end
    return nothing
end

function telemetry_move(telemetry::Telemetry, id, location, bytes)
    action = location == Local ? MoveToLocal : MoveToRemote
    record = TelemetryRecord(time_ns(), 0, action)
    @spinlock telemetry.lock begin
        # Log should exist, so don't get fancy trying to allocate it.
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

#####
##### Save Telemetry
#####

# Custom JSON serialization for Telemetry
struct TelemetrySerialization <: JSON.CommonSerialization end

function JSON.lower(x::Telemetry)
    return Dict(
        :logs => x.logs,
        :objects => x.objects,
        :backtraces => x.backtraces,
    )
end

const SC = JSON.StructuralContext

function JSON.show_json(io::SC, ::TelemetrySerialization, x::BacktraceType)
    # Step 1: Convert from a vector of pointers to a full on stacktrace.
    trace = prettify(stacktrace(x))
    return JSON.show_json(io, TelemetrySerialization(), trace)
end

function JSON.show_json(io::SC, ::TelemetrySerialization, x::Base.StackTraces.StackFrame)
    # Don't worry about macrto expansions.
    if x.func == Symbol("macro expansion")
        return nothing
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
tostring(::Type{<:CachedArray{T,N}}) where {T,N} = "CachedArray{$T,$N}"

# Top level save functions
function JSON.show_json(io::SC, ::TelemetrySerialization, x::Core.MethodInstance)
    return JSON.show_json(io, JSON.StandardSerialization(), tostring.(x.specTypes.parameters))
end

function save_trace(file::AbstractString, x::Telemetry)
    open(io -> save_trace(io, x), file; write = true)
end

function save_trace(io::IO, x::Telemetry)
    return JSON.show_json(io, TelemetrySerialization(), x; indent = 4)
    #return JSON.show_json(io, TelemetrySerialization(), x)
end
