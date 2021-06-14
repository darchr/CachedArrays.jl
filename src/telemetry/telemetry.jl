#####
##### Telemetry
#####

@enum TelemetryAction::UInt begin
    AllocFast
    AllocSlow
    GarbageCollected
end

struct TelemetryRecord
    accesstime::UInt
    # Index into a global backtrace buffer.
    backtrace_key::Int
    action::TelemetryAction
end

const BacktraceType = Vector{Union{Ptr{Nothing},Base.InterpreterIP}}

struct Telemetry
    # Map allocated IDs to their sequence of actions.
    logs::Dict{UInt, Vector{TelemetryRecord}}
    backtraces::Vector{BacktraceType}
end

function Telemetry()
    return Telemetry(
        Dict{UInt, Vector{TelemetryRecord}}(),
        Vector{BacktraceType}(),
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
    key = findfirst(isequal(bt), backtraces)
    if key === nothing
        push!(backtraces, bt)
        key = length(backtraces)::Int
    end

    v = get!(() -> TelemetryRecord[], telemetry.logs, id)
    push!(v, TelemetryRecord(now, key, action))
    return nothing
end

function telemetry_gc(telemetry::Telemetry, id)
    now = time_ns()
    log = @inbounds(telemetry.logs[id])
    push!(log, TelemetryRecord(now, 0, GarbageCollected))
    return nothing
end
