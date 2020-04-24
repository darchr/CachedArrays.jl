#####
##### Get counters and timing numbers
#####

const DATAFILE = joinpath(@__DIR__, "..", "data.jls")

# A lot of this is copied directly from the `CountOnMe` project repo.
# TODO: Can we figure out an API for this to roll it into CounterTools for making
# these kinds of measurements a lot easier??
getdb() = ispath(DATAFILE) ? ExperimentsDB.load(DATAFILE) : ExperimentsDB.Database()

# Alow for debugging
@static if DEBUG
    save(x) = nothing
else
   save(x) = ExperimentsDB.save(DATAFILE, x)
end

# By default, we run on Socket 1
default_cpuset() = 25:48
default_socket() = CounterTools.IndexZero(1)
default_cha_sample_cpu() = CounterTools.IndexZero(72)
daemon_port() = 2000

const CoreSelectRegister = CounterTools.CoreSelectRegister
const UncoreSelectRegister = CounterTools.UncoreSelectRegister

function dictpush!(d, x, v::T) where {T}
    vec = get!(d, x, T[])
    push!(vec, v)
    return nothing
end

#####
##### Parameters for the benchmarks we are running.
#####

Base.@kwdef struct BenchmarkParameters
    # Dimensions of the internal arrays
    arraydims::Tuple
    totalsize::Int
    # sampletime for measurements
    sampletime::TimePeriod
    # Brief description of the benchmark
    description::String
    kernel::String
    iterations::Int
    # number of threads
    num_threads::Int
    mode::String
end

# Convert this parameter to a dict.
todict(x::BenchmarkParameters) = SortedDict(f => getproperty(x, f) for f in fieldnames(typeof(x)))

function imcmeasurements(params, data::Dict{String,Any}; applyfilter = true)
    eventlist = [
        "CAS Count Read"    => UncoreSelectRegister(; event = 0x04, umask = 0x03),
        "CAS Count Write"   => UncoreSelectRegister(; event = 0x04, umask = 0x0C),
        "PMM Read"          => UncoreSelectRegister(; event = 0xE3),
        "PMM Write"         => UncoreSelectRegister(; event = 0xE7),
        "Tag Hit"           => UncoreSelectRegister(; event = 0xD3, umask = 0x01),
        "Tag Miss Clean"    => UncoreSelectRegister(; event = 0xD3, umask = 0x02),
        "Tag Miss Dirty"    => UncoreSelectRegister(; event = 0xD4, umask = 0x04),
        "Uncore Clocks"     => UncoreSelectRegister(; event = 0x00, umask = 0x00),
    ]

    # Optionally filter
    if applyfilter
        filter!(x -> !haskey(data, makekey(x)), eventlist)
    end

    # wrap this up in an iterators.partition to only yield a maximum of 4 values at a time.
    # only 4 core counters are programmable at a time.
    numcounters = 4
    socket = default_socket()
    return map(Iterators.partition(eventlist, numcounters)) do x
        keys = makekey.(x)
        events = last.(x)
        return keys => MattDaemon.@measurement CounterTools.IMCMonitor(Tuple(events), socket)
    end
end

#####
##### Running and Recording Logic
#####

# Use a `maybeiterate` function to
# - Iterate if the iterate if the iterator isn't done.
# - Do nothing if it IS done.
maybeiterate(itr, state::Tuple) = iterate(itr, last(state))
maybeiterate(itr, ::Nothing) = nothing

maybefirst(state::Tuple) = first(state)
maybefirst(::Nothing) = nothing

_first(x::AbstractString) = x
_first(x::Pair) = first(x)

makekey(x...) = join(_first.(x), " - ")

function runner(@nospecialize(f), params::BenchmarkParameters; force = false)
    # Now, grab the database.
    # See if we have entries for this.
    db = getdb()
    paramsdict = todict(params)

    printstyled(stdout, "Current Parameters\n"; color = :cyan)
    for (k,v) in paramsdict
        println("    ", k, " => ", v)
    end

    entry = ExperimentsDB.findonly(db, paramsdict)
    if isnothing(entry)
        println("Entry Does not yet exist in Database")
        data = Dict{String,Any}()
        db[paramsdict] = data
    else
        println("Found Entry in Database!")
        data = entry[]
    end

    # This may look overly compilcated, but that's because it's designed to be
    # quickly augmented with `core` and `cha` measurements if desired.
    imc = imcmeasurements(params, data; applyfilter = !force)
    iters = (imc,)
    states = iterate.(iters)
    order = (:imc,)
    timestamp = MattDaemon.@measurement SystemSnoop.Timestamp()

    # Iterate until all states are exhausted.
    firstiter = true
    while !all(isnothing, states)
        # Run the function once to trigger compilation
        if firstiter
            f()
            firstiter = false
        end

        measurements = (pretime = timestamp,)
        # Create a measurements tuple.
        #
        # We begin by unpacking the iterator tuples to get actual values from these iterators.
        values = maybefirst.(states)

        for (i, v) in enumerate(values)
            # If this iterator is empty, don't apply it to any measurement.
            isnothing(v) && continue

            # Create a Named tuple using this symbol
            symbol = order[i]
            keys = first(v)
            measurement = last(v)

            nt = NamedTuple{(symbol,)}((measurement,))
            measurements = merge(measurements, nt)

            # Print out a helpful message
            name = uppercase(String(symbol))
            printstyled(stdout, "Running $name Measurements: "; color = :green)
            println(stdout, keys)
        end


        # Apply one final timestamp
        measurements = merge(measurements, (posttime = timestamp,))

        # Construct a payload and launch the function.
        payload = MattDaemon.ServerPayload(params.sampletime, measurements)
        @time run_data, _, runtime = MattDaemon.run(
            f,
            payload,
            daemon_port();
            sleeptime = Second(5)
        )

        @show runtime
        dictpush!(data, "Runtimes", runtime)
        # Print some statistics on measurement times.
        measurement_times = Dates.value.(Dates.Millisecond.(run_data.posttime .- run_data.pretime))
        printstyled(stdout, "Measurement Times\n"; color = :yellow)
        println("    Minimum: ", minimum(measurement_times), " (ms)")
        println("    Maximum: ", maximum(measurement_times), " (ms)")
        println("    Average: ", mean(measurement_times), " (ms)")

        #####
        ##### Post processing
        #####

        for (i,v) in enumerate(values)
            isnothing(v) && continue

            symbol = order[i]
            keys = first(v)

            counter_values = CounterTools.aggregate.(diff(getproperty(run_data, symbol)))
            for (i, m) in enumerate(keys)
                data[makekey(m)] = getindex.(counter_values, i)
            end
        end

        states = maybeiterate.(iters, states)
        save(db)
    end
    return nothing
end

#####
##### The actual entry points
#####

function tests_1d(totalsize, arraysize)
    # if in 2LM mode, all of the temporary arrays should go into local memory
    if IS_2LM
        # Round up for headroom
        localsize = round(Int, totalsize * 1.5)
        remotesize = 4096
    # Size local mamory to be a little smaller than the DRAM cache to allow for other
    # program activities that don't belong to our heap.
    else
        localsize = DEBUG ? totalsize >> 1 : 180_000_000_000
        remotesize = DEBUG ? 2 * totalsize : 1_000_000_000_000
    end

    manager = CachedArrays.CacheManager(
        ENV["JULIA_PMEM_PATH"];
        localsize = localsize,
        remotesize = remotesize
    )

    arrays = alloc_1d(manager, totalsize, arraysize)
    mode = IS_2LM ?  "2LM" : "1LM"

    #####
    ##### Tests to Run
    #####

    # If we have a small total size, do more iterations.
    if totalsize < 180_000_000_000
        iterations = 10
    else
        iterations = 2
    end

    # Sequential Copy
    params = BenchmarkParameters(;
        arraydims = size(first(arrays)),
        totalsize = sum(sizeof, arrays),
        # Sample twice per second
        sampletime = Millisecond(500),
        description = "Sequential Copy",
        kernel = string(sequential_copy),
        iterations = iterations,
        mode = mode,
        num_threads = length(default_cpuset())
    )

    f = () -> sequential_copy(arrays; iterations = iterations)
    runner(f, params)

    # Sequential Accumulate
    params = BenchmarkParameters(;
        arraydims = size(first(arrays)),
        totalsize = sum(sizeof, arrays),
        # Sample twice per second
        sampletime = Millisecond(500),
        description = "Sequential Accumulate",
        kernel = string(sequential_accumulation),
        iterations = iterations,
        mode = mode,
        num_threads = length(default_cpuset())
    )

    f = () -> sequential_accumulation(arrays; iterations = iterations)
    runner(f, params)

    # Sequential zero
    params = BenchmarkParameters(;
        arraydims = size(first(arrays)),
        totalsize = sum(sizeof, arrays),
        # Sample twice per second
        sampletime = Millisecond(500),
        description = "Sequential Zero",
        kernel = string(sequential_zero),
        iterations = iterations,
        mode = mode,
        num_threads = length(default_cpuset())
    )

    f = () -> sequential_zero(arrays; iterations = iterations)
    runner(f, params)
    return nothing
end

function tests_2d()
    # Setup and get arrays
    setup()
    arrays = alloc_2d()
    mode = IS_2LM ?  "2LM" : "1LM"

    #####
    ##### Tests to Run
    #####

    iterations = 1

    # Sequential Copy
    params = BenchmarkParameters(;
        arraydims = size(first(arrays)),
        totalsize = sum(sizeof, arrays),
        # Sample twice per second
        sampletime = Millisecond(500),
        description = "Matrix Multiplies",
        kernel = string(stepping_square_matrix_mult),
        iterations = iterations,
        mode = mode,
        num_threads = length(default_cpuset())
    )

    f = () -> stepping_square_matrix_mult(arrays; iterations = iterations)
    runner(f, params; force = true)
end
