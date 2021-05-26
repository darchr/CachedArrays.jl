function safeprint(message; force = false)
    if (force || VERBOSE)
        ccall(
            :jl_safe_printf,
            Cvoid,
            (Cstring,),
            "$message\n",
        )
    end
    return nothing
end
