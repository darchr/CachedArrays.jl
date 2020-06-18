using Documenter
#using CachedArrays

makedocs(
    modules     = [Documenter],
    format = Documenter.HTML(
        prettyurls = get(ENV, "CI", nothing) == "true"
    ),
    sitename    = "CachedArrays",
    doctest     = true,
    pages       = Any[
        "Getting Started"   => "index.md",
        "MemoryAllocation" => Any[
            "blocks.md",
        ],
    ],
)

deploydocs(
    repo        = "github.com/darchr/CachedArrays.jl.git",
    target      = "build",
)
