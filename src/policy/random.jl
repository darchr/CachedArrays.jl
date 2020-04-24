#####
##### Random replacement policy
#####

struct Random{T}
    objects::Set{T}
end

Base.eltype(::Random{T}) where {T} = T
fulleltype(::Random{T}) where {T} = T
Base.isempty(R::Random) = isempty(R.objects)

# Could pop off the end, but this ensures more entropy.
function Base.pop!(R::Random)
    x = rand(R)
    delete!(R, x)
    return x
end

fullpop!(R::Random) = pop!(R)

Base.push!(R::Random{T}, v::T) where {T} = push!(R.objects, v)
update!(R::Random, v) = nothing

Base.delete!(R::Random, v) = delete!(R.objects, v)
Base.in(v, R::Random) = in(v, R.objects)
