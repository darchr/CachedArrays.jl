#####
##### Random replacement policy
#####

struct RandomPolicy{T}
    objects::Set{T}
end

RandomPolicy{T}() where {T} = RandomPolicy{T}(Set{T}())

Base.eltype(::RandomPolicy{T}) where {T} = T
fulleltype(::RandomPolicy{T}) where {T} = T
Base.isempty(R::RandomPolicy) = isempty(R.objects)

# Could pop off the end, but this ensures more entropy.
function Base.pop!(R::RandomPolicy)
    x = rand(R.objects)
    delete!(R, x)
    return x
end

fullpop!(R::RandomPolicy) = pop!(R)

Base.push!(R::RandomPolicy{T}, v::T) where {T} = push!(R.objects, v)
update!(R::RandomPolicy, v) = nothing

Base.delete!(R::RandomPolicy, v) = delete!(R.objects, v)
Base.in(v, R::RandomPolicy) = in(v, R.objects)
