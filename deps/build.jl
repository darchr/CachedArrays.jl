# Download the PMDK repo
using LibGit2

url = "https://github.com/memkind/memkind"
tag = "v1.10.0"
localdir = joinpath(@__DIR__, "memkind")
installdir = joinpath(@__DIR__, "usr")

if !ispath(localdir)
    LibGit2.clone(url, localdir)
    # This is a hack for now - eventually, I need to figure out how to do this
    # using LibGit2.
    #
    # However, the `branch` keyword argument doesn't seem to work for tabs.
    cd(localdir)
    run(`git checkout $tag`)
end

cd(localdir)
run(`./autogen.sh`)
run(`./configure --prefix=$installdir`)
nproc = parse(Int, read(`nproc`, String))
run(`make -j$nproc`)
run(`make install`)

