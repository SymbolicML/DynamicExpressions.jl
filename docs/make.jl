using Documenter
using DynamicExpressions
using Random: AbstractRNG

makedocs(;
    sitename="DynamicExpressions.jl",
    authors="Miles Cranmer",
    doctest=false,
    clean=true,
    format=Documenter.HTML(),
    warnonly=true,
)

# Forward links from old docs:

redirect_page = """
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Redirecting...</title>
    <script type="text/javascript">
        var fragment = window.location.hash;
        window.location.href = "../api/" + fragment;
    </script>
</head>
<body>
    <p>If you are not redirected automatically, follow this <a id="redirect-link" href="../api/">link to API</a>.</p>
    <script type="text/javascript">
        document.getElementById('redirect-link').href = "../api/" + window.location.hash;
    </script>
</body>
</html>
"""

# Create the types directory and write the redirect page
types_dir = joinpath(@__DIR__, "build", "types")
mkpath(types_dir)
redirect_file = joinpath(types_dir, "index.html")
open(redirect_file, "w") do f
    write(f, redirect_page)
end

deploydocs(; repo="github.com/SymbolicML/DynamicExpressions.jl.git")
