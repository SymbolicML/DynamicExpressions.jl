using Documenter, DynamicExpressions, Zygote
using Random: AbstractRNG
using Literate: markdown

####################################
# Literate #########################
####################################

include("utils.jl")
process_literate_blocks()

####################################
# index.md #########################
####################################

readme = joinpath(@__DIR__, "..", "README.md")

index_content = let r = read(readme, String)
    # Wrap img tags in raw HTML blocks:
    r = replace(r, r"(<img\s+[^>]+>)" => s"""

```@raw html
\1
```

""")
    # Remove end img tags:
    r = replace(r, r"</img>" => "")
    # Remove div tags:
    r = replace(r, r"<div[^>]*>" => "")
    # Remove end div tags:
    r = replace(r, r"</div>" => "")

    top_part = """
    # Introduction

    """

    bottom_part = """
    ## Contents

    """

    join((top_part, r, bottom_part), "\n")
end

index_md = joinpath(@__DIR__, "src", "index.md")
open(index_md, "w") do f
    write(f, index_content)
end

####################################

makedocs(;
    sitename="DynamicExpressions.jl",
    authors="Miles Cranmer",
    clean=true,
    format=Documenter.HTML(;
        canonical="https://symbolicml.org/DynamicExpressions.jl/stable"
    ),
    pages=[
        "Home" => "index.md",
        "Examples" => [
            "examples/base_operations.md", # Defined by `test/test_base_2.jl`
            "examples/expression.md", # Defined by `test/test_expression.jl`
            "examples/structured_expression.md", # Defined by `test/test_structured_expression.jl`
        ],
        "Eval" => "eval.md",
        "Utils" => "utils.md",
        "API" => "api.md",
    ],
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
