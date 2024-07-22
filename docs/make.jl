using Documenter
using DynamicExpressions
using Random: AbstractRNG
using Literate: markdown

####################################
# Literate #########################
####################################

# Function to process literate blocks in test files
function process_literate_blocks()
    test_dir = joinpath(@__DIR__, "..", "test")
    for file in readdir(test_dir)
        if endswith(file, ".jl")
            process_file(joinpath(test_dir, file))
        end
    end
end

function process_file(filepath)
    content = read(filepath, String)
    blocks = match_literate_blocks(content)
    for (output_file, block_content) in blocks
        process_literate_block(output_file, block_content, filepath)
    end
end

function match_literate_blocks(content)
    pattern = r"^(\s*)#literate_begin\s+file=\"(.*?)\"\n(.*?)#literate_end"sm
    matches = collect(eachmatch(pattern, content))
    return Dict(
        m.captures[2] => process_block_content(m.captures[1], m.captures[3]) for
        m in matches
    )
end

function process_block_content(indent, block_content)
    if isempty(block_content)
        return ""
    end
    indent_length = length(indent)
    lines = split(block_content, '\n')
    stripped_lines = [
        if length(line) > indent_length
            line[(indent_length + 1):end]
        else
            ""
        end for line in lines
    ]
    return strip(join(stripped_lines, '\n'))
end

function process_literate_block(output_file, content, source_file)
    # Create a temporary .jl file
    temp_file = tempname() * ".jl"
    write(temp_file, content)

    # Process the temporary file with Literate.markdown
    output_dir = joinpath(@__DIR__, "src")
    base_name = first(splitext(basename(output_file))) # Remove any existing extension

    markdown(temp_file, output_dir; name=base_name, documenter=true)

    # Generate the relative path for EditURL
    edit_path = relpath(source_file, joinpath(@__DIR__, "src"))

    # Read the generated markdown file
    md_file = joinpath(output_dir, base_name * ".md")
    md_content = read(md_file, String)

    # Replace the existing EditURL with the correct one
    new_content = replace(md_content, r"EditURL = .*" => "EditURL = \"$edit_path\"")

    # Write the updated content back to the file
    write(md_file, new_content)

    @info "Processed literate block to $md_file with EditURL set to $edit_path"
end

# Call the function to process literate blocks
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

    ```@contents
    Pages = ["utils.md", "api.md", "eval.md"]
    ```
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
