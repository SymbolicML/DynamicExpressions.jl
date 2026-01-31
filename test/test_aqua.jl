using DynamicExpressions
using Aqua

# `project_toml_formatting` keyword was added in newer Aqua versions.
# Keep compatibility with older Aqua while allowing newer versions to disable formatting checks.
try
    Aqua.test_all(DynamicExpressions; project_toml_formatting=false)
catch err
    if err isa MethodError && occursin("project_toml_formatting", sprint(showerror, err))
        Aqua.test_all(DynamicExpressions)
    else
        rethrow()
    end
end
