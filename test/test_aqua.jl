using DynamicExpressions
using Aqua

if VERSION >= v"1.9"
    Aqua.test_all(DynamicExpressions; project_toml_formatting=false)
end
