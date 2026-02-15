using DynamicExpressions
using Aqua

if VERSION >= v"1.9"
    # Aqua's piracy check relies on some Julia internals that changed in Julia 1.12,
    # which can cause a hard error (FieldError: Core.TypeName has no field `mt`).
    # We still run piracy checks on older Julia versions.
    piracy_ok = VERSION < v"1.12.0-"
    Aqua.test_all(DynamicExpressions; project_toml_formatting=false, piracy=piracy_ok)
end
