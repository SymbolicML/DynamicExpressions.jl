using DynamicExpressions
using Aqua

Aqua.test_all(DynamicExpressions; project_toml_formatting=false, unbound_args=false)
