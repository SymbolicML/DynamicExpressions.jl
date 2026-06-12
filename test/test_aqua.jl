using DynamicExpressions
using Aqua

const aqua_08_on_julia_112 = Base.pkgversion(Aqua) == v"0.8.0" && VERSION >= v"1.12"

Aqua.test_all(
    DynamicExpressions;
    piracies=!aqua_08_on_julia_112,
    persistent_tasks=!aqua_08_on_julia_112,
)
