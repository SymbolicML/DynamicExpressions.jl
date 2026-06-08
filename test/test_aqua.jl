using DynamicExpressions
using Aqua

Aqua.test_all(
    DynamicExpressions;
    piracies=VERSION < v"1.12.0-DEV",
    persistent_tasks=VERSION < v"1.12.0-DEV",
)
