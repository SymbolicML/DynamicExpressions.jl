using DynamicExpressions

maximum_residual = 1e-2
custom_cos(x) = cos(x)

default_params = (
    binary_operators=(/, +, *),
    unary_operators=(exp, custom_cos)
)