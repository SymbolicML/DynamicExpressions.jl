using DynamicExpressions

maximum_residual = 1e-2
custom_cos(x) = cos(x)

operators = OperatorEnum(; binary_operators=(/, +, *), unary_operators=(exp, custom_cos))