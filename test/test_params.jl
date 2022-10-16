using DynamicExpressions

maximum_residual = 1e-2
safe_log(x::T) where T = (x <= 0) ? T(NaN) : log(x)
safe_log2(x::T) where T = (x <= 0) ? T(NaN) : log2(x)
safe_log10(x::T) where T = (x <= 0) ? T(NaN) : log10(x)
safe_sqrt(x::T) where T = (x < 0) ? T(NaN) : sqrt(x)
relu(x::T) where T = (x < 0) ? zero(T) : x
safe_acosh(x::T) where T = (x < 1) ? T(NaN) : acosh(x)
sub(x::T, y::T) where T = x - y

safe_log(x) = log(x)
safe_log2(x) = log2(x)
safe_log10(x) = log10(x)
safe_sqrt(x) = sqrt(x)
relu(x) = max(x, 0)
safe_acosh(x) = acosh(x)
sub(x, y) = x - y

custom_cos(x) = cos(x)

default_params = (
    binary_operators=(/, +, *),
    unary_operators=(exp, custom_cos)
)