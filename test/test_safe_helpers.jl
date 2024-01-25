using DynamicExpressions
using Test

function _square(x)
    return x^2
end

operators = OperatorEnum(;
    binary_operators=[+, -, *, /], unary_operators=[cos, sin, _square]
)
@extend_operators operators
x1, x2, x3 = (i -> Node(Float64; feature=i)).(1:3)

# Should work normally:
@test _square(x1 + x2 / x3) * x2 + 0.5 isa Node

# But, upon redefining `operators`, we should get errors:
operators = OperatorEnum(; binary_operators=[+, -, *], unary_operators=[cos, sin, _square])

# Safe:
@test _square(x1 + x2) * x2 + 0.5 isa Node

# Breaks:
@test_throws ErrorException _square(x1 + x2 / x3) * x2 + 0.5

operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[cos, sin])
# Safe:
@test (x1 + x2 / x3) * x2 + 0.5 isa Node

# Breaks:
@test_throws ErrorException _square(x1 + x2 / x3) * x2 + 0.5

# We can also turn this behavior off:
operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[cos, sin])
operators = OperatorEnum(;
    binary_operators=[+, -, *, /], unary_operators=[cos, tan], empty_old_operators=false
)
@test tan(x1) == sin(x1)

# Should catch errors in kws:
@test_throws LoadError begin
    @eval @extend_operators operators empty_old_operators_bad_kw = true
end
