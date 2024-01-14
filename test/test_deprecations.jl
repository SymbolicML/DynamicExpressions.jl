using DynamicExpressions
using Test
using Zygote

operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[cos, sin])
x1, x2 = Node{Float64}(; feature=1), Node{Float64}(; feature=2)
tree = cos(2.1 * x1)

# Also test warnings:
for constructor in (OperatorEnum, GenericOperatorEnum)
    operators = constructor(; binary_operators=[+, -, *, /], unary_operators=[cos, sin])
    VERSION >= v"1.9" &&
        @test_warn "The `tree(X; kws...)` syntax is deprecated" tree([1.0; 2.0;;])

    constructor == GenericOperatorEnum && continue

    tree'([1.0; 2.0;;])
    VERSION >= v"1.9" &&
        @test_warn "The `tree(X; kws...)` syntax is deprecated" tree'([1.0; 2.0;;])
end
