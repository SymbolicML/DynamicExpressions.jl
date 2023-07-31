using DynamicExpressions
using Test
using Zygote

# Before defining OperatorEnum, calling the implicit (deprecated)
# syntax should fail:
tree = Node(; feature=1)

if VERSION >= v"1.8"
    @test_throws ErrorException tree([1.0 2.0]')
    @test_throws "Please use the " tree([1.0 2.0]')
    @test_throws ErrorException tree'([1.0 2.0]')
    @test_throws "Please use the " tree'([1.0 2.0]')
end

@test string(tree) == "x1"
@test string(Node(1, tree)) == "unary_operator[1](x1)"
@test string(Node(1, tree, tree)) == "binary_operator[1](x1, x1)"

# Also test warnings:
for constructor in (OperatorEnum, GenericOperatorEnum)
    operators = constructor(;
        binary_operators=[+, -, *, /],
        unary_operators=[cos, sin],
        (constructor == OperatorEnum ? (enable_autodiff=true,) : ())...,
    )
    tree([1.0 2.0]')
    # Can't test for this:
    # expected_warn_msg = "The `tree(X; kws...)` syntax is deprecated"
    # @test occursin(expected_warn_msg, msg)

    constructor == GenericOperatorEnum && continue

    tree'([1.0 2.0]')
    # Can't test for this:
    # expected_warn_msg = "The `tree'(X; kws...)` syntax is deprecated"
    # @test occursin(expected_warn_msg, msg)
end
