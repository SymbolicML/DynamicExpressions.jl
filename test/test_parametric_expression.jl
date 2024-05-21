"""Test what happens if we create an expression with a parametric field."""

using Test
using DynamicExpressions

let parameters = [1.0 2.0 3.0],
    ex = @parse_expression(
        sin(x) + p,
        operators = OperatorEnum(; binary_operators=(+, -), unary_operators=(sin,)),
        variable_names = ["x"],
        node_type = ParametricNode,
        expression_type = ParametricExpression,
        extra_metadata = (; parameters, parameter_names=["p"]),
    ),
    X = [0.0 π / 2 π 3π / 2 2π]

    @test ex isa ParametricExpression{Float64}
    @test ex.tree isa ParametricNode{Float64}
    @test string_tree(ex) == "sin(x) + p"

    # Evaluate on X with classes [1]
    @test ex(X, [1, 1, 1, 1, 1]) ≈ [1.0, 2.0, 1.0, 0.0, 1.0]

    # Then, with different classes
    @test ex(X, [1, 2, 2, 3, 1]) ≈ [1.0, 3.0, 2.0, 2.0, 1.0]
end

# With 2 parameters, 2 variables
let parameters = [
        1.0 1.0 0.8
        2.0 3.0 5.0
    ],
    ex = @parse_expression(
        sin(x) + y + p1 * p2,
        operators = OperatorEnum(; binary_operators=(+, -, *), unary_operators=(sin,)),
        variable_names = ["x", "y"],
        node_type = ParametricNode,
        expression_type = ParametricExpression,
        extra_metadata = (; parameters, parameter_names=["p1", "p2"]),
    ),
    X = [
        0.0 π/2 π 1.2
        0.0 0.0 1.5 0.1
    ],
    param_idx = [1, 1, 2, 3]

    @test string_tree(ex) == "(sin(x) + y) + (p1 * p2)"

    @test ex(X, param_idx) ≈ [
        sin(0.0) + 0.0 + 1.0 * 2.0
        sin(π / 2) + 0.0 + 1.0 * 2.0
        sin(π) + 1.5 + 1.0 * 3.0
        sin(1.2) + 0.1 + 0.8 * 5.0
    ]
end
