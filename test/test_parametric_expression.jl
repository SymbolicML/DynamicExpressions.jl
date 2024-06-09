"""Test what happens if we create an expression with a parametric field."""

using Test
using DynamicExpressions
using Random: MersenneTwister
using Optim

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

let
    variable_names = ["x", "y"]
    parameter_names = ["p1", "p2"]
    binary_operators = [+, -, *, /]
    unary_operators = [sin]
    expression_type = ParametricExpression

    rng = MersenneTwister(0)
    true_parameters = [
        -0.2 +0.2 +0.3
        +1.4 +0.5 -0.9
    ]
    init_parameters = zero(true_parameters)
    X = [
        11 12 13 14 15 16 17 18 19.0
        21 22 23 24 25 26 27 28 29
    ]
    classes = [1, 2, 3, 3, 2, 1, 1, 2, 3]

    (init_ex, true_ex) = map(
        p -> parse_expression(
            :((x * p2) + y + p1);
            variable_names,
            binary_operators,
            unary_operators,
            expression_type,
            parameters=p,
            parameter_names,
        ),
        (copy(init_parameters), copy(true_parameters)),
    )
    s = string_tree(init_ex)
    @test s == "((x * p2) + y) + p1"

    @test init_ex.metadata.parameters == init_parameters
    @test true_ex.metadata.parameters == true_parameters

    (init_out, true_out) = map(
        p -> [
            (X[1, i] * p[2, classes[i]] + X[2, i] + p[1, classes[i]]) for i in 1:size(X, 2)
        ],
        (init_parameters, true_parameters),
    )

    true_init_out = init_ex(X, classes)
    true_true_out = true_ex(X, classes)

    @test init_out == true_init_out
    @test true_out == true_true_out

    true_constants, true_refs = get_constants(true_ex)
    set_constants!(init_ex, true_constants, true_refs)
    @test init_ex.metadata.parameters == true_parameters

    init_loss = sum(abs2, init_out - true_out)
    # # Check if we can optimize this
    result = optimize(
        let classes = classes, X = X, true_out = true_out
            ex -> sum(abs2, ex(X, classes) - true_out)
        end,
        init_ex,
        Optim.BFGS(),
    )
    @test result.minimum < 1e-5 && init_loss > 0.1
    @test result.minimizer.metadata.parameters ≈ true_parameters
end
