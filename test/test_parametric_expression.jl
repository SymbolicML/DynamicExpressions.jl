@testitem "Conversion should not change metadata" begin
    using DynamicExpressions

    p1 = ParametricNode{Float64}()
    p1.degree = 0
    p1.constant = false
    p1.is_parameter = true
    p1.parameter = 2

    # Default string prints the feature value (random value; as its undefined)
    @test startswith(repr(p1), "x")

    @test copy(p1).degree == 0
    @test copy(p1).constant == false
    @test copy(p1).is_parameter == true
    @test copy(p1).parameter == 2

    # Converting eltype shouldn't change this
    @test convert(ParametricNode{Float32}, p1).degree == 0
    @test convert(ParametricNode{Float32}, p1).constant == false
    @test convert(ParametricNode{Float32}, p1).is_parameter == true
end
@testitem "Interface for parametric expressions" begin
    using DynamicExpressions
    using DynamicExpressions: ExpressionInterface, NodeInterface
    using Interfaces: test

    ex = @parse_expression(
        x + y + p1 * p2,
        binary_operators = [+, -, *, /],
        variable_names = ["x", "y"],
        node_type = ParametricNode,
        expression_type = ParametricExpression,
        extra_metadata = (; parameters=ones(2, 5), parameter_names=["p1", "p2"]),
    )
    @test test(ExpressionInterface, ParametricExpression, [ex])

    x1 = ParametricNode{Float64}(; feature=1)
    x2 = ParametricNode{Float64}(; feature=2)

    operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])

    tree_branch_deg2 = x1 + sin(x2 * 3.5)
    tree_branch_deg1 = sin(x1)
    tree_leaf_feature = x1
    tree_leaf_constant = ParametricNode{Float64}(; val=1.0)

    @test test(
        NodeInterface,
        ParametricNode,
        [
            ex.tree,
            tree_branch_deg2,
            tree_branch_deg1,
            tree_leaf_feature,
            tree_leaf_constant,
        ],
    )
end
@testitem "Basic evaluation of parametric expressions" begin
    using DynamicExpressions

    parameters = [1.0 2.0 3.0]
    ex = @parse_expression(
        sin(x) + p,
        operators = OperatorEnum(; binary_operators=(+, -), unary_operators=(sin,)),
        variable_names = ["x"],
        node_type = ParametricNode,
        expression_type = ParametricExpression,
        extra_metadata = (; parameters, parameter_names=["p"]),
    )
    X = [0.0 π / 2 π 3π / 2 2π]

    @test ex isa ParametricExpression{Float64}
    @test ex.tree isa ParametricNode{Float64}
    @test string_tree(ex) == "sin(x) + p"

    # Evaluate on X with classes [1]
    @test ex(X, [1, 1, 1, 1, 1]) ≈ [1.0, 2.0, 1.0, 0.0, 1.0]

    # Then, with different classes
    @test ex(X, [1, 2, 2, 3, 1]) ≈ [1.0, 3.0, 2.0, 2.0, 1.0]

    # Helpful error if we use it incorrectly
    if VERSION >= v"1.9"
        @test_throws "Incorrect call. You must pass" ex(X)
    end
end

@testitem "2 parameters, 2 variables" begin
    using DynamicExpressions

    parameters = [
        1.0 1.0 0.8
        2.0 3.0 5.0
    ]
    ex = @parse_expression(
        sin(x) + y + p1 * p2,
        operators = OperatorEnum(; binary_operators=(+, -, *), unary_operators=(sin,)),
        variable_names = ["x", "y"],
        node_type = ParametricNode,
        expression_type = ParametricExpression,
        extra_metadata = (; parameters, parameter_names=["p1", "p2"]),
    )
    X = [
        0.0 π/2 π 1.2
        0.0 0.0 1.5 0.1
    ]
    param_idx = [1, 1, 2, 3]

    @test string_tree(ex) == "(sin(x) + y) + (p1 * p2)"

    @test ex(X, param_idx) ≈ [
        sin(0.0) + 0.0 + 1.0 * 2.0
        sin(π / 2) + 0.0 + 1.0 * 2.0
        sin(π) + 1.5 + 1.0 * 3.0
        sin(1.2) + 0.1 + 0.8 * 5.0
    ]
end

@testitem "Optimization of parametric expressions" begin
    using DynamicExpressions
    using Optim
    using Random: MersenneTwister

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

    true_constants, true_refs = get_scalar_constants(true_ex)
    set_scalar_constants!(init_ex, true_constants, true_refs)
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

@testitem "Allowed empty operators in parametric expression" begin
    using DynamicExpressions: ParametricExpression, ParametricNode

    tree = ParametricNode{Float64}()
    tree.degree = 0
    tree.constant = true
    tree.val = 0.0

    ex = ParametricExpression(
        tree;
        operators=nothing,
        variable_names=nothing,
        parameters=Array{Float32}(undef, 0, 0),
        parameter_names=nothing,
    )
    @test ex.metadata.parameters == Array{Float32}(undef, 0, 0)
    @test ex.metadata.parameter_names === nothing

    @test copy(ex) == ex
end

@testitem "Passing node within ParametricExpression parsing" begin
    using DynamicExpressions

    tree = ParametricNode{Float32}()
    tree.degree = 0
    tree.constant = true
    tree.val = 1.5
    ex = parse_expression(
        :($tree);
        expression_type=ParametricExpression,
        parameters=Array{Float32}(undef, 0, 0),
        parameter_names=nothing,
    )
    @test ex.tree == tree
end

@testitem "Parametric expression conversion" begin
    using DynamicExpressions

    pex = @parse_expression(
        x1 + 1.5f0,
        binary_operators = [+, -, *],
        variable_names = ["x1"],
        expression_type = ParametricExpression{Float32},
        extra_metadata = (; parameters=Array{Float32}(undef, 0, 0), parameter_names=nothing)
    )
    ex = @parse_expression(
        x1 + 1.5f0, binary_operators = [+, -, *], variable_names = ["x1"],
    )

    @test pex.tree isa ParametricNode{Float32}
    tree = convert(Node, pex)
    @test tree isa Node{Float32}
    @test DynamicExpressions.get_tree(ex) == tree
end

@testitem "Parametric expression utilities" begin
    using DynamicExpressions

    ex1 = @parse_expression(
        x + y + p1 * p2 + 1.5,
        binary_operators = [+, -, *],
        variable_names = ["x", "y"],
        expression_type = ParametricExpression,
        extra_metadata = (; parameters=[1.0 2.0; 3.0 4.0], parameter_names=["p1", "p2"]),
    )
    ex2 = @parse_expression(
        x + y + p1 * p2 + 1.5,
        binary_operators = [+, -, *],
        variable_names = ["x", "y"],
        expression_type = ParametricExpression,
        extra_metadata = (; parameters=[1.0 2.0; 3.0 4.0], parameter_names=["p1", "p2"]),
    )
    @test ex1 == ex2
    @test hash(ex1) == hash(ex2)
    @test hash(ex1, UInt64(0)) != hash(ex2, UInt64(1))

    ex3 = @parse_expression(
        x + y + p1 * p2 + 1.5,
        binary_operators = [+, -, *],
        variable_names = ["x", "y"],
        expression_type = ParametricExpression,
        extra_metadata = (; parameters=[1.0 2.0; 3.0 4.1], parameter_names=["p1", "p2"]),
    )
    @test ex1 != ex3
    @test hash(ex1) != hash(ex3)

    ex4 = @parse_expression(
        x + y + p1 * p2 + 1.6,
        binary_operators = [+, -, *],
        variable_names = ["x", "y"],
        expression_type = ParametricExpression,
        extra_metadata = (; parameters=[1.0 2.0; 3.0 4.0], parameter_names=["p1", "p2"]),
    )
    @test ex1 != ex4
    @test hash(ex1) != hash(ex4)
end

@testitem "Parametric expression missing functions" begin
    using DynamicExpressions
    using DynamicExpressions.ParametricExpressionModule: InterfaceError

    ex = @parse_expression(
        x + y + p1 * p2 + 1.5,
        binary_operators = [+, -, *],
        variable_names = ["x", "y"],
        expression_type = ParametricExpression,
        extra_metadata = (; parameters=[1.0 2.0; 3.0 4.0], parameter_names=["p1", "p2"]),
    )

    @test_throws InterfaceError count_constant_nodes(ex)
    @test_throws InterfaceError index_constant_nodes(ex)
    @test_throws InterfaceError has_constants(ex)
    if VERSION >= v"1.9"
        @test_throws "You should not use this function with `ParametricExpression`." count_constants(
            ex
        )
    end
end

@testitem "Parametric expression derivatives" begin
    using DynamicExpressions
    using Zygote: Zygote
    using Random: MersenneTwister
    using DifferentiationInterface: value_and_gradient, AutoZygote

    rng = MersenneTwister(0)
    X = rand(rng, 2, 32)
    true_params = [0.5 2.0]
    init_params = [0.1 0.2]
    classes = rand(rng, 1:2, 32)
    y = [X[1, i] * X[1, i] - cos(2.6 * X[2, i]) + true_params[1, classes[i]] for i in 1:32]

    (true_val, true_grad) =
        value_and_gradient(AutoZygote(), (X, init_params, [2.5])) do (X, params, c)
            pred = [
                X[1, i] * X[1, i] - cos(c[1] * X[2, i]) + params[1, classes[i]] for
                i in 1:32
            ]
            sum(abs2, pred .- y)
        end

    operators = OperatorEnum(; unary_operators=[cos], binary_operators=[+, *, -])
    ex = @parse_expression(
        x * x - cos(2.5 * y) + p1,
        operators = operators,
        expression_type = ParametricExpression,
        variable_names = ["x", "y"],
        extra_metadata = (parameter_names=["p1"], parameters=init_params)
    )
    f = let operators = operators, X = X, classes = classes, y = y
        ex -> sum(abs2, ex(X, classes) .- y)
    end
    @test f(ex) isa Float64
    (val, grad) = value_and_gradient(f, AutoZygote(), ex)

    @test val isa Float64
    @test grad isa NamedTuple
    @test grad.tree isa DynamicExpressions.ChainRulesModule.NodeTangent{
        Float64,ParametricNode{Float64},Vector{Float64}
    }
    @test grad.metadata._data.parameters isa Matrix{Float64}

    # Loss value:
    @test val ≈ true_val
    # Gradient w.r.t. the constant:
    @test grad.tree.gradient ≈ true_grad[3]
    # Gradient w.r.t. the parameters:
    @test grad.metadata._data.parameters ≈ true_grad[2]

    # Gradient extractor
    @test extract_gradient(grad, ex) ≈ vcat(true_grad[3], true_grad[2][:])
    @test axes(extract_gradient(grad, ex)) == axes(first(get_scalar_constants(ex)))
end
