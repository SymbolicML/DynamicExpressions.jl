using DynamicExpressions
using Random
using Test
include("test_params.jl")
include("tree_gen_utils.jl")

# Test simple evaluations:
functions = [
    # deg2_l0_r0_eval
    (x1, x2, x3) -> x1 * x2,
    (x1, x2, x3) -> x1 * 3.0,
    (x1, x2, x3) -> 3.0 * x2,
    (((x1, x2, x3) -> 3.0 * 6.0), ((x1, x2, x3) -> Node(; val=3.0) * 6.0)),

    # deg2_l0_eval
    (x1, x2, x3) -> x1 * sin(x2),
    (x1, x2, x3) -> 3.0 * sin(x2),

    # deg2_r0_eval
    (x1, x2, x3) -> sin(x1) * x2,
    (x1, x2, x3) -> sin(x1) * 3.0,

    # deg1_l2_ll0_lr0_eval
    (x1, x2, x3) -> cos(x1 * x2),
    (x1, x2, x3) -> cos(x1 * 3.0),
    (x1, x2, x3) -> cos(3.0 * x2),
    (
        ((x1, x2, x3) -> cos(3.0 * -0.5)),
        ((x1, x2, x3) -> cos(Node(2, Node(; val=3.0), Node(; val=-0.5)))),
    ),

    # deg1_l1_ll0_eval
    (x1, x2, x3) -> cos(sin(x1)),
    (((x1, x2, x3) -> cos(sin(3.0))), ((x1, x2, x3) -> cos(sin(Node(; val=3.0))))),

    # everything else:
    (x1, x2, x3) -> (sin(cos(sin(cos(x1) * x3) * 3.0) * -0.5) + 2.0) * 5.0,
]

for turbo in [Val(false), Val(true)],
    T in [Float16, Float32, Float64, ComplexF32, ComplexF64],
    bumper in [Val(false), Val(true)]

    # Float16 not implemented:
    (turbo isa Val{true} || bumper isa Val{true}) && !(T in (Float32, Float64)) && continue
    turbo isa Val{true} && bumper isa Val{true} && continue
    @testset "Test evaluation of trees with turbo=$turbo, bumper=$bumper, T=$T" begin
        for (i_func, fnc) in enumerate(functions)

            # check if fnc is tuple
            realfnc, nodefnc = if typeof(fnc) <: Tuple
                fnc
            else
                fnc, fnc
            end

            local tree, operators, X
            operators = OperatorEnum(;
                default_params..., binary_operators=(+, *, /, -), unary_operators=(cos, sin)
            )
            tree = nodefnc(Node("x1"), Node("x2"), Node("x3"))
            tree = convert(Node{T}, tree)

            N = 100
            nfeatures = 3
            X = randn(MersenneTwister(0), T, nfeatures, N)

            true_y = realfnc.(X[1, :], X[2, :], X[3, :])
            !all(isfinite.(true_y)) && continue

            @inferred eval_tree_array(tree, X, operators; turbo=turbo, bumper=Val(bumper))

            test_y = eval_tree_array(tree, X, operators; turbo=turbo, bumper=Val(bumper))[1]

            zero_tolerance = (T <: Union{Float16,Complex} ? 1e-4 : 1e-6)
            @test all(abs.(test_y .- true_y) / N .< zero_tolerance)

            test_y_helper = tree(X, operators; turbo=turbo, bumper=Val(bumper))
            @test all(test_y .== test_y_helper)
        end
    end
end

@testset "Test specific branches of evaluation" begin
    for turbo in [false, true], T in [Float16, Float32, Float64, ComplexF32, ComplexF64]
        turbo && !(T in (Float32, Float64)) && continue
        # Test specific branches of evaluation code:
        # op(op(<constant>))
        local tree, operators
        operators = OperatorEnum(;
            default_params..., binary_operators=(+, *, /, -), unary_operators=(cos, sin)
        )
        tree = Node(1, Node(1, Node(; val=3.0f0)))
        @test repr(tree) == "cos(cos(3.0))"
        tree = convert(Node{T}, tree)
        truth = cos(cos(T(3.0f0)))
        @test DynamicExpressions.EvaluateEquationModule.deg1_l1_ll0_eval(tree, [zero(T)]', cos, cos, Val(turbo)).x[1] ≈
            truth

        # op(<constant>, <constant>)
        tree = Node(1, Node(; val=3.0f0), Node(; val=4.0f0))
        @test repr(tree) == "3.0 + 4.0"
        tree = convert(Node{T}, tree)
        truth = T(3.0f0) + T(4.0f0)
        @test DynamicExpressions.EvaluateEquationModule.deg2_l0_r0_eval(tree, [zero(T)]', (+), Val(turbo)).x[1] ≈
            truth

        # op(op(<constant>, <constant>))
        tree = Node(1, Node(1, Node(; val=3.0f0), Node(; val=4.0f0)))
        @test repr(tree) == "cos(3.0 + 4.0)"
        tree = convert(Node{T}, tree)
        truth = cos(T(3.0f0) + T(4.0f0))
        @test DynamicExpressions.EvaluateEquationModule.deg1_l2_ll0_lr0_eval(tree, [zero(T)]', cos, (+), Val(turbo)).x[1] ≈
            truth

        # Test for presence of NaNs:
        operators = OperatorEnum(;
            binary_operators=[+, -, *, /], unary_operators=[cos, sin]
        )
        x1 = Node(T; feature=1)
        tree = sin(x1 / 0.0)
        X = randn(Float32, 3, 10)
        @test isnan(tree(X, operators; turbo=turbo)[1])
    end
end

# Check if julia version >= 1.7:
if VERSION >= v"1.7"
    @testset "Test error catching for GenericOperatorEnum" begin
        # And, with generic operator enum, this should be an actual error:
        operators = GenericOperatorEnum(;
            binary_operators=[+, -, *, /], unary_operators=[cos, sin]
        )
        x1 = Node(Float64; feature=1)
        tree = sin(x1 / 0.0)
        X = randn(Float32, 10)
        local stack
        try
            tree(X, operators)[1]
            @test false
        catch e
            @test e isa ErrorException
            # Check that "Failed to evaluate" is in the message:
            @test occursin("Failed to evaluate", e.msg)
            stack = current_exceptions()
        end
        @test length(stack) == 2
        @test stack[1].exception isa DomainError

        # If a method is not defined, we should get a nothing:
        X = randn(Float32, 1, 10)
        @test tree(X, operators; throw_errors=false) === nothing
        # or a MethodError:
        try
            tree(X, operators; throw_errors=true)
            @test false
        catch e
            @test e isa ErrorException
            @test occursin("Failed to evaluate", e.msg)
            stack = current_exceptions()
        end
        @test length(stack) == 2
        @test stack[1].exception isa MethodError
    end
end

@testset "Test many operators" begin
    # Since we use `@nif` in evaluating expressions,
    # we can see if there are any issues with LARGE numbers of operators.
    num_ops = 100
    binary_operators = [@eval function (x, y)
        return x + y
    end for i in 1:num_ops]
    unary_operators = [@eval function (x)
        return x^2
    end for i in 1:num_ops]
    operators = if VERSION >= v"1.9"
        @test_logs (:warn, r"You have passed over 15 binary.*") OperatorEnum(;
            binary_operators, unary_operators
        )
    else
        OperatorEnum(; binary_operators, unary_operators)
    end
    tree = Node(1, Node(num_ops ÷ 2, Node(; val=3.0), Node(; feature=2)))
    # = (3.0 + x2)^2
    X = randn(Float64, 2, 10)
    truth = @. (3.0 + X[2, :])^2
    @test truth ≈ tree(X, operators)

    VERSION >= v"1.9" &&
        @test_logs (:warn, r"You have passed over 15 unary.*") OperatorEnum(;
            unary_operators
        )

    # This OperatorEnum will trigger the fallback code for fast compilation.
    many_ops_operators = OperatorEnum(;
        binary_operators=cat([+, -, *, /], binary_operators; dims=1),
        unary_operators=cat([sin, cos], unary_operators; dims=1),
    )

    # This OperatorEnum will go through the regular evaluation code.
    only_basic_ops_operator = OperatorEnum(;
        binary_operators=[+, -, *, /], unary_operators=[sin, cos]
    )

    # We want to compare them:
    num_tests = 100
    n_features = 3
    for _ in 1:num_tests
        tree = gen_random_tree_fixed_size(20, only_basic_ops_operator, n_features, Float64)
        X = randn(Float64, n_features, 10)
        basic_eval = tree(X, only_basic_ops_operator)
        many_ops_eval = tree(X, many_ops_operators)
        @test (all(isnan, basic_eval) && all(isnan, many_ops_eval)) ||
            basic_eval ≈ many_ops_eval
    end
end
