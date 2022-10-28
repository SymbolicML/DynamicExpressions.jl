using DynamicExpressions
using Random
using Test
include("test_params.jl")

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

for turbo in [false, true],
    T in [Float16, Float32, Float64],
    (i_func, fnc) in enumerate(functions)

    # Float16 not implemented:
    turbo && T == Float16 && continue

    # check if fnc is tuple
    if typeof(fnc) <: Tuple
        realfnc = fnc[1]
        nodefnc = fnc[2]
    else
        realfnc = fnc
        nodefnc = fnc
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

    test_y = eval_tree_array(tree, X, operators; turbo=turbo)[1]
    true_y = realfnc.(X[1, :], X[2, :], X[3, :])

    zero_tolerance = (T == Float16 ? 1e-4 : 1e-6)
    try
        @test all(abs.(test_y .- true_y) / N .< zero_tolerance)
    catch
        println("Test for type $T and turbo=$turbo and function $i_func $tree failed.")
        mse = sum((x,) -> x^2, test_y .- true_y) / N
        mean = sum(test_y) / N
        stdev = sqrt(sum((x,) -> x^2, true_y .- mean) / N)
        println("Relative error: $(mse / stdev)")
    end
end

for turbo in [false, true], T in [Float16, Float32, Float64]
    turbo && T == Float16 && continue
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
    @test DynamicExpressions.EvaluateEquationModule.deg1_l1_ll0_eval(
        tree, [zero(T)]', Val(1), Val(1), operators, Val(turbo)
    )[1][1] ≈ truth

    # op(<constant>, <constant>)
    tree = Node(1, Node(; val=3.0f0), Node(; val=4.0f0))
    @test repr(tree) == "(3.0 + 4.0)"
    tree = convert(Node{T}, tree)
    truth = T(3.0f0) + T(4.0f0)
    @test DynamicExpressions.EvaluateEquationModule.deg2_l0_r0_eval(
        tree, [zero(T)]', Val(1), operators, Val(turbo)
    )[1][1] ≈ truth

    # op(op(<constant>, <constant>))
    tree = Node(1, Node(1, Node(; val=3.0f0), Node(; val=4.0f0)))
    @test repr(tree) == "cos(3.0 + 4.0)"
    tree = convert(Node{T}, tree)
    truth = cos(T(3.0f0) + T(4.0f0))
    @test DynamicExpressions.EvaluateEquationModule.deg1_l2_ll0_lr0_eval(
        tree, [zero(T)]', Val(1), Val(1), operators, Val(turbo)
    )[1][1] ≈ truth

    # Test for presence of NaNs:
    operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[cos, sin])
    x1 = Node(T; feature=1)
    tree = sin(x1 / 0.0)
    X = randn(Float32, 3, 10)
    @test isnan(tree(X)[1])
end

# And, with generic operator enum, this should be an actual error:
operators = GenericOperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[cos, sin])
x1 = Node(Float64; feature=1)
tree = sin(x1 / 0.0)
X = randn(Float32, 10);
@noinline stack = try
    tree(X)[1]
    @test false
catch e
    @test isa(e, ErrorException)
    # Check that "Failed to evaluate" is in the message:
    @test occursin("Failed to evaluate", e.msg)
    current_exceptions()
end;
@test length(stack) == 2
@test isa(stack[1].exception, DomainError)

# If a method is not defined, we should get a nothing:
X = randn(Float32, 1, 10);
@test tree(X; throw_errors=false) === nothing
# or a MethodError:
try
    tree(X; throw_errors=true)
    @test false
catch e
    @test isa(current_exceptions()[1].exception, MethodError)
end
