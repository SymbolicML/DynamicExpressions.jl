#! format: off
@testitem "Test validity of expression evaluation" begin
using DynamicExpressions, Bumper, LoopVectorization, Random

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

            @inferred eval_tree_array(tree, X, operators; turbo, bumper)

            test_y = eval_tree_array(tree, X, operators; turbo, bumper)[1]

            zero_tolerance = (T <: Union{Float16,Complex} ? 1e-4 : 1e-6)
            @test all(abs.(test_y .- true_y) / N .< zero_tolerance)

            test_y_helper = tree(X, operators; turbo, bumper)
            @test all(test_y .== test_y_helper)
        end
    end
end
end
#! format: on

@testitem "Test specific branches of evaluation" begin
    using DynamicExpressions, DynamicExpressions, Bumper, LoopVectorization
    using DynamicExpressions.EvaluateModule: EvalOptions

    include("test_params.jl")

    for turbo in [Val(false), Val(true)],
        T in [Float16, Float32, Float64, ComplexF32, ComplexF64]

        turbo isa Val{true} && !(T in (Float32, Float64)) && continue
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
        @test DynamicExpressions.EvaluateModule.deg1_l1_ll0_eval(tree, [zero(T)]', cos, cos, EvalOptions(; turbo)).x[1] ≈
            truth

        # op(<constant>, <constant>)
        tree = Node(1, Node(; val=3.0f0), Node(; val=4.0f0))
        @test repr(tree) == "3.0 + 4.0"
        tree = convert(Node{T}, tree)
        truth = T(3.0f0) + T(4.0f0)
        @test DynamicExpressions.EvaluateModule.deg2_l0_r0_eval(tree, [zero(T)]', (+), EvalOptions(; turbo)).x[1] ≈
            truth

        # op(op(<constant>, <constant>))
        tree = Node(1, Node(1, Node(; val=3.0f0), Node(; val=4.0f0)))
        @test repr(tree) == "cos(3.0 + 4.0)"
        tree = convert(Node{T}, tree)
        truth = cos(T(3.0f0) + T(4.0f0))
        @test DynamicExpressions.EvaluateModule.deg1_l2_ll0_lr0_eval(tree, [zero(T)]', cos, (+), EvalOptions(; turbo)).x[1] ≈
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
@testitem "Test error catching for GenericOperatorEnum" begin
    using DynamicExpressions

    # And, with generic operator enum, this should be an actual error:
    @eval my_fnc(x::Real) = x
    operators = GenericOperatorEnum(;
        binary_operators=[+, -, *, /], unary_operators=[cos, sin, my_fnc]
    )
    @extend_operators operators
    x1 = Node(Float64; feature=1)
    tree = sin(x1 / 0.0)
    X = randn(Float32, 10)
    let
        try
            tree(X, operators)[1]
            @test false
        catch e
            @test e isa ErrorException
            # Check that "Failed to evaluate" is in the message:
            @test occursin("Failed to evaluate", e.msg)
            stack = current_exceptions()
            @test length(stack) == 2
            @test stack[1].exception isa DomainError
        end

        # If a method is not defined, we should get a nothing:
        X2 = randn(ComplexF64, 1, 10)
        tree2 = my_fnc(x1)
        @test tree2(X2, operators; throw_errors=false) === nothing
        # or a MethodError:
        try
            tree2(X2, operators; throw_errors=true)
            @test false
        catch e
            @test e isa ErrorException
            @test occursin("Failed to evaluate", e.msg)
            stack2 = current_exceptions()
            @test length(stack2) == 2
            @test stack2[1].exception isa MethodError
        end
    end
end

@testitem "Test many operators" begin
    using DynamicExpressions

    include("tree_gen_utils.jl")

    # Since we use `@nif` in evaluating expressions,
    # we can see if there are any issues with LARGE numbers of operators.
    num_ops = 100
    binary_operators = [@eval function (x, y)
        return x + y
    end for i in 1:num_ops]
    unary_operators = [@eval function (x)
        return x^2
    end for i in 1:num_ops]
    operators = @test_logs(
        (:warn, r"You have passed over 15 degree2.*"), OperatorEnum(2 => binary_operators)
    )
    operators = @test_logs(
        (:warn, r"You have passed over 15 degree1.*"),
        OperatorEnum(1 => unary_operators, 2 => binary_operators)
    )
    tree = Node(;
        op=1,
        children=(Node(; op=num_ops ÷ 2, children=(Node(; val=3.0), Node(; feature=2))),),
    )
    # = (3.0 + x2)^2
    X = randn(Float64, 2, 10)
    truth = @. (3.0 + X[2, :])^2
    @test truth ≈ tree(X, operators)

    @test_logs(
        (:warn, r"You have passed over 15 degree1.*"), OperatorEnum(1 => unary_operators)
    )

    # This OperatorEnum will trigger the fallback code for fast compilation.
    many_ops_operators = OperatorEnum(
        1 => [[sin, cos]; unary_operators], 2 => [[+, -, *, /]; binary_operators]
    )

    # This OperatorEnum will go through the regular evaluation code.
    only_basic_ops_operator = OperatorEnum(1 => [sin, cos], 2 => [+, -, *, /])

    # We want to compare them:
    num_tests = 100
    n_features = 3
    for _ in 1:num_tests
        let tree = gen_random_tree_fixed_size(
                20, only_basic_ops_operator, n_features, Float64
            ),
            X = randn(Float64, n_features, 10),
            basic_eval = tree(X, only_basic_ops_operator),
            many_ops_eval = tree(X, many_ops_operators)

            @test (all(isnan, basic_eval) && all(isnan, many_ops_eval)) ||
                basic_eval ≈ many_ops_eval
        end
    end
end

@testitem "Disable early exit" begin
    using DynamicExpressions
    using Bumper, LoopVectorization

    let
        T = Float16
        ex = @parse_expression(
            2 * x, binary_operators = [*], variable_names = ["x"], node_type = Node{T}
        )
        X = T[1.0 floatmax(T)]
        @test all(isnan.(ex(X)))
        @test ex(X; eval_options=EvalOptions(; early_exit=Val(false))) ≈ [2.0, Inf]
    end

    for turbo in [Val(false), Val(true)],
        T in [Float32, Float64],
        bumper in [Val(false), Val(true)]

        ex = @parse_expression(
            (-b - sqrt(b^2 - (4 * a) * c)) / (2 * c),
            binary_operators = [-, *, /, ^],
            unary_operators = [-, sqrt],
            variable_names = ["a", "b", "c"],
            node_type = Node{T}
        )
        X = T[
            -1 -1
            1 floatmax(T)
            1 1
        ]
        @test all(isnan.(ex(X; eval_options=EvalOptions(; bumper, turbo))))
        y = ex(X; eval_options=EvalOptions(; bumper, turbo, early_exit=Val(false)))
        @test y[1] ≈ T(-1.618033988749895)
        @test !isfinite(y[2])
    end
end

@testitem "Test EvalOptions constructor" begin
    using DynamicExpressions, LoopVectorization

    @test EvalOptions(; turbo=true) isa EvalOptions{true}
    @test EvalOptions(; turbo=Val(true)) isa EvalOptions{true}
    @test EvalOptions(; turbo=false) isa EvalOptions{false}
    @test EvalOptions(; turbo=Val(false)) isa EvalOptions{false}

    ex = Expression(Node{Float64}(; feature=1))
    @test_throws ArgumentError ex(randn(1, 5), OperatorEnum(); bad_arg=1)
end

@testitem "Test EvalOptions copy" begin
    using DynamicExpressions
    using DynamicExpressions: ArrayBuffer

    # Test copying with various configurations
    buffer = zeros(5, 10)
    buffer_ref = Ref(0)
    original = EvalOptions(;
        turbo=true, bumper=false, early_exit=true, buffer=ArrayBuffer(buffer, buffer_ref)
    )
    copied = copy(original)

    # Test that all fields are equal
    @test copied.turbo == original.turbo
    @test copied.bumper == original.bumper
    @test copied.early_exit == original.early_exit
    @test copied.buffer.array == original.buffer.array
    @test copied.buffer.index[] == original.buffer.index[]

    # Test that buffer is copied, not referenced
    @test copied.buffer !== original.buffer
    @test copied.buffer.array !== original.buffer.array
    @test copied.buffer.index !== original.buffer.index

    # Test without buffer
    original_no_buffer = EvalOptions(; turbo=true, bumper=false, early_exit=true)
    copied_no_buffer = copy(original_no_buffer)
    @test copied_no_buffer.buffer === nothing
end

@testitem "Test Inf val" begin
    using DynamicExpressions

    include("tree_gen_utils.jl")

    # Even if the `.val` is `Inf`, we should
    # be able to safely evaluate the expression.
    some_unsafe_operators = OperatorEnum(;
        binary_operators=[+, -, *, /], unary_operators=[sin, cos]
    )

    function fill_vals!(tree, val)
        foreach(tree) do node
            if node.degree == 0 && node.constant
                node.val = val
            end
        end
    end

    # We want to compare them:
    num_tests = 100
    n_features = 3
    for _ in 1:num_tests
        let tree, X, eval
            tree = gen_random_tree_fixed_size(
                20, some_unsafe_operators, n_features, Float64
            )
            # Now, we set all the constant values to Inf:
            X = zeros(Float64, n_features, 10)
            # Smoke test – we just want to make sure we can evaluate the expression!
            eval_tree_array(tree, X, some_unsafe_operators)
            fill_vals!(tree, Inf)
            eval_tree_array(tree, X, some_unsafe_operators)
            fill_vals!(tree, NaN)
        end
    end
end

@testitem "Evaluate vector dispatch & numeric promotion" begin
    using DynamicExpressions
    using DynamicExpressions.OperatorEnumModule: OperatorEnum
    using Test

    ops = OperatorEnum(1 => (), 2 => (+,))
    vars = ["x₁", "x₂"]
    x1, x2 = (
        Expression(Node(Float64; feature=i); operators=ops, variable_names=vars) for
        i in 1:2
    )

    expr = x1 + x2

    # Matrix{Int} with one column (2 features × 1 sample) satisfies the validator
    X = reshape([1, 2], 2, 1)
    out = expr(X)
    @test out[1] == 3

    # Matrix{Int} promotion path is already exercised above
end
