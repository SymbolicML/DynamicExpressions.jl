using DynamicExpressions
using Random
using ForwardDiff
using Test
using SpecialFunctions
include("test_params.jl")

x1 = 2.0

# Initialize functions in Base....
for unaop in [cos, exp, safe_log, safe_log2, safe_log10, safe_sqrt, relu, gamma, safe_acosh]
    for binop in [sub]
        function make_operators(; kw...)
            return OperatorEnum(;
                default_params...,
                binary_operators=(+, *, ^, /, binop),
                unary_operators=(unaop, abs),
                kw...,
            )
        end
        make_operators()

        # for unaop in 
        f_true = (x,) -> binop(abs(3.0 * unaop(x))^2.0, -1.2)

        # binop at outside:
        const_tree = Node(
            5, Node(2, Node(; val=3.0) * Node(1, Node("x1")))^2.0, Node(; val=-1.2)
        )
        const_tree_bad = Node(
            5, Node(2, Node(; val=3.0) * Node(1, Node("x1")))^2.1, Node(; val=-1.3)
        )
        n = count_nodes(const_tree)

        true_result = f_true(x1)

        result = eval(Meta.parse(string_tree(const_tree, make_operators())))

        # Test Basics
        @test n == 9
        @test n ==
            count_nodes_with_stack(const_tree, Vector{typeof(const_tree)}(undef, 100))
        @test result == true_result

        types_to_test = [Float32, Float64, BigFloat]
        if unaop == cos
            # Other unary operators produce numbers too large
            # to do meaningful tests
            types_to_test = [Float16, types_to_test...]
        end
        for T in types_to_test
            if T == Float16 || unaop == gamma
                zero_tolerance = 3e-2
            else
                zero_tolerance = 1e-6
            end

            tree = convert(Node{T}, const_tree)
            tree_bad = convert(Node{T}, const_tree_bad)

            Random.seed!(0)
            N = 100
            if unaop in [safe_log, safe_log2, safe_log10, safe_acosh, safe_sqrt]
                X = T.(rand(MersenneTwister(0), 5, N) / 3)
            else
                X = T.(randn(MersenneTwister(0), 5, N) / 3)
            end
            X = X + sign.(X) * T(0.1)
            if unaop == safe_acosh
                X = X .+ T(1.0)
            end

            y = T.(f_true.(X[1, :]))
            test_y, complete = eval_tree_array(tree, X, make_operators())
            test_y2, complete2 = differentiable_eval_tree_array(tree, X, make_operators())

            # Test Evaluation
            @test complete == true
            @test all(abs.(test_y .- y) / N .< zero_tolerance)
            @test complete2 == true
            @test all(abs.(test_y2 .- y) / N .< zero_tolerance)

            # Test gradients:
            df_true = x -> ForwardDiff.derivative(f_true, x)
            dy = T.(df_true.(X[1, :]))
            test_dy = ForwardDiff.gradient(
                _x -> sum(differentiable_eval_tree_array(tree, _x, make_operators())[1]), X
            )
            test_dy = test_dy[1, 1:end]
            @test all(abs.(test_dy .- dy) / N .< zero_tolerance)
        end
    end
end

# We also test whether we can set a node equal to another node:
operators = OperatorEnum(; default_params...)
tree = Node(Float64; feature=1)
tree2 = exp(Node(; feature=2) / 3.2) + Node(; feature=1) * 2.0

# Test printing works:
io = IOBuffer()
print(io, tree2)
s = String(take!(io))
@test s == "(exp(x2 / 3.2) + (x1 * 2.0))"

set_node!(tree, tree2)
@test tree !== tree2
@test repr(tree) == repr(tree2)

# Test that we can work with custom operators:
function op1(x, y)
    return x + y
end
function op2(x, y)
    return x^2 + 1 / ((y)^2 + 0.1)
end
function op3(x)
    return sin(x) + cos(x)
end
local operators, tree
operators = OperatorEnum(;
    default_params..., binary_operators=(op1, op2), unary_operators=(op3,)
)
@extend_operators operators
x1 = Node(; feature=1)
x2 = Node(; feature=2)
tree = op1(op2(x1, x2), op3(x1))
@test repr(tree) == "op1(op2(x1, x2), op3(x1))"
# Test evaluation:
X = randn(MersenneTwister(0), 2, 10);
@test tree(X) ≈ ((x1, x2) -> op1(op2(x1, x2), op3(x1))).(X[1, :], X[2, :])
