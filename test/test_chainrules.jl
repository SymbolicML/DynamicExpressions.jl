using Test
using DynamicExpressions
using Random: MersenneTwister
using ForwardDiff: gradient as fd_gradient
using Zygote: gradient as zg_gradient
using Suppressor: @suppress_err
include("test_params.jl")
include("tree_gen_utils.jl")

let
    rng = MersenneTwister(0)
    n_features = 5
    operators = OperatorEnum(; binary_operators=(+, *, -), unary_operators=(sin,))
    tree = gen_random_tree_fixed_size(20, operators, n_features, Float64, Node, rng)
    X = rand(rng, Float64, n_features, 100)

    function f(X)
        y, _ = eval_tree_array(tree, X, operators)
        return sum(i -> y[i]^2, eachindex(y))
    end

    @suppress_err begin
        # Check zg_gradient against fd_gradient; the latter of which is computed explicitly
        @test isapprox([only(zg_gradient(f, X))...], [fd_gradient(f, X)...]; atol=1e-6)
    end
end

mean(x) = sum(x) / length(x)

let
    operators = OperatorEnum(; binary_operators=(+, *, -), unary_operators=(sin,))
    x1, x2, x3 = [Node{Float64}(; feature=i) for i in 1:3]
    tree = sin(x1 * 3.2 - 0.9) + 0.2 * x2 - x3
    X = [
        1.0 2.0 3.0
        4.0 5.0 6.0
        7.0 8.0 9.0
    ]
    function eval_tree(X, tree)
        y, _ = eval_tree_array(tree, X, operators)
        return mean(y)
    end

    function true_eval_tree(X, c)
        y = @. sin(X[1, :] * c[1] - c[2]) + c[3] * X[2, :] - X[3, :]
        return mean(y)
    end

    evaluated_gradient = zg_gradient(tree -> eval_tree(X, tree), tree)[1]
    true_gradient = fd_gradient(c -> true_eval_tree(X, c), [3.2, 0.9, 0.2])

    @test evaluated_gradient.tree == tree
    @test isapprox(evaluated_gradient.gradient, true_gradient)
end
