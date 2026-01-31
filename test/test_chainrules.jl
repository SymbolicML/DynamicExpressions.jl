using Test
using DynamicExpressions
using Random: MersenneTwister
using ChainRulesCore: ChainRulesCore, ZeroTangent, NoTangent
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

    # Misc tests of uncovered portions
    let tree = tree,
        X = X,
        evaluated_gradient = evaluated_gradient,
        true_gradient = true_gradient

        evaluated_gradient_2 = zg_gradient(tree -> eval_tree(X, tree), tree)[1]
        true_gradient_2 = fd_gradient(c -> true_eval_tree(X, c), [3.2, 0.9, 0.2])

        evaluated_aggregate = evaluated_gradient + evaluated_gradient_2
        true_aggregate = true_gradient + true_gradient_2
        @test evaluated_aggregate.tree == tree
        @test isapprox(evaluated_aggregate.gradient, true_aggregate)

        scalar_prod = evaluated_gradient * 2.0
        scalar_prod2 = 2.0 * (1.0 * evaluated_gradient)
        true_scalar_prod = true_gradient * 2.0
        @test scalar_prod.tree == tree
        @test isapprox(scalar_prod.gradient, true_scalar_prod)
        @test isapprox(scalar_prod2.gradient, true_scalar_prod)

        # Should be able to use with other types
        @test zero(evaluated_gradient) == ZeroTangent()

        @test evaluated_gradient + ZeroTangent() == evaluated_gradient
        @test evaluated_gradient + NoTangent() == evaluated_gradient
    end
end

# Operator that is NaN for forward pass
# Define only for numeric types; `@extend_operators` adds the `Node` method.
bad_op(x::Real) = x > 0.0 ? log(x) : convert(typeof(x), NaN)
# And operator that is undefined for backward pass
undefined_grad_op(x::Real) = x >= 0.0 ? x : zero(x)
# And operator that gives a NaN for backward pass
bad_grad_op(x) = x

function ChainRulesCore.rrule(::typeof(bad_grad_op), x)
    return bad_grad_op(x), (_) -> (NoTangent(), convert(typeof(x), NaN))
end

# Also test NaN modes
let
    operators = OperatorEnum(;
        binary_operators=(+, *, -),
        unary_operators=(sin, bad_op, bad_grad_op, undefined_grad_op),
    )
    @extend_operators operators
    x1 = Node(Float64; feature=1)

    nan_forward = bad_op(x1 + 0.5)
    undefined_grad = undefined_grad_op(x1 + 0.5)
    nan_grad = bad_grad_op(x1)

    function eval_tree(X, tree)
        y, _ = eval_tree_array(tree, X, operators)
        return mean(y)
    end
    X = ones(1, 1) * -1.0

    # Forward pass is NaN; Gradient will also be NaN
    @test isnan(only(eval_tree(X, nan_forward)))
    evaluated_gradient = zg_gradient(X -> eval_tree(X, nan_forward), X)[1]
    @test isnan(only(evaluated_gradient))

    # Both forward and gradient are not NaN despite giving `nothing` back
    @test !isnan(only(eval_tree(X, undefined_grad)))
    evaluated_gradient = zg_gradient(X -> eval_tree(X, undefined_grad), X)[1]
    @test iszero(only(evaluated_gradient))

    # Finally, the operator with a NaN gradient but non-NaN forward
    @test !isnan(only(eval_tree(X, nan_grad)))
    evaluated_gradient = zg_gradient(X -> eval_tree(X, nan_grad), X)[1]
    @test isnan(only(evaluated_gradient))
    evaluated_gradient = zg_gradient(t -> eval_tree(X, t), nan_grad)[1]
    @show evaluated_gradient
    # @test isnan(only(evaluated_gradient.gradient))
end
