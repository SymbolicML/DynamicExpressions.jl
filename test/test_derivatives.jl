using Test
using DynamicExpressions
using DynamicExpressions: eval_diff_tree_array, eval_grad_tree_array
using Random
using Zygote
using LinearAlgebra
include("test_params.jl")

seed = 0
# SIMD doesn't like abs(x) ^ y for some reason.
pow_abs2(x, y) = exp(y * log(abs(x)))

equation1(x1, x2, x3) = x1 + x2 + x3 + 3.2
equation2(x1, x2, x3) = pow_abs2(x1, x2) + x3 + custom_cos(1.0 + x3) + 3.0 / x1
function equation3(x1, x2, x3)
    return (
        ((x2 + x2) * ((-0.5982493 / pow_abs2(x1, x2)) / -0.54734415)) + (
            sin(
                custom_cos(
                    sin(1.2926733 - 1.6606787) / sin(
                        ((0.14577048 * x1) + ((0.111149654 + x1) - -0.8298334)) - -1.2071426
                    ),
                ) * (custom_cos(x3 - 2.3201916) + ((x1 - (x1 * x2)) / x2)),
            ) / (0.14854191 - ((custom_cos(x2) * -1.6047639) - 0.023943262))
        )
    )
end

nx1 = Node("x1")
nx2 = Node("x2")
nx3 = Node("x3")

# Equations to test gradients on:

function array_test(ar1, ar2; rtol=0.1)
    return isapprox(ar1, ar2; rtol=rtol)
end

for type in [Float16, Float32, Float64], turbo in [true, false]
    type == Float16 && turbo && continue

    println(
        "Testing derivatives with respect to variables, with type=$(type) and turbo=$(turbo).",
    )
    rng = MersenneTwister(seed)
    nfeatures = 3
    N = 100

    local X, operators
    X = rand(rng, type, nfeatures, N) * 5

    operators = OperatorEnum(;
        binary_operators=(+, *, -, /, pow_abs2),
        unary_operators=(custom_cos, exp, sin),
        enable_autodiff=true,
    )
    @extend_operators operators

    for j in 1:3
        equation = [equation1, equation2, equation3][j]
        if type == Float16 && j == 3
            # Numerical precision hurts this comparison too much
            continue
        end

        local tree
        tree = convert(Node{type}, equation(nx1, nx2, nx3))
        predicted_output = eval_tree_array(tree, X, operators)[1]
        true_output = equation.([X[i, :] for i in 1:nfeatures]...)
        true_output = convert(AbstractArray{type}, true_output)

        # First, check if the predictions are approximately equal:
        @test array_test(predicted_output, true_output)

        true_grad = gradient(
            (x1, x2, x3) -> sum(equation.(x1, x2, x3)), [X[i, :] for i in 1:nfeatures]...
        )
        # Convert tuple of vectors to matrix:
        true_grad = reduce(hcat, true_grad)'
        predicted_grad = eval_grad_tree_array(
            tree, X, operators; variable=true, turbo=turbo
        )[2]
        predicted_grad2 =
            reduce(
                hcat,
                [
                    eval_diff_tree_array(tree, X, operators, i; turbo=turbo)[2] for
                    i in 1:nfeatures
                ],
            )'
        predicted_grad3 = tree'(X, operators; turbo=turbo)
        # Test deprecated syntax:
        predicted_grad4 = tree'(X; turbo=turbo)

        # Print largest difference between predicted_grad, true_grad:
        @test array_test(predicted_grad, true_grad)
        @test array_test(predicted_grad2, true_grad)
        @test array_test(predicted_grad3, true_grad)
        @test array_test(predicted_grad4, true_grad)

        # Make sure that the array_test actually works:
        @test !array_test(predicted_grad .* 0, true_grad)
        @test !array_test(predicted_grad2 .* 0, true_grad)
        @test !array_test(predicted_grad3 .* 0, true_grad)
    end
    println("Done.")
    println("Testing derivatives with respect to constants, with type=$(type).")

    # Test gradient with respect to constants:
    equation4(x1, x2, x3) = 3.2f0 * x1
    # The gradient should be: (C * x1) => x1 is gradient with respect to C.
    local tree
    tree = equation4(nx1, nx2, nx3)
    tree = convert(Node{type}, tree)
    predicted_grad = eval_grad_tree_array(tree, X, operators; variable=false, turbo=turbo)[2]
    @test array_test(predicted_grad[1, :], X[1, :])

    # More complex expression:
    const_value = 2.1f0
    const_value2 = -3.2f0

    function equation5(x1, x2, x3)
        return pow_abs2(x1, x2) + x3 + custom_cos(const_value + x3) + const_value2 / x1
    end
    function equation5_with_const(c1, c2, x1, x2, x3)
        return pow_abs2(x1, x2) + x3 + custom_cos(c1 + x3) + c2 / x1
    end

    tree = equation5(nx1, nx2, nx3)
    tree = convert(Node{type}, tree)

    # Use zygote to explicitly find the gradient:
    true_grad = gradient(
        (c1, c2, x1, x2, x3) -> sum(equation5_with_const.(c1, c2, x1, x2, x3)),
        fill(const_value, N),
        fill(const_value2, N),
        [X[i, :] for i in 1:nfeatures]...,
    )[1:2]
    true_grad = reduce(hcat, true_grad)'
    predicted_grad = eval_grad_tree_array(tree, X, operators; variable=false, turbo=turbo)[2]

    @test array_test(predicted_grad, true_grad)
    println("Done.")
end

println("Testing NodeIndex.")

import DynamicExpressions: get_constants, NodeIndex, index_constants

operators = OperatorEnum(;
    binary_operators=(+, *, -, /, pow_abs2),
    unary_operators=(custom_cos, exp, sin),
    enable_autodiff=true,
)
@extend_operators operators
tree = equation3(nx1, nx2, nx3)

"""Check whether the ordering of constant_list is the same as the ordering of node_index."""
function check_tree(tree::Node, node_index::NodeIndex, constant_list::AbstractVector)
    if tree.degree == 0
        (!tree.constant) || tree.val == constant_list[node_index.constant_index]
    elseif tree.degree == 1
        check_tree(tree.l, node_index.l, constant_list)
    else
        check_tree(tree.l, node_index.l, constant_list) &&
            check_tree(tree.r, node_index.r, constant_list)
    end
end

@test check_tree(tree, index_constants(tree), get_constants(tree))

println("Done.")
