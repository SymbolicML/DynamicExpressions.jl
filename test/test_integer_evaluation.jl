using DynamicExpressions
using Random
using Test
include("test_params.jl")

# Test evaluation on integer-based trees.
operators = OperatorEnum(;
    default_params..., binary_operators=(+, *, /, -, ^), unary_operators=()
)

nodefnc(x1, x2, x3) = x2 * x3 + Int32(2) - x1^Int32(2)

xs = [Node(Int32; feature=i) for i in 1:3]
tree = nodefnc(xs...)

tree = convert(Node{Int32}, tree)
X = Int32.(rand(MersenneTwister(0), -5:5, 3, 100))

true_out = nodefnc.(X[1, :], X[2, :], X[3, :])
@test eltype(true_out) == Int32
out, flag = eval_tree_array(tree, X, operators)
@test flag
@test isapprox(out, true_out)
@test eltype(out) == Int32
