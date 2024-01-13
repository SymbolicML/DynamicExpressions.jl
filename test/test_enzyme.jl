using Test
using Enzyme
using DynamicExpressions

operators = OperatorEnum(; binary_operators=(+, -, *, /), unary_operators=(cos, sin))
# TODO: More operators will trigger a segfault in Enzyme

# These options are required for Enzyme to work:
const eval_options = (turbo=Val(false),)

x1, x2, x3 = (i -> Node(Float64; feature=i)).(1:3)

tree = Node(1, x1, Node(1, x2))  # == x1 + cos(x2)

X = randn(3, 100);
dX = zero(X)

function f(tree, X, operators, output)
    output[] = sum(eval_tree_array(tree, X, operators; eval_options...)[1])
    return nothing
end

output = [0.0]
doutput = [1.0]

autodiff(
    Reverse,
    f,
    Const(tree),
    Duplicated(X, dX),
    Const(operators),
    Duplicated(output, doutput),
)

true_dX = cat(ones(100), -sin.(X[2, :]), zeros(100); dims=2)'

@test true_dX ≈ dX
