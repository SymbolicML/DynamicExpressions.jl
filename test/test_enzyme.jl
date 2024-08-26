# TODO: This test is broken again due to segfault
using Test
using Enzyme
using DynamicExpressions

operators = OperatorEnum(; binary_operators=(+, -, *, /), unary_operators=(cos, sin))
# TODO: More operators will trigger a segfault in Enzyme

x1, x2, x3 = (i -> Node(Float64; feature=i)).(1:3)

tree = Node(1, x1, Node(1, x2))  # == x1 + cos(x2)

X = randn(3, 100);
dX = zero(X)

function f(tree, X, operators, output)
    output[] = sum(eval_tree_array(tree, X, operators)[1])
    return nothing
end

output = [0.0]
doutput = [1.0]

fetch(
    schedule(
        Task(64 * 1024^2) do
            autodiff(
                Reverse,
                f,
                Const(tree),
                Duplicated(X, dX),
                Const(operators),
                Duplicated(output, doutput),
            )
        end,
    ),
)

true_dX = cat(ones(100), -sin.(X[2, :]), zeros(100); dims=2)'

@test true_dX ≈ dX

function my_loss_function(tree, X, operators)
    # Get the outputs
    y, _ = eval_tree_array(tree, X, operators)
    # Sum them (so we can take a gradient, rather than a jacobian)
    return sum(y)
end
tree = 0.5 * x1 + cos(x2 - 0.2)

# Just to keep things simple:
X = [1.0; 1.0;;]

d_tree = begin
    storage_tree = copy(tree)
    # Set all constants to zero:
    foreach(storage_tree) do node
        if node.degree == 0 && node.constant
            node.val = 0.0
        end
    end
    fetch(
        schedule(
            Task(64 * 1024^2) do
                autodiff(
                    Reverse,
                    my_loss_function,
                    Active,
                    Duplicated(tree, storage_tree),
                    Const(X),
                    Const(operators),
                )
            end,
        ),
    )
    storage_tree
end

@test isapprox(first(get_scalar_constants(d_tree)), [1.0, 0.717356]; atol=1e-3)
