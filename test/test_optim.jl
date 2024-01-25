using DynamicExpressions, Optim, Zygote
using Random: Xoshiro

operators = OperatorEnum(; binary_operators=(+, -, *, /), unary_operators=(sin, cos))
x1, x2, x3 = (i -> Node(Float64; feature=i)).(1:3);

X = randn(Xoshiro(0), Float64, 3, 100);
y = @. cos(X[1, :] * 2.1 - 0.9) + X[3, :] * -0.9

original_tree = cos(x1 * 0.8 - 0.0) + 5.2 * x3
tree = copy(original_tree)

res = optimize(t -> sum(abs2, t(X, operators) .- y), tree)

# Should be unchanged by default
@test tree == original_tree
