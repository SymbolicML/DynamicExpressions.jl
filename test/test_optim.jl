using DynamicExpressions, Optim, Zygote
using Random: Xoshiro

operators = OperatorEnum(; binary_operators=(+, -, *, /), unary_operators=(exp,))
x1, x2, x3 = (i -> Node(Float64; feature=i)).(1:3);

X = rand(Xoshiro(0), Float64, 3, 100);
y = @. exp(X[1, :] * 2.1 - 0.9) + X[3, :] * -0.9

original_tree = exp(x1 * 0.8 - 0.0) + 5.2 * x3
target_tree = exp(x1 * 2.1 - 0.9) + -0.9 * x3
tree = copy(original_tree)

res = optimize(t -> sum(abs2, t(X, operators) .- y), tree)

# Should be unchanged by default
if VERSION >= v"1.9"
    ext = Base.get_extension(DynamicExpressions, :DynamicExpressionsOptimExt)
    @test res isa ext.ExpressionOptimizationResults
end
@test tree == original_tree
@test get_constants(target_tree) ≈ get_constants(target_tree)
