using DynamicExpressions
using Random: MersenneTwister as RNG

const operators = OperatorEnum(; binary_operators=(+, -, *, /), unary_operators=(exp,))
x1, x2 = (i -> Node(Float64; feature=i)).(1:2)

const X = rand(RNG(0), Float64, 2, 100)
const y = @. exp(X[1, :] * 2.1 - 0.9) + X[2, :] * -0.9

original_tree = exp(x1 * 0.8 - 0.0) + 5.2 * x2
target_tree = exp(x1 * 2.1 - 0.9) + -0.9 * x2

function f(tree)
    out = tree(X, operators)
    return sum(i -> abs2(out[i] - y[i]), eachindex(out, y))
end
function g!(G, tree)
    dy = only(gradient(f, tree))
    G .= dy.gradient
    return nothing
end
