using Test
using DynamicExpressions

include("test_params.jl")

## Test Base.print
operators = OperatorEnum(;
    default_params..., binary_operators=(+, *, /, -), unary_operators=(cos, sin)
)

f = (x1, x2, x3) -> (sin(cos(sin(cos(x1) * x3) * 3.0) * -0.5) + 2.0) * 5.0

tree = f(Node("x1"), Node("x2"), Node("x3"))

s = repr(tree)
true_s = "((sin(cos(sin(cos(x1) * x3) * 3.0) * -0.5) + 2.0) * 5.0)"

@test s == true_s

# TODO: Next, we test that custom varMaps work:

s = string_tree(tree, operators; varMap=["v1", "v2", "v3"])
true_s = "((sin(cos(sin(cos(v1) * v3) * 3.0) * -0.5) + 2.0) * 5.0)"
@test s == true_s

for unaop in [safe_log, safe_log2, safe_log10, safe_log1p, safe_sqrt, safe_acosh]
    opts = OperatorEnum(;
        default_params..., binary_operators=(+, *, /, -), unary_operators=(unaop,)
    )
    minitree = Node(1, Node("x1"))
    @test string_tree(minitree, opts) == replace(string(unaop), "safe_" => "") * "(x1)"
end

for binop in [safe_pow, ^]
    opts = OperatorEnum(;
        default_params..., binary_operators=(+, *, /, -, binop), unary_operators=(cos,)
    )
    minitree = Node(5, Node("x1"), Node("x2"))
    @test string_tree(minitree, opts) == "(x1 ^ x2)"
end
