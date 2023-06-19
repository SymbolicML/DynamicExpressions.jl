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

s = string_tree(tree, operators; variable_names=["v1", "v2", "v3"])
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

@testset "Test printing of complex numbers" begin
    @eval my_custom_op(x, y) = x + y
    operators = OperatorEnum(;
        default_params...,
        binary_operators=(+, *, /, -, my_custom_op),
        unary_operators=(cos, sin),
    )
    @extend_operators operators
    x1, x2, x3 = [Node(; feature=i) for i in 1:3]
    tree = sin(x1 * 1.0)
    @test string_tree(tree, operators) == "sin(x1 * 1.0)"
    tree = sin(x1 * (1.0 + 2.0im))
    @test string_tree(tree, operators) == "sin(x1 * (1.0 + 2.0im))"
    tree = my_custom_op(x1, 1.0 + 2.0im)
    @test string_tree(tree, operators) == "my_custom_op(x1, 1.0 + 2.0im)"
end
