using DynamicExpressions
using Test
using Zygote

operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[cos, sin])
x1, x2 = Node{Float64}(; feature=1), Node{Float64}(; feature=2)
tree = cos(2.1 * x1)

# Also test warnings:
for constructor in (OperatorEnum, GenericOperatorEnum)
    VERSION >= v"1.9" &&
        @test_logs (:warn, r"The `tree\(X; kws...\)` syntax is deprecated.*") tree(
            [1.0; 2.0;;]
        )

    constructor == GenericOperatorEnum && continue

    VERSION >= v"1.9" &&
        @test_logs (:warn, r"The `tree'\(X; kws...\)` syntax is deprecated.*") tree'(
            [1.0; 2.0;;]
        )
end

if VERSION >= v"1.9"
    @test_logs (:warn, r"Node\(d, c, v\) is deprecated.*") (
        n = Node(1, true, 1.0 + 0im); @assert (n.val isa ComplexF64)
    )
    @test_logs (:warn, r"Node\(T, d, c, v\) is deprecated.*") (
        n = Node(Float32, 1, true, 1.0 + 0im); @assert (n.val isa Float32)
    )
    @test_logs (:warn, r"Node\(T, d, c, v, f\) is deprecated.*") (
        n = Node(Float32, 1, false, nothing, 1); @assert (n.feature == 1)
    )
    @test_logs (:warn, r"Node\(d, c, v, f, o, l\) is deprecated.*") (
        x1 = Node(; feature=1);
        n = Node(1, true, nothing, 1, 3, x1);
        @assert (n.op == 3 && n.l === x1)
    )
    @test_logs (:warn, r"Node\(d, c, v, f, o, l, r\) is deprecated.*") (
        x1 = Node(; feature=1);
        x2 = Node(; feature=2);
        n = Node(2, true, nothing, 1, 1, x1, x2);
        @assert (n.op == 1 && n.l === x1 && n.r === x2)
    )
end
