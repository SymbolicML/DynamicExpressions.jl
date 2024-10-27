@testitem "SymbolicUtils conversion" begin
    using DynamicExpressions, Test
    import DynamicExpressions.StringsModule: strip_brackets
    import SymbolicUtils: simplify, Symbolic
    import Base: ≈

    strip_brackets(a::String) = String(strip_brackets(collect(a)))
    function Base.:≈(a::String, b::String)
        a = strip_brackets(a)
        b = strip_brackets(b)
        a = replace(a, r"\s+" => "")
        b = replace(b, r"\s+" => "")
        return a == b
    end

    operators = OperatorEnum(; binary_operators=(+, -, /, *))
    tree = Node("x1") + Node("x1")

    # Should simplify to 2*x1:
    eqn = convert(Symbolic, tree, operators)
    eqn2 = simplify(eqn)
    @test occursin("2", "$(repr(eqn2)[1])")

    # Let's convert back the simplified version.
    tree = convert(Node, eqn2, operators)
    @test (tree.l.constant ? tree.l : tree.r).val == 2
    @test (!tree.l.constant ? tree.l : tree.r).feature == 1

    # Test that SymbolicUtils does not convert multiplication to power:
    tree = Node("x1") * Node("x1")
    eqn = convert(Symbolic, tree, operators)
    @test repr(eqn) ≈ "x1*x1"
    tree_copy = convert(Node, eqn, operators)
    @test repr(tree_copy) ≈ "(x1*x1)"
end

@testitem "Complex expression simplification" begin
    using DynamicExpressions, Test
    using SymbolicUtils: simplify, Symbolic
    import Random: MersenneTwister
    include("test_params.jl")

    x1, x2, x3 = Node("x1"), Node("x2"), Node("x3")
    pow_abs2(x, y) = abs(x)^y

    operators = OperatorEnum(;
        binary_operators=(+, *, -, /, pow_abs2), unary_operators=(custom_cos, exp, sin)
    )
    @extend_operators operators
    tree = (
        ((x2 + x2) * ((-0.5982493 / pow_abs2(x1, x2)) / -0.54734415)) + (
            sin(
                custom_cos(
                    sin(1.2926733 - 1.6606787) /
                    sin(((0.14577048 * x1) + ((0.111149654 + x1) - -0.8298334)) - -1.2071426),
                ) * (custom_cos(x3 - 2.3201916) + ((x1 - (x1 * x2)) / x2)),
            ) / (0.14854191 - ((custom_cos(x2) * -1.6047639) - 0.023943262))
        )
    )
    
    eqn = convert(Symbolic, tree, operators; index_functions=true)
    tree_copy = convert(Node, eqn, operators)
    tree_copy2 = convert(Node, simplify(eqn), operators)

    N = 100
    X = rand(MersenneTwister(0), 3, N) .+ 0.1
    output1, flag1 = eval_tree_array(tree, X, operators)
    output2, flag2 = eval_tree_array(tree_copy, X, operators)
    output3, flag3 = eval_tree_array(tree_copy2, X, operators)

    @test isapprox(output1, output2, atol=1e-4 * sqrt(N))
    @test isapprox(output1, output3, atol=1e-2 * sqrt(N))
end

@testitem "Constant folding" begin
    using DynamicExpressions, Test

    operators = OperatorEnum(; binary_operators=(+, -, *, /), unary_operators=(cos, sin))
    x1, x2, x3 = [Node(; feature=i) for i in 1:3]

    # Unary operator applied to constant => constant:
    tree = Node(1, Node(; val=0.0))
    @test repr(tree) ≈ "cos(0.0)"
    @test repr(simplify_tree!(tree, operators)) ≈ "1.0"

    # NaN handling
    tree = Node(1, Node(; val=NaN))
    @test repr(tree) ≈ "cos(NaN)"
    @test repr(simplify_tree!(tree, operators)) ≈ "cos(NaN)"

    # Nested constant folding
    tree = Node(1, Node(1, Node(; val=0.1), Node(; val=0.2)) + Node(; val=0.2)) + Node(; val=2.0)
    @test repr(tree) ≈ "(cos((0.1 + 0.2) + 0.2) + 2.0)"
    @test repr(combine_operators(tree, operators)) ≈ "(cos(0.4 + 0.1) + 2.0)"
end

# (const - (const - var)) => (var - const)
tree = Node(2, Node(; val=0.5), Node(; val=0.2) - x1)
@test repr(tree) ≈ "(0.5 - (0.2 - x1))"
@test repr(combine_operators(tree, operators)) ≈ "(x1 - -0.3)"

# ((const - var) - const) => (const - var)
tree = Node(2, Node(; val=0.5) - x1, Node(; val=0.2))
@test repr(tree) ≈ "((0.5 - x1) - 0.2)"
@test repr(combine_operators(tree, operators)) ≈ "(0.3 - x1)"

# (const - (var - const)) => (const - var)
tree = Node(2, Node(; val=0.5), x1 - Node(; val=0.2))
@test repr(tree) ≈ "(0.5 - (x1 - 0.2))"
@test repr(combine_operators(tree, operators)) ≈ "(0.7 - x1)"

# ((var - const) - const) => (var - const)
tree = ((x1 - 0.2) - 0.6)
@test repr(tree) ≈ "((x1 - 0.2) - 0.6)"
@test repr(combine_operators(tree, operators)) ≈ "(x1 - 0.8)"
###############################################################################
