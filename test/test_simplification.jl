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
                    sin(1.2926733 - 1.6606787) / sin(
                        ((0.14577048 * x1) + ((0.111149654 + x1) - -0.8298334)) - -1.2071426
                    ),
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
    tree =
        Node(1, Node(1, Node(; val=0.1), Node(; val=0.2)) + Node(; val=0.2)) +
        Node(; val=2.0)
    @test repr(tree) ≈ "(cos((0.1 + 0.2) + 0.2) + 2.0)"
    @test repr(combine_operators!(tree, operators)) ≈ "(cos(0.4 + 0.1) + 2.0)"
end

@testitem "Basic operator simplifications" begin
    using DynamicExpressions, Test
    import DynamicExpressions.SimplifyModule: combine_operators!

    operators = OperatorEnum(; binary_operators=(+, -, *, /), unary_operators=(cos, sin))
    x = Node(; feature=1)
    zero_node = Node(; val=0.0)
    one_node = Node(; val=1.0)
    two_node = Node(; val=2.0)
    three_node = Node(; val=3.0)

    # multiplication by 0
    tree = zero_node * x
    @test combine_operators!(tree, operators) == zero_node
    tree = x * zero_node
    @test combine_operators!(tree, operators) == zero_node

    # multiplication by 1
    tree = one_node * x
    @test combine_operators!(tree, operators) == x
    tree = x * one_node
    @test combine_operators!(tree, operators) == x

    # addition by 0
    tree = zero_node + x
    @test combine_operators!(tree, operators) == x
    tree = x + zero_node
    @test combine_operators!(tree, operators) == x

    # division by self -> 1
    tree = x / x
    @test combine_operators!(tree, operators).val == 1.0

    # nested multiplication by constants
    tree1 = (two_node * x) * three_node
    tree2 = Node(; val=6.0) * x
    @test combine_operators!(tree1, operators) == combine_operators!(tree2, operators)
end

@testitem "Constant combination" begin
    using DynamicExpressions, Test
    import DynamicExpressions.SimplifyModule: combine_operators!

    operators = OperatorEnum(; binary_operators=(+, -, *, /), unary_operators=(cos, sin))
    x1 = Node(; feature=1)

    # Test commutative constant combination
    tree = Node(; val=0.5) + (Node(; val=0.2) + x1)
    @test combine_operators!(tree, operators) == x1 + Node(; val=0.7)

    # Test nested multiplication by constants
    tree = (Node(; val=2.0) * x1) * Node(; val=3.0)
    @test combine_operators!(tree, operators) == x1 * Node(; val=6.0)

    # Test nested addition by constants
    tree = (Node(; val=2.0) + x1) + Node(; val=3.0)
    @test combine_operators!(tree, operators) == x1 + Node(; val=5.0)

    # Test mixed operations don't combine incorrectly
    tree = (Node(; val=2.0) * x1) + Node(; val=3.0)
    @test combine_operators!(tree, operators) == tree
end

@testitem "Random tree simplification" begin
    using DynamicExpressions, Test
    import DynamicExpressions.SimplifyModule: combine_operators!, simplify_tree!
    import Random: MersenneTwister
    include("tree_gen_utils.jl")

    operators = OperatorEnum(; binary_operators=(+, -, *, /), unary_operators=(sin, cos))

    initial_sizes = Int[]
    simplified_sizes = Int[]

    for i in 1:100
        rng = MersenneTwister(i)

        # Generate a random tree with 3 features and size ~50 nodes
        tree = gen_random_tree_fixed_size(50, operators, 3, Float64, Node, rng)

        # Randomly set some nodes to 0 or 1
        if rand(rng) < 0.5
            any(tree) do node
                if node.degree == 0 && node.constant && rand(rng) < 0.5
                    node.val = rand(rng) < 0.5 ? 0.0 : 1.0
                    true
                else
                    false
                end
            end
        end

        # Simplify it
        simplified = combine_operators!(copy(tree), operators)

        # Simplified tree should not be larger than original
        push!(initial_sizes, count_nodes(tree))
        push!(simplified_sizes, count_nodes(simplified))

        # Evaluate both trees on the same output
        X = randn(rng, Float64, 3, 10)
        output1, flag1 = eval_tree_array(tree, X, operators)
        output2, flag2 = eval_tree_array(simplified, X, operators)

        # Both should succeed or fail together
        @test flag1 == flag2

        if flag1 && flag2
            # Results should be approximately equal
            @test isapprox(output1, output2, rtol=1e-10)
        end
    end
    # At least SOME should simplify
    @test any(i -> initial_sizes[i] > simplified_sizes[i], 1:length(initial_sizes))
end
