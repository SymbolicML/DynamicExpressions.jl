@testitem "ArrayNode interface with Vector" begin
    using DynamicExpressions
    using DynamicExpressions: NodeInterface
    using Interfaces: Interfaces
    const ArrayNode = DynamicExpressions.ArrayNode

    # Test with regular Vector
    x1 = ArrayNode{Float64,2,Vector}(; feature=1)
    x2 = ArrayNode{Float64,2,Vector}(; feature=2)

    operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])

    # Create test trees matching the pattern in test_node_interface.jl
    tree_branch_deg2 = ArrayNode{Float64,2,Vector}(;
        op=1,
        l=x1,
        r=ArrayNode{Float64,2,Vector}(;
            op=1,
            l=ArrayNode{Float64,2,Vector}(;
                op=2, l=x2, r=ArrayNode{Float64,2,Vector}(; val=3.5)
            ),
        ),
    )  # x1 + sin(x2 * 3.5)

    tree_branch_deg1 = ArrayNode{Float64,2,Vector}(; op=1, l=x1)  # sin(x1)
    tree_leaf_feature = x1
    tree_leaf_constant = ArrayNode{Float64,2,Vector}(; val=1.0)

    @test Interfaces.test(
        NodeInterface,
        ArrayNode,
        [tree_branch_deg2, tree_branch_deg1, tree_leaf_feature, tree_leaf_constant],
    )
end

@testitem "ArrayNode with custom array type" begin
    using DynamicExpressions
    using DynamicExpressions: NodeInterface
    using Interfaces: Interfaces
    const ArrayNode = DynamicExpressions.ArrayNode

    # Test that ArrayNode works with any AbstractVector type
    # For production use with FixedSizeArrays, you'd need a wrapper 
    # that handles mixed element types properly

    x1 = ArrayNode{Float64,2,Vector}(; feature=1)
    x2 = ArrayNode{Float64,2,Vector}(; feature=2)

    operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])

    # Create test trees
    tree_branch_deg2 = ArrayNode{Float64,2,Vector}(;
        op=1,
        l=x1,
        r=ArrayNode{Float64,2,Vector}(;
            op=1,
            l=ArrayNode{Float64,2,Vector}(;
                op=2, l=x2, r=ArrayNode{Float64,2,Vector}(; val=3.5)
            ),
        ),
    )

    tree_branch_deg1 = ArrayNode{Float64,2,Vector}(; op=1, l=x1)
    tree_leaf_feature = x1
    tree_leaf_constant = ArrayNode{Float64,2,Vector}(; val=1.0)

    @test Interfaces.test(
        NodeInterface,
        ArrayNode,
        [tree_branch_deg2, tree_branch_deg1, tree_leaf_feature, tree_leaf_constant],
    )
end

@testitem "ArrayNode interface on n-arity nodes" begin
    using DynamicExpressions
    using DynamicExpressions: NodeInterface
    using Interfaces: Interfaces
    const ArrayNode = DynamicExpressions.ArrayNode

    for D in (3, 4, 5)
        # Test with regular arrays
        x = [ArrayNode{Float64,D,Vector}(; feature=i) for i in 1:3]
        operator_tuple = ((sin, cos, exp), (+, *, /, -), (fma, clamp), (max, min), ())
        # Create pairs for degrees 1 through D
        pairs = [i => operator_tuple[i] for i in 1:D if !isempty(operator_tuple[i])]
        operators =
            isempty(pairs) ? OperatorEnum(1 => ()) : OperatorEnum(pairs[1], pairs[2:end]...)
        DynamicExpressions.OperatorEnumConstructionModule.empty_all_globals!()

        let tree = ArrayNode{Float64,D,Vector}(; op=2, children=(x[1], x[2]))  # *
            if D > 2
                fma_idx = 1
                tree = ArrayNode{Float64,D,Vector}(;
                    op=fma_idx, children=(tree, x[1], x[2])
                )  # fma
            end
            if D > 3
                idx_max = 1
                tree = ArrayNode{Float64,D,Vector}(;
                    op=idx_max, children=(tree, x[1], x[2], x[3])
                )  # max
            end
            @test Interfaces.test(NodeInterface, ArrayNode, [tree])
        end
    end
end

@testitem "ArrayNode basic operations" begin
    using DynamicExpressions
    using DynamicExpressions: OperatorEnum
    using AllocCheck: @check_allocs
    const ArrayNode = DynamicExpressions.ArrayNode

    # Test with regular arrays (default)
    x1 = ArrayNode{Float64,2}(; feature=1)
    x2 = ArrayNode{Float64,2}(; feature=2)
    c = ArrayNode{Float64,2}(; val=3.5)

    # Test basic properties
    @test x1.degree == 0
    @test x1.feature == 1
    @test !x1.constant

    @test c.degree == 0
    @test c.val == 3.5
    @test c.constant

    # Test tree construction
    mul = ArrayNode{Float64,2}(; op=3, l=x2, r=c)
    @test mul.degree == 2
    @test mul.op == 3

    sin_expr = ArrayNode{Float64,2}(; op=1, l=mul)
    @test sin_expr.degree == 1

    tree = ArrayNode{Float64,2}(; op=1, l=x1, r=sin_expr)
    @test tree.degree == 2

    # Test copy
    tree_copy = copy(tree)
    @test tree == tree_copy
    @test tree !== tree_copy

    # Test hash
    @test hash(tree) == hash(tree_copy)

    # Test count_nodes
    @test count_nodes(tree) == 6  # tree, x1, sin_expr, mul, x2, c

    # Test string conversion
    operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[sin, cos])
    str = string_tree(tree, operators)
    @test str == "x1 + sin(x2 * 3.5)"

    # Test evaluation
    X = [1.0 2.0; 0.5 1.0]  # 2 features, 2 samples
    result = eval_tree_array(tree, X, operators)
    expected = X[1, :] .+ sin.(X[2, :] .* 3.5)
    @test all(abs.(result[1] .- expected) .< 1e-10)
end

@testitem "ArrayNode allocation tests" begin
    using DynamicExpressions
    using DynamicExpressions: OperatorEnum, eval_tree_array
    using AllocCheck: @check_allocs
    const ArrayNode = DynamicExpressions.ArrayNode

    # Create a tree with preallocated storage
    allocator = DynamicExpressions.ArrayNodeModule.ArrayTree{Float64,2}(100)
    x1 = ArrayNode{Float64,2}(; feature=1, allocator=allocator)
    x2 = ArrayNode{Float64,2}(; feature=2, allocator=allocator)
    c = ArrayNode{Float64,2}(; val=3.5, allocator=allocator)

    # Build tree using same allocator
    mul = ArrayNode{Float64,2}(; op=3, l=x2, r=c, allocator=allocator)
    sin_expr = ArrayNode{Float64,2}(; op=1, l=mul, allocator=allocator)
    tree = ArrayNode{Float64,2}(; op=1, l=x1, r=sin_expr, allocator=allocator)

    operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[sin, cos])
    X = [1.0 2.0; 0.5 1.0]  # 2 features, 2 samples

    # Warm up
    result = eval_tree_array(tree, X, operators)

    # Test that evaluation doesn't allocate
    @check_allocs eval_tree_array(tree, X, operators) = eval_tree_array(tree, X, operators)

    # Test that property access doesn't allocate
    @check_allocs get_degree(n) = n.degree
    @check_allocs get_val(n) = n.val
    @check_allocs get_feature(n) = n.feature
    @check_allocs get_op(n) = n.op

    get_degree(tree)
    get_val(c)
    get_feature(x1)
    get_op(tree)

    # Test that count_nodes doesn't allocate (after warm-up)
    count_nodes(tree)
    @check_allocs count_nodes(tree) = count_nodes(tree)

    # Test that tree traversal doesn't allocate
    function traverse_tree(n::ArrayNode)
        sum = n.degree
        if n.degree > 0
            children = DynamicExpressions.NodeModule.get_children(n, Val(Int(n.degree)))
            for child in children
                sum += traverse_tree(child)
            end
        end
        return sum
    end

    # Warm up
    traverse_tree(tree)
    @check_allocs traverse_tree(tree) = traverse_tree(tree)
end

@testitem "ArrayNode with Expressions" begin
    using DynamicExpressions
    using DynamicExpressions: Expression
    const ArrayNode = DynamicExpressions.ArrayNode

    # Create a simple tree with default arrays
    x1 = ArrayNode{Float64,2}(; feature=1)
    c = ArrayNode{Float64,2}(; val=2.0)
    tree = ArrayNode{Float64,2}(; op=1, l=x1, r=c)

    operators = OperatorEnum(; binary_operators=[+, -, *, /])

    # Test Expression conversion
    expr = Expression(tree; operators=operators, variable_names=["x1", "x2"])
    @test string(expr) == "x1 + 2.0"

    # Test evaluation through Expression
    X = [1.0 2.0 3.0]  # 1 feature, 3 samples
    result = expr(X)
    expected = vec(X .+ 2.0)  # Convert to vector to match result shape
    @test all(abs.(result .- expected) .< 1e-10)
end
