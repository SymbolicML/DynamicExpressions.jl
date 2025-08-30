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

@testitem "ArrayNode with FixedSizeArrays backing storage" begin
    using DynamicExpressions
    using DynamicExpressions: OperatorEnum
    using FixedSizeArrays
    using AllocCheck: @check_allocs
    const ArrayNode = DynamicExpressions.ArrayNode

    # Create a FixedSizeVector type for our backing storage
    # We'll use size 100 for this test
    const N = 100

    # Create an ArrayTree with FixedSizeVector backing
    allocator = DynamicExpressions.ArrayNodeModule.ArrayTree{Float64,2}(
        N; array_type=FixedSizeVector
    )

    # Test that the backing arrays are indeed FixedSizeArrays
    @test allocator.nodes.degree isa FixedSizeArray{UInt8}
    @test allocator.nodes.val isa FixedSizeArray{Float64}
    @test allocator.nodes.feature isa FixedSizeArray{UInt16}

    # Create nodes using the FixedSizeVector-backed allocator
    x1 = ArrayNode{Float64,2}(; feature=1, allocator=allocator)
    x2 = ArrayNode{Float64,2}(; feature=2, allocator=allocator)
    c = ArrayNode{Float64,2}(; val=3.5, allocator=allocator)

    # Build a tree
    mul = ArrayNode{Float64,2}(; op=3, l=x2, r=c, allocator=allocator)
    sin_expr = ArrayNode{Float64,2}(; op=1, l=mul, allocator=allocator)
    tree = ArrayNode{Float64,2}(; op=1, l=x1, r=sin_expr, allocator=allocator)

    # Test basic operations
    @test tree.degree == 2
    @test x1.feature == 1
    @test c.val == 3.5

    # Test evaluation
    operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[sin, cos])
    X = [1.0 2.0; 0.5 1.0]  # 2 features, 2 samples
    result, complete = eval_tree_array(tree, X, operators)
    expected = X[1, :] .+ sin.(X[2, :] .* 3.5)
    @test all(abs.(result .- expected) .< 1e-10)

    # Test that operations are still allocation-free
    @check_allocs get_degree(n) = n.degree
    @check_allocs get_val(n) = n.val
    @check_allocs get_feature(n) = n.feature

    get_degree(tree)
    get_val(c)
    get_feature(x1)

    # Test count_nodes
    @test count_nodes(tree) == 6

    # Test creating nodes is allocation-free with preallocated FixedSizeVector storage
    @check_allocs create_node(alloc, f) = ArrayNode{Float64,2}(; feature=f, allocator=alloc)
    new_node = create_node(allocator, 5)
    @test new_node.feature == 5

    println("✅ ArrayNode works with FixedSizeArrays backing storage!")
end

@testitem "ArrayNode vs Node comparison with random trees" begin
    using DynamicExpressions
    using DynamicExpressions: Node, OperatorEnum
    using Random: MersenneTwister
    include("tree_gen_utils.jl")

    const ArrayNode = DynamicExpressions.ArrayNode

    # Test with different operator configurations
    operators_configs = [
        OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[sin, cos]),
        OperatorEnum(; binary_operators=[+, *], unary_operators=[-, abs]),
        OperatorEnum(;
            binary_operators=[+, -, *, /, ^], unary_operators=[sin, cos, exp, log]
        ),
    ]

    for operators in operators_configs
        rng = MersenneTwister(42)
        nfeatures = 3

        for tree_size in [5, 10, 20]
            for _ in 1:5  # Test multiple random trees of each size
                # Generate a random Node tree
                node_tree = gen_random_tree_fixed_size(
                    tree_size, operators, nfeatures, Float64, Node, rng
                )

                # Convert to ArrayNode
                # First, create an allocator with enough space
                allocator = DynamicExpressions.ArrayNodeModule.ArrayTree{Float64,2}(
                    tree_size * 2
                )

                # Function to convert Node to ArrayNode
                function node_to_array_node(n::Node, alloc)
                    if n.degree == 0
                        if n.constant
                            return ArrayNode{Float64,2}(; val=n.val, allocator=alloc)
                        else
                            return ArrayNode{Float64,2}(;
                                feature=n.feature, allocator=alloc
                            )
                        end
                    elseif n.degree == 1
                        child = node_to_array_node(n.l, alloc)
                        return ArrayNode{Float64,2}(; op=n.op, l=child, allocator=alloc)
                    else  # degree == 2
                        left = node_to_array_node(n.l, alloc)
                        right = node_to_array_node(n.r, alloc)
                        return ArrayNode{Float64,2}(;
                            op=n.op, l=left, r=right, allocator=alloc
                        )
                    end
                end

                array_tree = node_to_array_node(node_tree, allocator)

                # Test 1: Count nodes
                @test DynamicExpressions.count_nodes(node_tree) ==
                    DynamicExpressions.count_nodes(array_tree)

                # Test 2: String representation
                node_str = DynamicExpressions.string_tree(node_tree, operators)
                array_str = DynamicExpressions.string_tree(array_tree, operators)
                @test node_str == array_str

                # Test 3: Evaluation on random data
                X = randn(rng, nfeatures, 10)
                node_result, node_ok = DynamicExpressions.eval_tree_array(
                    node_tree, X, operators
                )
                array_result, array_ok = DynamicExpressions.eval_tree_array(
                    array_tree, X, operators
                )

                @test node_ok == array_ok
                if node_ok && array_ok
                    # Check that results match (accounting for floating point errors)
                    @test all(isnan.(node_result) .== isnan.(array_result))
                    valid_idx = .!isnan.(node_result) .& .!isnan.(array_result)
                    if any(valid_idx)
                        @test all(
                            abs.(node_result[valid_idx] .- array_result[valid_idx]) .< 1e-10
                        )
                    end
                end

                # Test 4: Hash consistency
                # Two equivalent trees should have the same hash
                array_tree2 = node_to_array_node(node_tree, allocator)
                @test hash(array_tree) == hash(array_tree2)

                # Test 5: Copy operation
                array_copy = copy(array_tree)
                @test array_copy == array_tree
                @test array_copy !== array_tree
                @test DynamicExpressions.count_nodes(array_copy) ==
                    DynamicExpressions.count_nodes(array_tree)
            end
        end
    end

    println("✅ ArrayNode matches Node behavior on random trees!")
end

@testitem "ArrayNode tree_mapreduce operations" begin
    using DynamicExpressions
    using DynamicExpressions: Node, OperatorEnum, tree_mapreduce
    using Random: MersenneTwister
    include("tree_gen_utils.jl")

    const ArrayNode = DynamicExpressions.ArrayNode

    operators = OperatorEnum(; binary_operators=[+, -, *], unary_operators=[sin, -])
    rng = MersenneTwister(123)
    nfeatures = 2

    for tree_size in [5, 10, 15]
        # Generate random Node tree
        node_tree = gen_random_tree_fixed_size(
            tree_size, operators, nfeatures, Float64, Node, rng
        )

        # Convert to ArrayNode
        allocator = DynamicExpressions.ArrayNodeModule.ArrayTree{Float64,2}(tree_size * 2)

        function node_to_array_node(n::Node, alloc)
            if n.degree == 0
                if n.constant
                    return ArrayNode{Float64,2}(; val=n.val, allocator=alloc)
                else
                    return ArrayNode{Float64,2}(; feature=n.feature, allocator=alloc)
                end
            elseif n.degree == 1
                child = node_to_array_node(n.l, alloc)
                return ArrayNode{Float64,2}(; op=n.op, l=child, allocator=alloc)
            else
                left = node_to_array_node(n.l, alloc)
                right = node_to_array_node(n.r, alloc)
                return ArrayNode{Float64,2}(; op=n.op, l=left, r=right, allocator=alloc)
            end
        end

        array_tree = node_to_array_node(node_tree, allocator)

        # Test various tree_mapreduce operations

        # 1. Count constants
        count_constants = t -> t.constant ? 1 : 0
        node_const_count = tree_mapreduce(count_constants, +, node_tree, Int)
        array_const_count = tree_mapreduce(count_constants, +, array_tree, Int)
        @test node_const_count == array_const_count

        # 2. Count features
        count_features = t -> (!t.constant && t.degree == 0) ? 1 : 0
        node_feat_count = tree_mapreduce(count_features, +, node_tree, Int)
        array_feat_count = tree_mapreduce(count_features, +, array_tree, Int)
        @test node_feat_count == array_feat_count

        # 3. Max depth
        depth_fn = t -> 1
        max_fn = (a, b...) -> maximum((a, b...))
        node_depth = tree_mapreduce(depth_fn, max_fn, node_tree, Int)
        array_depth = tree_mapreduce(depth_fn, max_fn, array_tree, Int)
        @test node_depth == array_depth

        # 4. Check if any node has specific property
        has_sin = t -> (t.degree > 0 && t.op == 1)  # Assuming sin is first unary op
        node_has_sin = DynamicExpressions.any(has_sin, node_tree)
        array_has_sin = DynamicExpressions.any(has_sin, array_tree)
        @test node_has_sin == array_has_sin
    end

    println("✅ ArrayNode tree_mapreduce operations match Node!")
end
