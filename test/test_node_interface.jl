@testitem "Node interface" begin
    using DynamicExpressions
    using DynamicExpressions: NodeInterface
    using Interfaces: Interfaces

    x1 = Node{Float64}(; feature=1)
    x2 = Node{Float64}(; feature=2)

    operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])

    tree_branch_deg2 = x1 + sin(x2 * 3.5)
    tree_branch_deg1 = sin(x1)
    tree_leaf_feature = x1
    tree_leaf_constant = Node{Float64}(; val=1.0)
    graph_tree_branch_deg2 = convert(GraphNode, tree_branch_deg2)
    graph_tree_branch_deg1 = convert(GraphNode, tree_branch_deg1)
    graph_tree_leaf_feature = convert(GraphNode, tree_leaf_feature)
    graph_tree_leaf_constant = convert(GraphNode, tree_leaf_constant)

    @test Interfaces.test(
        NodeInterface,
        Node,
        [tree_branch_deg2, tree_branch_deg1, tree_leaf_feature, tree_leaf_constant],
    )
    @test Interfaces.test(
        NodeInterface,
        GraphNode,
        [
            graph_tree_branch_deg2,
            graph_tree_branch_deg1,
            graph_tree_leaf_feature,
            graph_tree_leaf_constant,
        ],
    )
end

@testitem "Node interface on n-arity nodes" begin
    using DynamicExpressions
    using DynamicExpressions: NodeInterface
    using Interfaces: Interfaces

    for D in (3, 4, 5)
        x = [Node{Float64,D}(; feature=i) for i in 1:3]
        operator_tuple = ((sin, cos, exp), (+, *, /, -), (fma, clamp), (max, min), ())
        operators = OperatorEnum(operator_tuple[1:D])
        DynamicExpressions.OperatorEnumConstructionModule.empty_all_globals!()
        let tree = Node{Float64,D}(; op=2, children=(x[1], x[2]))  # *
            if D > 2
                fma_idx = 1
                tree = Node{Float64,D}(; op=fma_idx, children=(tree, x[1], x[2]))  # fma
            end
            if D > 3
                idx_max = 1
                tree = Node{Float64,D}(; op=idx_max, children=(tree, x[1], x[2], x[3]))  # max
            end
            @test Interfaces.test(NodeInterface, Node, [tree])
        end
    end
end
