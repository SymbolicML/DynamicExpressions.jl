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
