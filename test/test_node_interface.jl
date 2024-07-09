@testitem "Node interface" begin
    using DynamicExpressions
    using DynamicExpressions: NodeInterface
    using Interfaces: Interfaces

    x1 = Node{Float64}(; feature=1)
    x2 = Node{Float64}(; feature=2)

    operators = OperatorEnum(; binary_operators=[+, *])

    tree_branch = x1 + x2 * 3.5
    tree_leaf = x1
    graph_tree_branch = convert(GraphNode, tree_branch)
    graph_tree_leaf = convert(GraphNode, tree_leaf)

    @test Interfaces.test(NodeInterface, Node, [tree_branch, tree_leaf])
    @test Interfaces.test(NodeInterface, GraphNode, [graph_tree_branch, graph_tree_leaf])
end
