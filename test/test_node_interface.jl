using DynamicExpressions
using DynamicExpressions: NodeInterface
using Interfaces: test

x1 = Node{Float64}(; feature=1)
x2 = Node{Float64}(; feature=2)

operators = OperatorEnum(; binary_operators=[+, *])

tree = x1 + x2 * 3.5
graph_tree = convert(GraphNode, tree)

@test test(NodeInterface, Node, [tree])
@test test(NodeInterface, GraphNode, [graph_tree])