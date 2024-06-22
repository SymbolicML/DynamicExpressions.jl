using DynamicExpressions
using DynamicExpressions: NodeInterface
using Interfaces: Interfaces

x1 = Node{Float64}(; feature=1)
x2 = Node{Float64}(; feature=2)

operators = OperatorEnum(; binary_operators=[+, *])

tree = x1 + x2 * 3.5
graph_tree = convert(GraphNode, tree)

@test Interfaces.test(NodeInterface, Node, [tree])
@test Interfaces.test(NodeInterface, GraphNode, [graph_tree])
