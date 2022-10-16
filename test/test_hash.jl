using DynamicExpressions
using Test

operators = OperatorEnum(; binary_operators=(+, *, ^, /, greater), unary_operators=(cos,))
tree = Node(3, (Node(; val=3.0) * Node(1, Node("x1")))^2.0, Node(; val=-1.2))
x = hash(tree)
@test typeof(x) == UInt
