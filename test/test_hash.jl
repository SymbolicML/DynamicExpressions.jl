using DynamicExpressions
using Test

function greater(x::T, y::T) where T
    (x > y) ? one(T) : zero(T)
end

operators = OperatorEnum(; binary_operators=(+, *, ^, /, greater), unary_operators=(cos,))
tree = Node(3, (Node(; val=3.0) * Node(1, Node("x1")))^2.0, Node(; val=-1.2))
x = hash(tree)
@test typeof(x) == UInt
