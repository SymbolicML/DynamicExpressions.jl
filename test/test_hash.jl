using DynamicExpressions
using Test

function greater(x::T, y::T) where {T}
    return (x > y) ? one(T) : zero(T)
end

operators = OperatorEnum(; binary_operators=(+, *, ^, /, greater), unary_operators=(cos,))
tree = NNode(3, (NNode(; val=3.0) * NNode(1, NNode("x1")))^2.0, NNode(; val=-1.2))
x = hash(tree)
@test typeof(x) == UInt
