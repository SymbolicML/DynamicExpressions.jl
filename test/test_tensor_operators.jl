using DynamicExpressions
using Test

baseT = Float64
T = Union{baseT,Vector{baseT},Matrix{baseT}}

function vec_add(x, y)
    return x .+ y
end

operators = GenericOperatorEnum(; binary_operators=[vec_add])

x1, x2, x3 = [Node(T; feature=i) for i in 1:3]
c1 = Node(T; val=[1.0, 2.0, 3.0])

X = [[2.0, 2.0, 2.0], [3.0, 3.0, 3.0]]

tree = Node(1, c1, x2)
@test repr(tree) == "vec_add([1.0, 2.0, 3.0], x2)"
@test tree(X, operators) == [4.0, 5.0, 6.0]
tree = Node(1, x1, c1)
@test repr(tree) == "vec_add(x1, [1.0, 2.0, 3.0])"
@test tree(X, operators) == [3.0, 4.0, 5.0]

# Try same things, but with constructors:
@extend_operators operators
tree = vec_add(c1, x2)
@test repr(tree) == "vec_add([1.0, 2.0, 3.0], x2)"
@test tree(X, operators) == [4.0, 5.0, 6.0]
tree = vec_add(x1, c1)
@test repr(tree) == "vec_add(x1, [1.0, 2.0, 3.0])"
@test tree(X, operators) == [3.0, 4.0, 5.0]

# Also test unary operators:
function vec_square(x)
    return x .* x
end

operators = GenericOperatorEnum(; binary_operators=[vec_add], unary_operators=[vec_square])
@extend_operators operators
tree = Node(1, c1)
@test repr(tree) == "vec_square([1.0, 2.0, 3.0])"
@test tree(X, operators) == [1.0, 4.0, 9.0]
@test vec_square(c1).val == [1.0, 4.0, 9.0]
tree = Node(1, Node(1, c1), x1)
@test repr(tree) == "vec_add(vec_square([1.0, 2.0, 3.0]), x1)"
@test tree(X, operators) == [3.0, 6.0, 11.0]
@test (vec_add(vec_square(c1), x1))(X) == [3.0, 6.0, 11.0]

# Also test mixed scalar and floats:
c2 = Node(T; val=2.0)
@test repr(c2) == "2.0"
tree = Node(1, Node(1, c1, x1), c2)
@test repr(tree) == "vec_add(vec_add([1.0, 2.0, 3.0], x1), 2.0)"
tree(X, operators)
@test tree(X, operators) == [5.0, 6.0, 7.0]
