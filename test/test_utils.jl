using DynamicExpressions

operators = OperatorEnum(; binary_operators=(+, *, -, /, ^), unary_operators=(exp, sin))

x1, x2, x3 = Node("x1"), Node("x2"), Node("x3")

# Depth:

@test count_depth(x1) == 1
@test count_depth(x1 + x1) == 2
@test count_depth((x1 + x1) + (x1 + x1)) == 3
@test count_depth((x1 + x1) + (x1 + x1) + (x1 + x1)) == 4
@test count_depth(x1 + (x1 + (x1 + (x1 + (x1 + x1))))) == 6
@test count_depth(exp(exp(exp(x2)))) == 4

# Has constants:
@test has_constants(x1) == false
@test has_constants(x1 + 1) == true
@test has_constants(cos(x1)) == false
@test has_constants(cos(Node(; val=0.0))) == true

# Has operators
@test has_operators(x1) == false
@test has_operators(x1 + 1) == true
@test has_operators(cos(x1)) == true
@test has_operators(Node(; val=0.0)) == false

# Set constants:
tree = Node(; val=0.0)
set_constants!(tree, [1.0])
@test repr(tree) == "1.0"
tree = x1 + Node(; val=0.0) - sin(x2 - Node(; val=0.5))
@test get_constants(tree) == [0.0, 0.5]
set_constants!(tree, [1.0, 2.0])
@test repr(tree) == "((x1 + 1.0) - sin(x2 - 2.0))"
