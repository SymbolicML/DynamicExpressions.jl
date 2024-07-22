using DynamicExpressions
using DynamicExpressions.UtilsModule: fill_similar
using Test

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
@test has_constants(sin(x1)) == false
@test has_constants(sin(Node(; val=0.0))) == true

# Has operators
@test has_operators(x1) == false
@test has_operators(x1 + 1) == true
@test has_operators(sin(x1)) == true
@test has_operators(Node(; val=0.0)) == false

# Set constants:
tree = Node(; val=0.0)
set_scalar_constants!(tree, [1.0], [Ref(tree)])
@test repr(tree) == "1.0"
tree = x1 + Node(; val=0.0) - sin(x2 - Node(; val=0.5))
@test get_scalar_constants(tree)[1] == [0.0, 0.5]
set_scalar_constants!(tree, [1.0, 2.0], [Ref(tree.l.r), Ref(tree.r.l.r)])
@test repr(tree) == "(x1 + 1.0) - sin(x2 - 2.0)"

# Ensure that fill_similar is type stable
x = randn(Float32, 3, 10)
@inferred fill_similar(0.5f0, x, axes(x, 1))
fill_similar(0.5f0, x, axes(x, 1)) == fill(0.5f0, 3)
