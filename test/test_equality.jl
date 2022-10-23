using DynamicExpressions
using Test

operators = OperatorEnum(;
    binary_operators=[+, *, -, /], unary_operators=[sin, cos, exp, log]
)

# Create a big expression, using those operators:
x1 = Node(; feature=1)
x2 = Node(; feature=2)
x3 = Node(; feature=3)

tree = x1 + x2 * x3 - log(x2 * 3.2) + 1.5 * cos(x2 / x1)
same_tree = x1 + x2 * x3 - log(x2 * 3.2) + 1.5 * cos(x2 / x1)
@test tree == same_tree

copied_tree = copy_node(tree; preserve_topology=true)
@test tree == copied_tree

copied_tree2 = copy_node(tree; preserve_topology=false)
@test tree == copied_tree2

modifed_tree = x1 + x2 * x1 - log(x2 * 3.2) + 1.5 * cos(x2 / x1)
@test tree != modifed_tree
modifed_tree2 = x1 + x2 * x3 - log(x2 * 3.1) + 1.5 * cos(x2 / x1)
@test tree != modifed_tree2
modifed_tree3 = x1 + x2 * x3 - exp(x2 * 3.2) + 1.5 * cos(x2 / x1)
@test tree != modifed_tree3
modified_tree4 = x1 + x2 * x3 - log(x2 * 3.2) + 1.5 * cos(x2 * x1)
@test tree != modified_tree4

# Order matters!
modified_tree5 = 1.5 * cos(x2 * x1) + x1 + x2 * x3 - log(x2 * 3.2)
@test tree != modified_tree5

# Type should not matter if equivalent in the promoted type:
f64_tree = x1 + x2 * x3 - log(x2 * 3.0) + 1.5 * cos(x2 / x1)
f32_tree = x1 + x2 * x3 - log(x2 * 3.0f0) + 1.5f0 * cos(x2 / x1)
@test typeof(f64_tree) == Node{Float64}
@test typeof(f32_tree) == Node{Float32}

@test convert(Node{Float64}, f32_tree) == f64_tree

@test f64_tree == f32_tree
