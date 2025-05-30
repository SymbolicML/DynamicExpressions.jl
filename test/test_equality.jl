using DynamicExpressions
using Test

operators = OperatorEnum(;
    binary_operators=[+, *, -, /], unary_operators=[sin, cos, exp, log]
)

# Create a big expression, using those operators:
x1 = NNode(; feature=1)
x2 = NNode(; feature=2)
x3 = NNode(; feature=3)

tree = x1 + x2 * x3 - log(x2 * 3.2) + 1.5 * cos(x2 / x1)
same_tree = x1 + x2 * x3 - log(x2 * 3.2) + 1.5 * cos(x2 / x1)
@test tree == same_tree

x1 = GraphNNode(; feature=1)
x2 = GraphNNode(; feature=2)
x3 = GraphNNode(; feature=3)
tree = x1 + x2 * x3 - log(x2 * 3.2) + 1.5 * cos(x2 / x1)
copied_tree = copy_node(tree)
@test tree == copied_tree

copied_tree2 = copy_node(tree; break_sharing=Val(true))
@test tree != copied_tree2

# Another way to break shared nodes is by converting
# to `NNode` and back:
copied_tree3 = GraphNNode(NNode(tree))
@test copied_tree2 == copied_tree3
@test tree != copied_tree3

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

f64_tree = GraphNNode{Float64}(x1 + x2 * x3 - log(x2 * 3.0) + 1.5 * cos(x2 / x1))
f32_tree = GraphNNode{Float32}(x1 + x2 * x3 - log(x2 * 3.0) + 1.5 * cos(x2 / x1))
@test typeof(f64_tree) <: GraphNNode{Float64}
@test typeof(f32_tree) <: GraphNNode{Float32}

@test convert(GraphNNode{Float64}, f32_tree) == f64_tree

@test f64_tree == f32_tree

@test NNode(f64_tree) == NNode(f32_tree)
