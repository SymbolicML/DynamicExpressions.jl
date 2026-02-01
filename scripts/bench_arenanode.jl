using DynamicExpressions

const AN = DynamicExpressions.ArenaNodeModule

operators = OperatorEnum(1 => (sin, cos), 2 => (+, *))

# A moderately-sized tree:
x1 = Node{Float64}(; feature=1)
x2 = Node{Float64}(; feature=1)
tree = cos(sin(x1) + x1 * 3.2) + sin(x2 * 1.1 + 0.7)

atree = AN.arena_from_tree(tree)

X = randn(Float64, 1, 10_000)

println("Node tree:   ", string_tree(tree, operators))
println("Arena tree:  ", string_tree(atree, operators))
println()

# Warmup
for _ in 1:5
    eval_tree_array(tree, X, operators)
    eval_tree_array(atree, X, operators)
end

println("@allocated eval_tree_array(Node):      ", @allocated(eval_tree_array(tree, X, operators)))
println("@allocated eval_tree_array(ArenaNode): ", @allocated(eval_tree_array(atree, X, operators)))
println("@allocated copy(Node):                 ", @allocated(copy(tree)))
println("@allocated copy(ArenaNode):            ", @allocated(copy(atree)))

println("summarysize(Node):     ", Base.summarysize(tree))
println("summarysize(Arena):    ", Base.summarysize(atree.arena))
