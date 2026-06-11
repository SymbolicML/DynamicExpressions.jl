using Test
using DynamicExpressions

const AN = DynamicExpressions.ArenaNodeModule

operators = OperatorEnum(1 => (sin, cos), 2 => (+, *))
x1 = DynamicExpressions.Node{Float64}(; feature=1)

function alloc_push_constant!(arena)
    AN.push_constant!(arena, 1.0)
    return nothing
end

function alloc_set_child!(parent, child)
    set_child!(parent, child, 1)
    return nothing
end

function alloc_copy_tree!(arena, tree)
    AN._copy_to_arena!(arena, tree)
    return nothing
end

function alloc_eval_tree(tree, X, operators)
    eval_tree_array(tree, X, operators)
    return nothing
end

arena_push = AN.Arena{Float64,2}(; capacity=128)

base_tree = sin(x1)
parent_arena = AN.Arena{Float64,2}(; capacity=128)
parent_idx = AN._copy_to_arena!(parent_arena, base_tree)
parent = AN.ArenaNode(parent_arena, parent_idx)

child_tree = x1 * 3.2
child_arena = AN.Arena{Float64,2}(; capacity=128)
child_idx = AN._copy_to_arena!(child_arena, child_tree)
child = AN.ArenaNode(child_arena, child_idx)

tree_large = sin(x1) + x1 * 3.2 + cos(x1)
atree_large = AN.arena_from_tree(tree_large)
arena_large = AN.Arena{Float64,2}(; capacity=128)
X = randn(Float64, 1, 1_000)

for _ in 1:5
    alloc_push_constant!(arena_push)
    alloc_set_child!(parent, child)
    alloc_copy_tree!(arena_large, tree_large)
    alloc_eval_tree(tree_large, X, operators)
    alloc_eval_tree(atree_large, X, operators)
end

alloc_counts = Dict(
    "push_constant" => @allocations(alloc_push_constant!(arena_push)),
    "set_child" => @allocations(alloc_set_child!(parent, child)),
    "copy_tree" => @allocations(alloc_copy_tree!(arena_large, tree_large)),
)
alloc_bytes = Dict(
    "eval_node" => @allocated(alloc_eval_tree(tree_large, X, operators)),
    "eval_arena" => @allocated(alloc_eval_tree(atree_large, X, operators)),
)

@test alloc_counts["push_constant"] == 0
@test alloc_counts["set_child"] == 0
@test alloc_counts["copy_tree"] == 0
@test alloc_bytes["eval_arena"] <= max(1024, ceil(Int, 1.10 * alloc_bytes["eval_node"]))
