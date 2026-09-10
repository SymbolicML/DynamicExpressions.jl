using Test
using DynamicExpressions
using DynamicExpressions.NodePreallocationModule: allocate_container, copy_into!

using DynamicExpressions: ArenaNode, Arena
using DynamicExpressions.ArenaNodeModule: _copy_to_arena!, push_constant!

operators = OperatorEnum(1 => (sin, cos), 2 => (+, *))
x1 = DynamicExpressions.Node{Float64}(; feature=1)

function alloc_push_constant!(arena)
    push_constant!(arena, 1.0)
    return nothing
end

function alloc_set_child!(parent, child)
    set_child!(parent, child, 1)
    return nothing
end

function alloc_copy_tree!(arena, tree)
    _copy_to_arena!(arena, tree)
    return nothing
end

function alloc_copy_into!(dest, tree)
    copy_into!(dest, tree)
    return nothing
end

arena_push = Arena{Float64,2}(; capacity=128)

base_tree = sin(x1)
parent_arena = Arena{Float64,2}(; capacity=128)
parent_idx = _copy_to_arena!(parent_arena, base_tree)
parent = ArenaNode(parent_arena, parent_idx)

child_tree = x1 * 3.2
child_arena = Arena{Float64,2}(; capacity=128)
child_idx = _copy_to_arena!(child_arena, child_tree)
child = ArenaNode(child_arena, child_idx)

tree_large = sin(x1) + x1 * 3.2 + cos(x1)
atree_large = convert(ArenaNode{Float64}, tree_large)
copy_dest = allocate_container(atree_large)
arena_large = Arena{Float64,2}(; capacity=128)

for _ in 1:5
    alloc_push_constant!(arena_push)
    alloc_set_child!(parent, child)
    alloc_copy_tree!(arena_large, tree_large)
    alloc_copy_into!(copy_dest, atree_large)
end

arena_push_nodes = arena_push.nodes
arena_push_ptr = pointer(arena_push_nodes)
arena_push_len = length(arena_push_nodes)
alloc_push_constant!(arena_push)

parent_nodes = parent_arena.nodes
parent_ptr = pointer(parent_nodes)
parent_len = length(parent_nodes)
alloc_set_child!(parent, child)

arena_large_nodes = arena_large.nodes
arena_large_ptr = pointer(arena_large_nodes)
arena_large_len = length(arena_large_nodes)
alloc_copy_tree!(arena_large, tree_large)

copy_dest_nodes = copy_dest.nodes
copy_dest_ptr = pointer(copy_dest_nodes)
alloc_copy_into!(copy_dest, atree_large)

@test arena_push.nodes === arena_push_nodes
@test pointer(arena_push.nodes) == arena_push_ptr
@test length(arena_push.nodes) == arena_push_len + 1
@test parent_arena.nodes === parent_nodes
@test pointer(parent_arena.nodes) == parent_ptr
@test length(parent_arena.nodes) == parent_len + count_nodes(child)
@test arena_large.nodes === arena_large_nodes
@test pointer(arena_large.nodes) == arena_large_ptr
@test length(arena_large.nodes) == arena_large_len + count_nodes(tree_large)
@test copy_dest.nodes === copy_dest_nodes
@test pointer(copy_dest.nodes) == copy_dest_ptr
@test length(copy_dest.nodes) == count_nodes(atree_large)
