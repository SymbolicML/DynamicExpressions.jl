module EquationUtilsModule

import ..EquationModule: Node, copy_node, tree_mapreduce, any, filter_and_map

"""
    count_nodes(tree::Node{T})::Int where {T}

Count the number of nodes in the tree.
"""
count_nodes(tree::Node) = tree_mapreduce(_ -> 1, +, tree)
# This code is given as an example. Normally we could just use sum(Returns(1), tree).

"""
    count_depth(tree::Node{T})::Int where {T}

Compute the max depth of the tree.
"""
count_depth(tree::Node) = tree_mapreduce(_ -> 1, (p, child...) -> p + max(child...), tree)

"""
    is_node_constant(tree::Node)::Bool

Check if the current node in a tree is constant.
"""
@inline is_node_constant(tree::Node) = tree.degree == 0 && tree.constant

"""
    count_constants(tree::Node)::Int

Count the number of constants in a tree.
"""
count_constants(tree::Node) = count(is_node_constant, tree)

"""
    has_constants(tree::Node)::Bool

Check if a tree has any constants.
"""
has_constants(tree::Node) = any(is_node_constant, tree)

"""
    has_operators(tree::Node)::Bool

Check if a tree has any operators.
"""
has_operators(tree::Node) = tree.degree > 0

"""
    is_constant(tree::Node)::Bool

Check if an expression is a constant numerical value, or
whether it depends on input features.
"""
is_constant(tree::Node) = all(t -> t.degree > 0 || tree.constant, tree)

"""
    get_constants(tree::Node{T})::Vector{T} where {T}

Get all the constants inside a tree, in depth-first order.
The function `set_constants!` sets them in the same order,
given the output of this function.
"""
function get_constants(tree::Node{T}) where {T}
    return filter_and_map(is_node_constant, t -> t.val::T, tree; result_type=T)
end

"""
    set_constants!(tree::Node{T}, constants::AbstractVector{T}) where {T}

Set the constants in a tree, in depth-first order.
The function `get_constants` gets them in the same order,
"""
function set_constants!(tree::Node{T}, constants::AbstractVector{T}) where {T}
    if tree.degree == 0
        if tree.constant
            tree.val = constants[1]
        end
    elseif tree.degree == 1
        set_constants!(tree.l, constants)
    else
        numberLeft = count_constants(tree.l)
        set_constants!(tree.l, constants)
        set_constants!(tree.r, @view constants[(numberLeft + 1):end])
    end
    return nothing
end

## Assign index to nodes of a tree
# This will mirror a Node struct, rather
# than adding a new attribute to Node.
mutable struct NodeIndex
    constant_index::Int  # Index of this constant (if a constant exists here)
    l::NodeIndex
    r::NodeIndex

    NodeIndex() = new()
end

function index_constants(tree::Node)::NodeIndex
    return index_constants(tree, 0)
end

function index_constants(tree::Node, left_index::Int)::NodeIndex
    index_tree = NodeIndex()
    index_constants(tree, index_tree, left_index)
    return index_tree
end

# Count how many constants to the left of this node, and put them in a tree
function index_constants(tree::Node, index_tree::NodeIndex, left_index::Int)
    if tree.degree == 0
        if tree.constant
            index_tree.constant_index = left_index + 1
        end
    elseif tree.degree == 1
        index_tree.constant_index = count_constants(tree.l)
        index_tree.l = NodeIndex()
        index_constants(tree.l, index_tree.l, left_index)
    else
        index_tree.l = NodeIndex()
        index_tree.r = NodeIndex()
        index_constants(tree.l, index_tree.l, left_index)
        index_tree.constant_index = count_constants(tree.l)
        left_index_here = left_index + index_tree.constant_index
        index_constants(tree.r, index_tree.r, left_index_here)
    end
end

end
