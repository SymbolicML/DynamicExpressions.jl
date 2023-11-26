module EquationUtilsModule

import Compat: Returns
import ..EquationModule: AbstractNode, Node, copy_node, tree_mapreduce, any, filter_map

"""
    count_nodes(tree::AbstractNode; preserve_sharing=false)::Int

Count the number of nodes in the tree.

If `preserve_sharing` is true, this will return a count of the
unique nodes.
"""
function count_nodes(tree::AbstractNode; preserve_sharing=false)
    return tree_mapreduce(
        _ -> 1,
        +,
        tree,
        Int64;
        preserve_sharing,
        f_on_shared=(c, is_shared) -> is_shared ? 0 : c,
    )
end

"""
    count_depth(tree::AbstractNode)::Int

Compute the max depth of the tree.
"""
function count_depth(tree::AbstractNode)
    return tree_mapreduce(Returns(1), (p, child...) -> p + max(child...), tree)
end

"""
    is_node_constant(tree::Node)::Bool

Check if the current node in a tree is constant.
"""
@inline is_node_constant(tree::Node) = tree.degree == 0 && tree.constant

"""
    count_constants(tree::Node; preserve_sharing=false)::Int

Count the number of constants in a tree.

If `preserve_sharing` is true, this will return a count of the
unique nodes holding a constant.
"""
function count_constants(tree::Node; preserve_sharing=false)
    return tree_mapreduce(
        node -> is_node_constant(node) ? 1 : 0,
        +,
        tree,
        Int64;
        preserve_sharing,
        f_on_shared=(c, is_shared) -> is_shared ? 0 : c,
    )
end

"""
    has_constants(tree::Node)::Bool

Check if a tree has any constants.
"""
has_constants(tree::Node) = any(is_node_constant, tree)

"""
    has_operators(tree::Node)::Bool

Check if a tree has any operators.
"""
has_operators(tree::Node) = tree.degree != 0

"""
    is_constant(tree::Node)::Bool

Check if an expression is a constant numerical value, or
whether it depends on input features.
"""
is_constant(tree::Node) = all(t -> t.degree != 0 || t.constant, tree)

"""
    get_constants(tree::Node{T}; preserve_sharing=false)::Vector{T} where {T}

Get all the constants inside a tree, in depth-first order.
The function `set_constants!` sets them in the same order,
given the output of this function.
"""
function get_constants(tree::Node{T}; preserve_sharing=false) where {T}
    return filter_map(is_node_constant, t -> (t.val::T), tree, T; preserve_sharing)
end

"""
    set_constants!(tree::Node{T}, constants::AbstractVector{T}; preserve_sharing=false) where {T}

Set the constants in a tree, in depth-first order. The function
`get_constants` gets them in the same order. Use `preserve_sharing=true`
if you also used it in `get_constants`.
"""
function set_constants!(
    tree::Node{T}, constants::AbstractVector{T}; preserve_sharing=false
) where {T}
    _constants = copy(constants)
    foreach(tree; preserve_sharing) do node
        if node.degree == 0 && node.constant
            node.val = popfirst!(_constants)
        end
    end
    return nothing
end

## Assign index to nodes of a tree
# This will mirror a Node struct, rather
# than adding a new attribute to Node.
const NodeIndex = Node{UInt16}

function index_constants(tree::Node; preserve_sharing=false)
    # Essentially we copy the tree, replacing the values
    # with indices
    T = UInt16
    constant_index = Ref(T(0))
    return tree_mapreduce(
        t -> if t.constant
            Node(T; val=(constant_index[] += T(1)))
        else
            Node(T; feature=t.feature)
        end,
        t -> t.op,
        (op, c...) -> Node(op, c...),
        tree,
        NodeIndex;
        preserve_sharing,
    )
end

end
