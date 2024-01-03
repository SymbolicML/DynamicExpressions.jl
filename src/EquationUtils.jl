module EquationUtilsModule

import Compat: Returns
import ..EquationModule:
    AbstractNode,
    AbstractExpressionNode,
    Node,
    preserve_sharing,
    constructorof,
    copy_node,
    count_nodes,
    tree_mapreduce,
    any,
    filter_map

"""
    count_depth(tree::AbstractNode)::Int

Compute the max depth of the tree.
"""
function count_depth(tree::N) where {N<:AbstractNode}
    return tree_mapreduce(
        Returns(1),
        ((p, child::Vararg{Int64,M}) where {M}) -> p + max(child...),
        tree,
        Int64;
        break_sharing=Val(true),
    )
end

"""
    is_node_constant(tree::AbstractExpressionNode)::Bool

Check if the current node in a tree is constant.
"""
@inline is_node_constant(tree::AbstractExpressionNode) = tree.degree == 0 && tree.constant

"""
    count_constants(tree::AbstractExpressionNode)::Int

Count the number of constants in a tree.
"""
function count_constants(tree::AbstractExpressionNode)
    return tree_mapreduce(
        node -> is_node_constant(node) ? 1 : 0,
        +,
        tree,
        Int64;
        f_on_shared=(c, is_shared) -> is_shared ? 0 : c,
    )
end

"""
    has_constants(tree::AbstractExpressionNode)::Bool

Check if a tree has any constants.
"""
has_constants(tree::AbstractExpressionNode) = any(is_node_constant, tree)

"""
    has_operators(tree::AbstractExpressionNode)::Bool

Check if a tree has any operators.
"""
has_operators(tree::AbstractExpressionNode) = tree.degree != 0

"""
    is_constant(tree::AbstractExpressionNode)::Bool

Check if an expression is a constant numerical value, or
whether it depends on input features.
"""
is_constant(tree::AbstractExpressionNode) = all(t -> t.degree != 0 || t.constant, tree)

"""
    get_constants(tree::AbstractExpressionNode{T})::Vector{T} where {T}

Get all the constants inside a tree, in depth-first order.
The function `set_constants!` sets them in the same order,
given the output of this function.
"""
function get_constants(tree::AbstractExpressionNode{T}) where {T}
    return filter_map(is_node_constant, t -> (t.val::T), tree, T)
end

"""
    set_constants!(tree::AbstractExpressionNode{T}, constants::AbstractVector{T}) where {T}

Set the constants in a tree, in depth-first order. The function
`get_constants` gets them in the same order.
"""
function set_constants!(
    tree::AbstractExpressionNode{T}, constants::AbstractVector{T}
) where {T}
    Base.require_one_based_indexing(constants)
    i = Ref(0)
    foreach(tree) do node
        if node.degree == 0 && node.constant
            @inbounds node.val = constants[i[] += 1]
        end
    end
    return nothing
end

## Assign index to nodes of a tree
# This will mirror a Node struct, rather
# than adding a new attribute to Node.
struct NodeIndex{T} <: AbstractNode
    degree::UInt8  # 0 for constant/variable, 1 for cos/sin, 2 for +/* etc.
    val::T  # If is a constant, this stores the actual value
    # ------------------- (possibly undefined below)
    l::NodeIndex{T}  # Left child node. Only defined for degree=1 or degree=2.
    r::NodeIndex{T}  # Right child node. Only defined for degree=2. 

    NodeIndex(::Type{_T}) where {_T} = new{_T}(0, zero(_T))
    NodeIndex(::Type{_T}, val) where {_T} = new{_T}(0, convert(_T, val))
    NodeIndex(::Type{_T}, l::NodeIndex) where {_T} = new{_T}(1, zero(_T), l)
    function NodeIndex(::Type{_T}, l::NodeIndex, r::NodeIndex) where {_T}
        return new{_T}(2, zero(_T), l, r)
    end
end
# Sharing is never needed for NodeIndex,
# as we trace over the node we are indexing on.
preserve_sharing(::Type{<:NodeIndex}) = false

function index_constants(tree::N, ::Type{T}=UInt16) where {T,N<:AbstractExpressionNode}
    # Essentially we copy the tree, replacing the values
    # with indices
    constant_index = Ref(T(0))
    return tree_mapreduce(
        t -> if t.constant
            NodeIndex(T, (constant_index[] += T(1)))
        else
            NodeIndex(T)
        end,
        t -> nothing,
        ((_, c::Vararg{NodeIndex{T},M}) where {M}) -> NodeIndex(T, c...),
        tree,
        NodeIndex{T};
    )
end

end
