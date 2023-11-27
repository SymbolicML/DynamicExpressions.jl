module EquationUtilsModule

import Compat: Returns
import ..EquationModule:
    AbstractNode,
    AbstractExpressionNode,
    Node,
    preserve_sharing,
    constructorof,
    copy_node,
    tree_mapreduce,
    any,
    filter_map

"""
    count_nodes(tree::AbstractNode)::Int

Count the number of nodes in the tree.
"""
function count_nodes(tree::AbstractNode; break_sharing=Val(false))
    return tree_mapreduce(
        _ -> 1,
        +,
        tree,
        Int64;
        break_sharing,
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
    _constants = copy(constants)
    foreach(tree) do node
        if node.degree == 0 && node.constant
            node.val = popfirst!(_constants)
        end
    end
    return nothing
end

## Assign index to nodes of a tree
# This will mirror a Node struct, rather
# than adding a new attribute to Node.
struct NodeIndex{T,N<:AbstractExpressionNode{T}} <: AbstractExpressionNode{T}
    data::N
    NodeIndex(data::_N) where {_T,_N<:AbstractExpressionNode{_T}} = new{_T,_N}(data)
end
function Base.getproperty(n::NodeIndex, s::Symbol)
    if s == :l
        return NodeIndex(getfield(n, :data).l)
    elseif s == :r
        return NodeIndex(getfield(n, :data).r)
    else
        return getproperty(getfield(n, :data), s)
    end
end
preserve_sharing(::Type{<:NodeIndex{T,N}}) where {T,N} = preserve_sharing(N)

function index_constants(tree::N) where {N<:AbstractExpressionNode}
    # Essentially we copy the tree, replacing the values
    # with indices
    T = UInt16
    output_N = constructorof(N){T}
    constant_index = Ref(T(0))
    raw_output = tree_mapreduce(
        t -> if t.constant
            constructorof(N)(T; val=(constant_index[] += T(1)))
        else
            constructorof(N)(T; feature=t.feature)
        end,
        t -> t.op,
        (op, c...) -> constructorof(N)(op, c...),
        tree,
        output_N;
    )::output_N
    return NodeIndex(raw_output)::NodeIndex{T,output_N}
end

end
