module NodeUtilsModule

import Compat: Returns
import ..NodeModule:
    AbstractNode,
    AbstractExpressionNode,
    Node,
    NodeTuple,
    preserve_sharing,
    constructorof,
    copy_node,
    count_nodes,
    tree_mapreduce,
    any,
    filter_map
import ..ValueInterfaceModule:
    pack_scalar_constants!, unpack_scalar_constants, count_scalar_constants, get_number_type

"""
    count_depth(tree::AbstractNode)::Int

Compute the max depth of the tree.
"""
function count_depth(tree::AbstractNode)
    return tree_mapreduce(
        Returns(1), (p, child...) -> p + max(child...), tree, Int64; break_sharing=Val(true)
    )
end

"""
    is_node_constant(tree::AbstractExpressionNode)::Bool

Check if the current node in a tree is constant.
"""
@inline is_node_constant(tree::AbstractExpressionNode) = tree.degree == 0 && tree.constant

"""
    count_constant_nodes(tree::AbstractExpressionNode)::Int

Count the number of constant nodes in a tree.
"""
function count_constant_nodes(tree::AbstractExpressionNode)
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
    count_scalar_constants(tree::AbstractExpressionNode{T})::Int64 where {T}

Counts the number of scalar constants in the tree.
Used in get_scalar_constants to preallocate a vector for storing constants array.
"""
function count_scalar_constants(tree::AbstractExpressionNode{T}) where {T}
    return tree_mapreduce(
        node -> is_node_constant(node) ? count_scalar_constants(node.val) : 0,
        +,
        tree,
        Int64;
        f_on_shared=(c, is_shared) -> is_shared ? 0 : c,
    )
end

"""
    get_scalar_constants(tree::AbstractExpressionNode{T}, BT::Type = T)::Vector{T} where {T}

Get all the scalar constants inside a tree, in depth-first order.
The function `set_scalar_constants!` sets them in the same order,
given the output of this function.
Also return metadata that can will be used in the `set_scalar_constants!` function.
"""
function get_scalar_constants(
    tree::AbstractExpressionNode{T}, ::Type{BT}=get_number_type(T)
) where {T,BT}
    refs = filter_map(
        is_node_constant, node -> Ref(node), tree, Base.RefValue{typeof(tree)}
    )
    if T <: Number
        # NOTE: Do not remove this `::T` as it is required for inference on empty collections
        return map(r -> r[].val::T, refs), refs
    else
        vals = Vector{BT}(undef, count_scalar_constants(tree))
        i = firstindex(vals)
        for ref in refs
            i = pack_scalar_constants!(vals, i, ref[].val::T)
        end
        return vals, refs
    end
end

"""
    set_scalar_constants!(tree::AbstractExpressionNode{T}, constants, refs) where {T}

Set the constants in a tree, in depth-first order. The function
`get_scalar_constants` gets them in the same order.
"""
function set_scalar_constants!(tree::AbstractExpressionNode{T}, constants, refs) where {T}
    if T <: Number
        @inbounds for i in eachindex(refs, constants)
            refs[i][].val = constants[i]
        end
    else
        nums_i = 1
        refs_i = 1
        while nums_i <= length(constants) && refs_i <= length(refs)
            ix, v = unpack_scalar_constants(constants, nums_i, refs[refs_i][].val::T)
            refs[refs_i][].val = v
            nums_i = ix
            refs_i += 1
        end
        if nums_i <= length(constants) || refs_i <= length(refs)
            error("`set_scalar_constants!` failed due to bad `unpack_scalar_constants`")
        end
    end
    return tree
end

## Assign index to nodes of a tree
# This will mirror a Node struct, rather
# than adding a new attribute to Node.
mutable struct NodeIndex{T,D} <: AbstractNode{D}
    degree::UInt8  # 0 for constant/variable, 1 for cos/sin, 2 for +/* etc.
    val::T  # If is a constant, this stores the actual value
    # ------------------- (possibly undefined below)
    children::NodeTuple{D,NodeIndex{T,D}}

    NodeIndex{_T,_D}() where {_T,_D} = new{_T,_D}()
    function NodeIndex(::Type{_T}, ::Val{_D}, val) where {_T,_D}
        return new{_T,_D}(0, convert(_T, val), NodeTuple(_ -> NodeIndex{_T,_D}(), Val(_D)))
    end
    function NodeIndex(
        ::Type{_T}, ::Val{_D}, children::Vararg{NodeIndex{_T,_D},_D2}
    ) where {_T,_D,_D2}
        _children = NodeTuple(i -> i <= _D2 ? children[i] : NodeIndex{_T,_D}(), Val(_D))
        return new{_T,_D}(convert(UInt8, _D2), zero(_T), _children)
    end
end
NodeIndex(::Type{T}, ::Val{D}) where {T,D} = NodeIndex(T, Val(D), zero(T))
@inline function Base.getproperty(n::NodeIndex, k::Symbol)
    if k == :children
        return getfield(n, :children).data
    elseif k == :l
        return getfield(n, :children).data[1]
    elseif k == :r
        return getfield(n, :children).data[2]
    else
        return getfield(n, k)
    end
end

# Sharing is never needed for NodeIndex,
# as we trace over the node we are indexing on.
preserve_sharing(::Union{Type{<:NodeIndex},NodeIndex}) = false

function index_constant_nodes(
    tree::AbstractExpressionNode{Ti,D} where {Ti}, ::Type{T}=UInt16
) where {D,T}
    # Essentially we copy the tree, replacing the values
    # with indices
    constant_index = Ref(T(0))
    return tree_mapreduce(
        t -> if t.constant
            NodeIndex(T, Val(D), (constant_index[] += T(1)))
        else
            NodeIndex(T, Val(D))
        end,
        t -> nothing,
        (_, c...) -> NodeIndex(T, Val(D), c...),
        tree,
        NodeIndex{T,D};
    )
end

end
