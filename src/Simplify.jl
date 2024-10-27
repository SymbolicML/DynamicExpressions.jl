module SimplifyModule

import ..NodeModule: AbstractExpressionNode, constructorof, Node, copy_node, set_node!
import ..NodeUtilsModule: tree_mapreduce, is_node_constant
import ..OperatorEnumModule: AbstractOperatorEnum
import ..ValueInterfaceModule: is_valid
import ..EvaluateModule: get_nbin

_una_op_kernel(f::F, l::T) where {F,T} = f(l)
_bin_op_kernel(f::F, l::T, r::T) where {F,T} = f(l, r)

# Operator traits
is_commutative(::typeof(*)) = true
is_commutative(::typeof(+)) = true
is_commutative(_) = false

# Zero-related traits
has_right_identity_zero(::typeof(+)) = true
has_right_identity_zero(::typeof(-)) = true
has_right_identity_zero(_) = false

has_left_identity_zero(::typeof(+)) = true
has_left_identity_zero(_) = false

absorbed_by_zero(::typeof(*)) = true
absorbed_by_zero(_) = false

# One-related traits
has_identity_one(::typeof(*)) = true
has_identity_one(_) = false

# Self-operation traits
simplifies_given_equal_operands(::typeof(/)) = true
simplifies_given_equal_operands(_) = false

combine_operators!(tree::AbstractExpressionNode, ::AbstractOperatorEnum) = tree

function combine_operators!(tree::Node{T}, operators::AbstractOperatorEnum) where {T}
    deg = tree.degree
    deg == 0 && return tree
    tree.l = combine_operators!(tree.l, operators)
    deg == 1 && return tree
    tree.r = combine_operators!(tree.r, operators)
    return dispatch_deg2_simplify!(tree, operators)
end
@generated function dispatch_deg2_simplify!(
    tree::Node{T}, operators::AbstractOperatorEnum
) where {T}
    nbin = get_nbin(operators)
    quote
        op_idx = tree.op
        return Base.Cartesian.@nif(
            $nbin, i -> i == op_idx, i -> _combine_operators_on!(tree, operators.binops[i])
        )
    end
end

function _combine_operators_on!(tree::Node{T}, f::F) where {F,T}
    # NOTE: This assumes tree.degree == 2 and tree.op corresponds to f
    # Handle basic simplifications first
    if is_node_constant(tree.r)
        rval = tree.r.val
        if rval == zero(T)
            # Operations where right zero is identity (x + 0 -> x, x - 0 -> x)
            if has_right_identity_zero(f)
                return tree.l
            end
            # Operations that are absorbed by zero (x * 0 -> 0)
            if absorbed_by_zero(f)
                return tree.r
            end
        elseif rval == one(T)
            # x * 1 -> x
            if has_identity_one(f)
                return tree.l
            end
        end
    end

    if is_node_constant(tree.l)
        lval = tree.l.val
        if lval == zero(T)
            # Operations where left zero is identity (0 + x -> x)
            if has_left_identity_zero(f)
                return tree.r
            end
            # Operations that are absorbed by zero (0 * x -> 0)
            if absorbed_by_zero(f)
                return tree.l
            end
        elseif lval == one(T) && has_identity_one(f)
            # 1 * x -> x
            return tree.r
        end
    end

    # x/x -> 1, or other self-simplifying operations
    if simplifies_given_equal_operands(f) && tree.l == tree.r
        return constructorof(typeof(tree))(T; val=one(T))
    end

    # Handle commutative operations with constants
    if is_commutative(f) && (is_node_constant(tree.l) || is_node_constant(tree.r))
        # Put constant on right for consistent handling
        if is_node_constant(tree.l)
            tree.l, tree.r = tree.r, tree.l
        end

        # Now we know tree.r is constant
        below = tree.l
        if below.degree == 2 && below.op == tree.op
            # Combine nested operations with constants: ((a * x) * b) -> ((a*b) * x)
            if is_node_constant(below.l)
                below.l.val = _bin_op_kernel(f, below.l.val, tree.r.val)
                return below
            elseif is_node_constant(below.r)
                below.r.val = _bin_op_kernel(f, below.r.val, tree.r.val)
                return below
            end
        end
    end

    return tree
end

function combine_children!(operators, p::N, c::N...) where {T,N<:AbstractExpressionNode{T}}
    all(is_node_constant, c) || return p
    vals = map(n -> n.val, c)
    all(is_valid, vals) || return p
    out = if length(c) == 1
        _una_op_kernel(operators.unaops[p.op], vals...)
    else
        _bin_op_kernel(operators.binops[p.op], vals...)
    end
    is_valid(out) || return p
    new_node = constructorof(N)(T; val=convert(T, out))
    set_node!(p, new_node)
    return p
end

function simplify_tree!(tree::AbstractExpressionNode, operators::AbstractOperatorEnum)
    return tree_mapreduce(
        identity, (p, c...) -> combine_children!(operators, p, c...), tree, typeof(tree);
    )
end

end
