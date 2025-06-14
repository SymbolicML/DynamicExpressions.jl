module SimplifyModule

using Compat: Fix

import ..NodeModule:
    AbstractExpressionNode, constructorof, Node, copy_node, set_node!, set_child!, get_child
import ..NodeUtilsModule: tree_mapreduce, is_node_constant
import ..OperatorEnumModule: AbstractOperatorEnum
import ..ValueInterfaceModule: is_valid

@inline _op_kernel(f::F, l::T, ls::T...) where {F,T} = f(l, ls...)

is_commutative(::typeof(*)) = true
is_commutative(::typeof(+)) = true
is_commutative(_) = false

is_subtraction(::typeof(-)) = true
is_subtraction(_) = false

combine_operators(tree::AbstractExpressionNode, ::AbstractOperatorEnum) = tree
# This is only defined for `Node` as it is not possible for, e.g.,
# `GraphNode`, and n-arity nodes.
function combine_operators(tree::Node{T,2}, operators::AbstractOperatorEnum) where {T}
    # NOTE: (const (+*-) const) already accounted for. Call simplify_tree! before.
    # ((const + var) + const) => (const + var)
    # ((const * var) * const) => (const * var)
    # ((const - var) - const) => (const - var)
    # (want to add anything commutative!)
    # TODO - need to combine plus/sub if they are both there.
    if tree.degree == 0
        return tree
    elseif tree.degree == 1
        set_child!(tree, combine_operators(get_child(tree, 1), operators), 1)
    elseif tree.degree == 2
        set_child!(tree, combine_operators(get_child(tree, 1), operators), 1)
        set_child!(tree, combine_operators(get_child(tree, 2), operators), 2)
    end

    top_level_constant =
        tree.degree == 2 &&
        (is_node_constant(get_child(tree, 1)) || is_node_constant(get_child(tree, 2)))
    if tree.degree == 2 && is_commutative(operators.binops[tree.op]) && top_level_constant
        # TODO: Does this break SymbolicRegression.jl due to the different names of operators?
        op = tree.op
        # Put the constant in r. Need to assume var in left for simplification assumption.
        if is_node_constant(get_child(tree, 1))
            tmp = get_child(tree, 2)
            set_child!(tree, get_child(tree, 1), 2)
            set_child!(tree, tmp, 1)
        end
        topconstant = get_child(tree, 2).val
        # Simplify down first
        below = get_child(tree, 1)
        if below.degree == 2 && below.op == op
            if is_node_constant(get_child(below, 1))
                tree = below
                get_child(tree, 1).val = _op_kernel(
                    operators.binops[op], get_child(tree, 1).val, topconstant
                )
            elseif is_node_constant(get_child(below, 2))
                tree = below
                get_child(tree, 2).val = _op_kernel(
                    operators.binops[op], get_child(tree, 2).val, topconstant
                )
            end
        end
    end

    if tree.degree == 2 && is_subtraction(operators.binops[tree.op]) && top_level_constant

        # Currently just simplifies subtraction. (can't assume both plus and sub are operators)
        # Not commutative, so use different op.
        if is_node_constant(get_child(tree, 1))
            if get_child(tree, 2).degree == 2 && tree.op == get_child(tree, 2).op
                if is_node_constant(get_child(get_child(tree, 2), 1))
                    #(const - (const - var)) => (var - const)
                    l = get_child(tree, 1)
                    r = get_child(tree, 2)
                    simplified_const = (get_child(r, 1).val - l.val) #neg(sub(l.val, get_child(r, 1).val))
                    set_child!(tree, get_child(get_child(tree, 2), 2), 1)
                    set_child!(tree, l, 2)
                    get_child(tree, 2).val = simplified_const
                elseif is_node_constant(get_child(get_child(tree, 2), 2))
                    #(const - (var - const)) => (const - var)
                    l = get_child(tree, 1)
                    r = get_child(tree, 2)
                    simplified_const = l.val + get_child(r, 2).val #plus(l.val, get_child(r, 2).val)
                    set_child!(tree, get_child(get_child(tree, 2), 1), 2)
                    get_child(tree, 1).val = simplified_const
                end
            end
        else #get_child(tree, 2) is a constant
            if get_child(tree, 1).degree == 2 && tree.op == get_child(tree, 1).op
                if is_node_constant(get_child(get_child(tree, 1), 1))
                    #((const - var) - const) => (const - var)
                    l = get_child(tree, 1)
                    r = get_child(tree, 2)
                    simplified_const = get_child(l, 1).val - r.val#sub(get_child(l, 1).val, r.val)
                    set_child!(tree, get_child(get_child(tree, 1), 2), 2)
                    set_child!(tree, r, 1)
                    get_child(tree, 1).val = simplified_const
                elseif is_node_constant(get_child(get_child(tree, 1), 2))
                    #((var - const) - const) => (var - const)
                    l = get_child(tree, 1)
                    r = get_child(tree, 2)
                    simplified_const = r.val + get_child(l, 2).val #plus(r.val, get_child(l, 2).val)
                    set_child!(tree, get_child(get_child(tree, 1), 1), 1)
                    get_child(tree, 2).val = simplified_const
                end
            end
        end
    end
    return tree
end

function combine_children!(
    operators, p::N, c::Vararg{N,degree}
) where {T,N<:AbstractExpressionNode{T},degree}
    all(is_node_constant, c) || return p
    vals = map(n -> n.val, c)
    all(is_valid, vals) || return p
    out = _op_kernel(operators[degree][p.op], vals...)
    is_valid(out) || return p
    new_node = constructorof(N)(T; val=convert(T, out))
    set_node!(p, new_node)
    return p
end

# Simplify tree
function simplify_tree!(tree::AbstractExpressionNode, operators::AbstractOperatorEnum)
    return tree_mapreduce(
        identity, Fix{1}(combine_children!, operators), tree, typeof(tree)
    )
end

end
