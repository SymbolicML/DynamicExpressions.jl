module SimplifyEquationModule

import ..EquationModule: AbstractExpressionNode, constructorof, Node, copy_node
import ..EquationUtilsModule: tree_mapreduce, is_node_constant
import ..OperatorEnumModule: AbstractOperatorEnum
import ..UtilsModule: isbad, isgood

_una_op_kernel(f::F, l::T) where {F,T} = f(l)
_bin_op_kernel(f::F, l::T, r::T) where {F,T} = f(l, r)

is_commutative(::typeof(*)) = true
is_commutative(::typeof(+)) = true
is_commutative(_) = false

is_subtraction(::typeof(-)) = true
is_subtraction(_) = false

# This is only defined for `Node` as it is not possible for
# `GraphNode`.
function combine_operators!(tree::Node{T}, operators::AbstractOperatorEnum) where {T}
    # NOTE: (const (+*-) const) already accounted for. Call simplify_tree before.
    # ((const + var) + const) => (const + var)
    # ((const * var) * const) => (const * var)
    # ((const - var) - const) => (const - var)
    # (want to add anything commutative!)
    # TODO - need to combine plus/sub if they are both there.
    if tree.degree == 0
        return tree
    elseif tree.degree == 1
        tree.l = combine_operators!(tree.l, operators)
    elseif tree.degree == 2
        tree.l = combine_operators!(tree.l, operators)
        tree.r = combine_operators!(tree.r, operators)
    end

    top_level_constant =
        tree.degree == 2 && (is_node_constant(tree.l) || is_node_constant(tree.r))
    if tree.degree == 2 && is_commutative(operators.binops[tree.op]) && top_level_constant
        # TODO: Does this break SymbolicRegression.jl due to the different names of operators?
        op = tree.op
        # Put the constant in r. Need to assume var in left for simplification assumption.
        if is_node_constant(tree.l)
            tmp = tree.r
            tree.r = tree.l
            tree.l = tmp
        end
        topconstant = tree.r.val
        # Simplify down first
        below = tree.l
        if below.degree == 2 && below.op == op
            if is_node_constant(below.l)
                tree = below
                tree.l.val = _bin_op_kernel(operators.binops[op], tree.l.val, topconstant)
            elseif is_node_constant(below.r)
                tree = below
                tree.r.val = _bin_op_kernel(operators.binops[op], tree.r.val, topconstant)
            end
        end
    end

    if tree.degree == 2 && is_subtraction(operators.binops[tree.op]) && top_level_constant

        # Currently just simplifies subtraction. (can't assume both plus and sub are operators)
        # Not commutative, so use different op.
        if is_node_constant(tree.l)
            if tree.r.degree == 2 && tree.op == tree.r.op
                if is_node_constant(tree.r.l)
                    #(const - (const - var)) => (var - const)
                    l = tree.l
                    r = tree.r
                    simplified_const = (r.l.val - l.val) #neg(sub(l.val, r.l.val))
                    tree.l = tree.r.r
                    tree.r = l
                    tree.r.val = simplified_const
                elseif is_node_constant(tree.r.r)
                    #(const - (var - const)) => (const - var)
                    l = tree.l
                    r = tree.r
                    simplified_const = l.val + r.r.val #plus(l.val, r.r.val)
                    tree.r = tree.r.l
                    tree.l.val = simplified_const
                end
            end
        else #tree.r is a constant
            if tree.l.degree == 2 && tree.op == tree.l.op
                if is_node_constant(tree.l.l)
                    #((const - var) - const) => (const - var)
                    l = tree.l
                    r = tree.r
                    simplified_const = l.l.val - r.val#sub(l.l.val, r.val)
                    tree.r = tree.l.r
                    tree.l = r
                    tree.l.val = simplified_const
                elseif is_node_constant(tree.l.r)
                    #((var - const) - const) => (var - const)
                    l = tree.l
                    r = tree.r
                    simplified_const = r.val + l.r.val #plus(r.val, l.r.val)
                    tree.l = tree.l.l
                    tree.r.val = simplified_const
                end
            end
        end
    end
    return tree
end

function combine_children(
    operators, p::N, c::Vararg{N,M}
) where {T,N<:AbstractExpressionNode{T},M}
    if !all(is_node_constant, c)
        return p
    end
    vals = map(n -> n.val, c)
    if !all(isgood, vals)
        return p
    end
    out = if length(c) == 1
        _una_op_kernel(operators.unaops[p.op], vals...)
    else
        _bin_op_kernel(operators.binops[p.op], vals...)
    end
    if !isgood(out)
        return p
    end
    return constructorof(N)(T; val=convert(T, out))
end

# Simplify tree
function simplify_tree(tree::AbstractExpressionNode, operators::AbstractOperatorEnum)
    return tree_mapreduce(
        identity, (p, c...) -> combine_children(operators, p, c...), tree, typeof(tree);
    )
end

end
