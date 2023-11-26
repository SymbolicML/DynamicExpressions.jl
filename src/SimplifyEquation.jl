module SimplifyEquationModule

import ..EquationModule: Node, copy_node, set_node!
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

# Simplify tree
function combine_operators(
    tree::Node{T}, operators::AbstractOperatorEnum; preserve_sharing=false
) where {T}
    @assert !preserve_sharing "Cannot preserve sharing when rearranging tree, please avoid calling this function."

    # NOTE: (const (+*-) const) already accounted for. Call simplify_tree! before.
    # ((const + var) + const) => (const + var)
    # ((const * var) * const) => (const * var)
    # ((const - var) - const) => (const - var)
    # (want to add anything commutative!)
    # TODO - need to combine plus/sub if they are both there.
    if tree.degree == 0
        return tree
    elseif tree.degree == 1
        tree.l = combine_operators(tree.l, operators)
    elseif tree.degree == 2
        tree.l = combine_operators(tree.l, operators)
        tree.r = combine_operators(tree.r, operators)
    end

    top_level_constant = tree.degree == 2 && (tree.l.constant || tree.r.constant)
    if tree.degree == 2 && is_commutative(operators.binops[tree.op]) && top_level_constant
        # TODO: Does this break SymbolicRegression.jl due to the different names of operators?
        op = tree.op
        # Put the constant in r. Need to assume var in left for simplification assumption.
        if tree.l.constant
            tmp = tree.r
            tree.r = tree.l
            tree.l = tmp
        end
        topconstant = tree.r.val::T
        # Simplify down first
        below = tree.l
        if below.degree == 2 && below.op == op
            if below.l.constant
                tree = below
                tree.l.val = _bin_op_kernel(
                    operators.binops[op], tree.l.val::T, topconstant
                )
            elseif below.r.constant
                tree = below
                tree.r.val = _bin_op_kernel(
                    operators.binops[op], tree.r.val::T, topconstant
                )
            end
        end
    end

    if tree.degree == 2 && is_subtraction(operators.binops[tree.op]) && top_level_constant

        # Currently just simplifies subtraction. (can't assume both plus and sub are operators)
        # Not commutative, so use different op.
        if tree.l.constant
            if tree.r.degree == 2 && tree.op == tree.r.op
                if tree.r.l.constant
                    #(const - (const - var)) => (var - const)
                    l = tree.l
                    r = tree.r
                    simplified_const = (r.l.val::T - l.val::T) #neg(sub(l.val, r.l.val))
                    tree.l = tree.r.r
                    tree.r = l
                    tree.r.val = simplified_const
                elseif tree.r.r.constant
                    #(const - (var - const)) => (const - var)
                    l = tree.l
                    r = tree.r
                    simplified_const = l.val::T + r.r.val::T #plus(l.val, r.r.val)
                    tree.r = tree.r.l
                    tree.l.val = simplified_const
                end
            end
        else #tree.r.constant is true
            if tree.l.degree == 2 && tree.op == tree.l.op
                if tree.l.l.constant
                    #((const - var) - const) => (const - var)
                    l = tree.l
                    r = tree.r
                    simplified_const = l.l.val::T - r.val::T#sub(l.l.val, r.val)
                    tree.r = tree.l.r
                    tree.l = r
                    tree.l.val = simplified_const
                elseif tree.l.r.constant
                    #((var - const) - const) => (var - const)
                    l = tree.l
                    r = tree.r
                    simplified_const = r.val::T + l.r.val::T #plus(r.val, l.r.val)
                    tree.l = tree.l.l
                    tree.r.val = simplified_const
                end
            end
        end
    end
    return tree
end

function combine_children!(operators, p::Node{T}, c::Node{T}...) where {T}
    all(is_node_constant, c) || return p
    vals = map(n -> n.val::T, c)
    all(isgood, vals) || return p
    out = if length(c) == 1
        _una_op_kernel(operators.unaops[p.op], vals...)
    else
        _bin_op_kernel(operators.binops[p.op], vals...)
    end
    isgood(out) || return p
    new_node = Node(T; val=convert(T, out))
    set_node!(p, new_node)
    return p
end

# Simplify tree
function simplify_tree!(
    tree::Node{T}, operators::AbstractOperatorEnum; preserve_sharing=false
) where {T}
    tree = tree_mapreduce(
        identity,
        (p, c...) -> combine_children!(operators, p, c...),
        tree,
        Node{T};
        preserve_sharing,
    )
    return tree
end

end
