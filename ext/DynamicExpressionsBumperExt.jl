module DynamicExpressionsBumperExt

using Bumper: @no_escape, @alloc
using DynamicExpressions: OperatorEnum, AbstractExpressionNode, tree_mapreduce
using DynamicExpressions.UtilsModule: ResultOk, counttuple, is_bad_array

import DynamicExpressions.ExtensionInterfaceModule:
    bumper_eval_tree_array, bumper_kern1!, bumper_kern2!

function bumper_eval_tree_array(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    ::Val{turbo},
    ::Val{early_exit},
) where {T,turbo,early_exit}
    result = similar(cX, axes(cX, 2))
    n = size(cX, 2)
    all_ok = Ref(false)
    @no_escape begin
        _result_ok = tree_mapreduce(
            # Leaf nodes, we create an allocation and fill
            # it with the value of the leaf:
            leaf_node -> begin
                ar = @alloc(T, n)
                ok = if leaf_node.constant
                    v = leaf_node.val
                    ar .= v
                    early_exit ? isfinite(v) : true
                else
                    ar .= view(cX, leaf_node.feature, :)
                    true
                end
                ResultOk(ar, ok)
            end,
            # Branch nodes, we simply pass them to the evaluation kernel:
            branch_node -> branch_node,
            # In the evaluation kernel, we combine the branch nodes
            # with the arrays created by the leaf nodes:
            ((args::Vararg{Any,M}) where {M}) ->
                dispatch_kerns!(operators, args..., Val(turbo), Val(early_exit)),
            tree;
            break_sharing=Val(true),
        )
        x = _result_ok.x
        result .= x
        all_ok[] = _result_ok.ok
    end
    return (result, all_ok[])
end

function dispatch_kerns!(
    operators, branch_node, cumulator, ::Val{turbo}, ::Val{early_exit}
) where {turbo,early_exit}
    cumulator.ok || return cumulator

    out = dispatch_kern1!(
        operators.unaops, branch_node.op, cumulator.x, Val(turbo), Val(early_exit)
    )
    return early_exit ? ResultOk(out, !is_bad_array(out)) : ResultOk(out, true)
end
function dispatch_kerns!(
    operators, branch_node, cumulator1, cumulator2, ::Val{turbo}, ::Val{early_exit}
) where {turbo,early_exit}
    cumulator1.ok || return cumulator1
    cumulator2.ok || return cumulator2

    out = dispatch_kern2!(
        operators.binops,
        branch_node.op,
        cumulator1.x,
        cumulator2.x,
        Val(turbo),
        Val(early_exit),
    )
    return early_exit ? ResultOk(out, !is_bad_array(out)) : ResultOk(out, true)
end

@generated function dispatch_kern1!(
    unaops, op_idx, cumulator, ::Val{turbo}, ::Val{early_exit}
) where {turbo,early_exit}
    nuna = counttuple(unaops)
    quote
        Base.@nif(
            $nuna,
            i -> i == op_idx,
            i -> let op = unaops[i]
                return bumper_kern1!(op, cumulator, Val(turbo), Val(early_exit))
            end,
        )
    end
end
@generated function dispatch_kern2!(
    binops, op_idx, cumulator1, cumulator2, ::Val{turbo}, ::Val{early_exit}
) where {turbo,early_exit}
    nbin = counttuple(binops)
    quote
        Base.@nif(
            $nbin,
            i -> i == op_idx,
            i -> let op = binops[i]
                return bumper_kern2!(op, cumulator1, cumulator2, Val(turbo), Val(early_exit))
            end,
        )
    end
end
# FIXME: keeping the early_exit parameter for readability... should it be removed?
function bumper_kern1!(
    op::F, cumulator, ::Val{false}, ::Val{early_exit}
) where {F,early_exit}
    @. cumulator = op(cumulator)
    return cumulator
end
function bumper_kern2!(
    op::F, cumulator1, cumulator2, ::Val{false}, ::Val{early_exit}
) where {F,early_exit}
    @. cumulator1 = op(cumulator1, cumulator2)
    return cumulator1
end

end
