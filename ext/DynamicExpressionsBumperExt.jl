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
) where {T,turbo}
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
                    v = leaf_node.val::T
                    ar .= v
                    isfinite(v)
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
                dispatch_kerns!(operators, args..., Val(turbo)),
            tree;
            break_sharing=Val(true),
        )
        x = _result_ok.x
        result .= x
        all_ok[] = _result_ok.ok
    end
    return (result, all_ok[])
end

function dispatch_kerns!(operators, branch_node, cumulator, ::Val{turbo}) where {turbo}
    cumulator.ok || return cumulator

    out = dispatch_kern1!(operators.unaops, branch_node.op, cumulator.x, Val(turbo))
    return ResultOk(out, !is_bad_array(out))
end
function dispatch_kerns!(
    operators, branch_node, cumulator1, cumulator2, ::Val{turbo}
) where {turbo}
    cumulator1.ok || return cumulator1
    cumulator2.ok || return cumulator2

    out = dispatch_kern2!(
        operators.binops, branch_node.op, cumulator1.x, cumulator2.x, Val(turbo)
    )
    return ResultOk(out, !is_bad_array(out))
end

@generated function dispatch_kern1!(unaops, op_idx, cumulator, ::Val{turbo}) where {turbo}
    nuna = counttuple(unaops)
    quote
        Base.@nif(
            $nuna,
            i -> i == op_idx,
            i -> let op = unaops[i]
                return bumper_kern1!(op, cumulator, Val(turbo))
            end,
        )
    end
end
@generated function dispatch_kern2!(
    binops, op_idx, cumulator1, cumulator2, ::Val{turbo}
) where {turbo}
    nbin = counttuple(binops)
    quote
        Base.@nif(
            $nbin,
            i -> i == op_idx,
            i -> let op = binops[i]
                return bumper_kern2!(op, cumulator1, cumulator2, Val(turbo))
            end,
        )
    end
end
function bumper_kern1!(op::F, cumulator, ::Val{false}) where {F}
    @. cumulator = op(cumulator)
    return cumulator
end
function bumper_kern2!(op::F, cumulator1, cumulator2, ::Val{false}) where {F}
    @. cumulator1 = op(cumulator1, cumulator2)
    return cumulator1
end

end
