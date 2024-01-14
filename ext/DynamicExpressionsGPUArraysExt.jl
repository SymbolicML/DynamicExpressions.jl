module DynamicExpressionsGPUArraysExt

using GPUArrays: AbstractDeviceArray
using DynamicExpressions: OperatorEnum, AbstractExpressionNode, tree_mapreduce
using DynamicExpressions.UtilsModule: counttuple

import DynamicExpressions.EvaluateEquationModule: eval_tree_array

function eval_tree_array(
    tree::AbstractExpressionNode{T},
    cX::AbstractDeviceArray{T,2},
    operators::OperatorEnum;
    _...,
) where {T<:Number}
    result = tree_mapreduce(
        # Leaf nodes, we create an allocation and fill
        # it with the value of the leaf:
        leaf -> begin
            if leaf.constant
                v = leaf.val::T
                ar = similar(cX, axes(cX, 2))
                ar .= v
                return ar
            else
                return cX[leaf.feature, :]
            end
        end,
        # Branch nodes, we simply pass them to the evaluation kernel:
        branch -> branch,
        # In the evaluation kernel, we combine the branch nodes
        # with the arrays created by the leaf nodes:
        ((branch, cumulators::Vararg{Any,M}) where {M}) -> begin
            if M == 1
                return dispatch_kern1!(operators.unaops, branch.op, cumulators[1])
            else
                return dispatch_kern2!(
                    operators.binops, branch.op, cumulators[1], cumulators[2]
                )
            end
        end,
        tree;
        break_sharing=Val(true),
    )
    return (result, isfinite(sum(result .* zero(T))))
end
@generated function dispatch_kern1!(unaops, op_idx, cumulator)
    nuna = counttuple(unaops)
    quote
        Base.@nif($nuna, i -> i == op_idx, i -> let op = unaops[i]
            return kern1!(op, cumulator)
        end,)
    end
end
@generated function dispatch_kern2!(binops, op_idx, cumulator1, cumulator2)
    nbin = counttuple(binops)
    quote
        Base.@nif(
            $nbin, i -> i == op_idx, i -> let op = binops[i]
                return kern2!(op, cumulator1, cumulator2)
            end,
        )
    end
end
function kern1!(op::F, cumulator) where {F}
    @. cumulator = op(cumulator)
    return cumulator
end
function kern2!(op::F, cumulator1, cumulator2) where {F}
    @. cumulator1 = op(cumulator1, cumulator2)
    return cumulator1
end

end
