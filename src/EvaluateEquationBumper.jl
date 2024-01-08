module EvaluateEquationBumperModule

using Bumper: @no_escape, @alloc, AllocBuffer
using StrideArrays
import ..OperatorEnumModule: OperatorEnum
import ..EquationModule: AbstractExpressionNode, tree_mapreduce

"""
    ResultOk{A}

Stores the result of an evaluation and whether
any errors occured during the evaluation. This
is used to quit early, so that we do not pass
`Inf` to `cos` (for example).
"""
struct ResultOk{A}
    x::A
    ok::Bool
end

function bumper_eval_tree_array(
    tree::AbstractExpressionNode{T}, cX::AbstractMatrix{T}, operators::OperatorEnum
) where {T}
    result = similar(cX, axes(cX, 2))
    n = size(cX, 2)
    ok = @no_escape begin
        _result_ok = tree_mapreduce(
            # Leaf nodes, we create an allocation and fill
            # it with the value of the leaf:
            leaf -> begin
                ar = @alloc(T, n)
                if leaf.constant
                    v = leaf.val::T
                    ar .= v
                else
                    @inbounds @simd for j in eachindex(ar, axes(cX, 2))
                        ar[j] = cX[leaf.feature, j]
                    end
                end
                ResultOk(ar, true)
            end,
            # Branch nodes, we simply pass them to the evaluation kernel:
            branch -> branch,
            # In the evaluation kernel, we combine the branch nodes
            # with the arrays created by the leaf nodes:
            ((branch, cumulators::Vararg{Any,M}) where {M}) -> begin
                if M == 1
                    if cumulators[1].ok
                        out = kern1!(operators.unaops[branch.op], cumulators[1].x)
                        ResultOk(out, isfinite(sum(yi -> yi * zero(yi), out)))
                    else
                        cumulators[1]
                    end
                else
                    if cumulators[1].ok && cumulators[2].ok
                        out = kern2!(
                            operators.binops[branch.op],
                            cumulators[1].x,
                            cumulators[2].x,
                        )
                        ResultOk(out, isfinite(sum(yi -> yi * zero(yi), out)))
                    elseif cumulators[1].ok
                        cumulators[2]
                    else
                        cumulators[1]
                    end
                end
            end,
            tree;
            break_sharing=Val(true),
        )
        x = _result_ok.x
        result .= x
        _result_ok.ok
    end
    return (result, ok)
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
