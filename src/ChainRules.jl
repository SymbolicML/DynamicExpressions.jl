module ChainRulesModule

using ChainRulesCore:
    ChainRulesCore, AbstractTangent, NoTangent, ZeroTangent, Tangent, @thunk, canonicalize
using ..OperatorEnumModule: OperatorEnum
using ..NodeModule: AbstractExpressionNode, with_type_parameters, tree_mapreduce
using ..EvaluateModule: eval_tree_array
using ..EvaluateDerivativeModule: eval_grad_tree_array

struct NodeTangent{T,N<:AbstractExpressionNode{T},A<:AbstractArray{T}} <: AbstractTangent
    tree::N
    gradient::A
end
function Base.:+(a::NodeTangent, b::NodeTangent)
    @assert a.tree == b.tree
    return NodeTangent(a.tree, a.gradient + b.gradient)
end
Base.:*(a::Number, b::NodeTangent) = NodeTangent(b.tree, a * b.gradient)
Base.:*(a::NodeTangent, b::Number) = NodeTangent(a.tree, a.gradient * b)
Base.zero(::Union{Type{NodeTangent},NodeTangent}) = ZeroTangent()

function ChainRulesCore.rrule(
    ::typeof(eval_tree_array),
    tree::AbstractExpressionNode,
    X::AbstractMatrix,
    operators::OperatorEnum;
    turbo=Val(false),
    bumper=Val(false),
)
    primal, complete = eval_tree_array(tree, X, operators; turbo, bumper)

    if !complete
        primal .= NaN
    end

    # TODO: Preferable to use the primal in the pullback somehow
    function pullback((dY, _))
        dtree = let X = X, dY = dY, tree = tree, operators = operators
            @thunk(
                let
                    _, gradient, complete = eval_grad_tree_array(
                        tree, X, operators; variable=Val(false)
                    )
                    if !complete
                        gradient .= NaN
                    end

                    NodeTangent(
                        tree,
                        sum(j -> gradient[:, j] * dY[j], eachindex(dY, axes(gradient, 2))),
                    )
                end
            )
        end
        dX = let X = X, dY = dY, tree = tree, operators = operators
            @thunk(
                let
                    _, gradient, complete = eval_grad_tree_array(
                        tree, X, operators; variable=Val(true)
                    )
                    if !complete
                        gradient .= NaN
                    end

                    gradient .* reshape(dY, 1, length(dY))
                end
            )
        end
        return (NoTangent(), dtree, dX, NoTangent())
    end

    return (primal, complete), pullback
end

end
