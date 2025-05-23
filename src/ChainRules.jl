module ChainRulesModule

using ChainRulesCore:
    ChainRulesCore as CRC,
    AbstractTangent,
    NoTangent,
    ZeroTangent,
    Tangent,
    @thunk,
    canonicalize
using ..OperatorEnumModule: OperatorEnum
using ..NodeModule: AbstractExpressionNode, with_type_parameters, tree_mapreduce
using ..EvaluateModule: eval_tree_array
using ..EvaluateDerivativeModule: eval_grad_tree_array

struct NodeTangent{T,N<:AbstractExpressionNode{T},A<:AbstractArray{T}} <: AbstractTangent
    tree::N
    gradient::A
end
function extract_gradient(gradient::NodeTangent, ::AbstractExpressionNode)
    return gradient.gradient
end
function Base.:+(a::NodeTangent, b::NodeTangent)
    # @assert a.tree == b.tree
    return NodeTangent(a.tree, a.gradient + b.gradient)
end
Base.:*(a::Number, b::NodeTangent) = NodeTangent(b.tree, a * b.gradient)
Base.:*(a::NodeTangent, b::Number) = NodeTangent(a.tree, a.gradient * b)
Base.zero(::Union{Type{NodeTangent},NodeTangent}) = ZeroTangent()

function CRC.rrule(
    ::typeof(eval_tree_array),
    tree::AbstractExpressionNode,
    X::AbstractMatrix,
    operators::OperatorEnum;
    kws...,
)
    primal, complete = eval_tree_array(tree, X, operators; kws...)

    if !complete
        primal .= NaN
    end

    return (primal, complete), EvalPullback(tree, X, operators)
end

# Wrap in struct rather than closure to ensure variables are boxed
struct EvalPullback{N,A,O} <: Function
    tree::N
    X::A
    operators::O
end

# TODO: Preferable to use the primal in the pullback somehow
function (e::EvalPullback)((dY, _))
    _, dX_constants_dY, complete = eval_grad_tree_array(
        e.tree, e.X, e.operators; variable=Val(:both)
    )

    if !complete
        dX_constants_dY .= NaN
    end

    nfeatures = size(e.X, 1)
    dX_dY = @view dX_constants_dY[1:nfeatures, :]
    dconstants_dY = @view dX_constants_dY[(nfeatures + 1):end, :]

    dtree = NodeTangent(
        e.tree, sum(j -> dconstants_dY[:, j] * dY[j], eachindex(axes(dconstants_dY, 2)))
    )

    dX = dX_dY .* reshape(dY, 1, size(dconstants_dY, 2))

    return (NoTangent(), dtree, dX, NoTangent())
end

end
