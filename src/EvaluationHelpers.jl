module EvaluationHelpersModule

import Base: adjoint
import ..OperatorEnumModule: AbstractOperatorEnum, OperatorEnum, GenericOperatorEnum
import ..NodeModule: AbstractExpressionNode
import ..EvaluateModule: eval_tree_array
import ..EvaluateDerivativeModule: eval_grad_tree_array

# Evaluation:
"""
    (tree::AbstractExpressionNode)(X, operators::OperatorEnum; kws...)

Evaluate a binary tree (equation) over a given input data matrix. The
operators contain all of the operators used. This function fuses doublets
and triplets of operations for lower memory usage.

# Arguments
- `tree::AbstractExpressionNode`: The root node of the tree to evaluate.
- `X::AbstractMatrix{T}`: The input data to evaluate the tree on, with shape `[num_features, num_rows]`.
- `operators::OperatorEnum`: The operators used in the tree.
- `kws...`: Passed to `eval_tree_array`.

# Returns
- `output::AbstractVector{T}`: the result, which is a 1D array.
    Any NaN, Inf, or other failure during the evaluation will result in the entire
    output array being set to NaN.
"""
function (tree::AbstractExpressionNode)(X, operators::OperatorEnum; kws...)
    out, did_finish = eval_tree_array(tree, X, operators; kws...)
    !did_finish && (out .= convert(eltype(out), NaN))
    return out
end
"""
    (tree::AbstractExpressionNode)(X, operators::GenericOperatorEnum; kws...)

# Arguments
- `X::AbstractArray`: The input data to evaluate the tree on.
- `operators::GenericOperatorEnum`: The operators used in the tree.
- `kws...`: Passed to `eval_tree_array`.

# Returns
- `output`: the result of the evaluation.
    If evaluation failed, `nothing` will be returned for the first argument.
    A `false` complete means an operator was called on input types
    that it was not defined for. You can change this behavior by
    setting `throw_errors=false`.
"""
function (tree::AbstractExpressionNode)(X, operators::GenericOperatorEnum; kws...)
    out, did_finish = eval_tree_array(tree, X, operators; kws...)
    !did_finish && return nothing
    return out
end

# Gradients:
function _grad_evaluator(
    tree::AbstractExpressionNode, X, operators::OperatorEnum; variable=Val(true), kws...
)
    _, grad, did_complete = eval_grad_tree_array(tree, X, operators; variable, kws...)
    !did_complete && (grad .= convert(eltype(grad), NaN))
    return grad
end
function _grad_evaluator(
    tree::AbstractExpressionNode, X, operators::GenericOperatorEnum; kws...
)
    return error("Gradients are not implemented for `GenericOperatorEnum`.")
end

"""
    (tree::AbstractExpressionNode{T})'(X::AbstractMatrix{T}, operators::OperatorEnum; turbo::Union{Bool,Val}=Val(false), variable::Union{Bool,Val}=Val(true))

Compute the forward-mode derivative of an expression, using a similar
structure and optimization to eval_tree_array. `variable` specifies whether
we should take derivatives with respect to features (i.e., X), or with respect
to every constant in the expression.

# Arguments
- `X::AbstractMatrix{T}`: The data matrix, with each column being a data point.
- `operators::OperatorEnum`: The operators used to create the `tree`.
- `variable::Union{Bool,Val}`: Whether to take derivatives with respect to features (i.e., `X` - with `variable=true`),
    or with respect to every constant in the expression (`variable=false`).
- `turbo::Union{Bool,Val}`: Use LoopVectorization.jl for faster evaluation. Currently this does not have
    any effect.

# Returns

- `(evaluation, gradient, complete)::Tuple{AbstractVector{T}, AbstractMatrix{T}, Bool}`: the normal evaluation,
    the gradient, and whether the evaluation completed as normal (or encountered a nan or inf).
"""
function Base.adjoint(tree::AbstractExpressionNode)
    ((args...; kws...) -> _grad_evaluator(tree, args...; kws...))
end

end
