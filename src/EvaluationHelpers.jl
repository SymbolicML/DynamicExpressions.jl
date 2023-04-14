module EvaluationHelpersModule

import Base: adjoint
import ..OperatorEnumModule: AbstractOperatorEnum, OperatorEnum, GenericOperatorEnum
import ..EquationModule: Node
import ..EvaluateEquationModule: eval_tree_array
import ..EvaluateEquationDerivativeModule: eval_grad_tree_array

# Evaluation:
"""
    (tree::Node)(X::AbstractMatrix{T}, operators::OperatorEnum; turbo::Bool=false)

Evaluate a binary tree (equation) over a given data matrix. The
operators contain all of the operators used in the tree.

# Arguments
- `X::AbstractMatrix{T}`: The input data to evaluate the tree on.
- `operators::OperatorEnum`: The operators used in the tree.
- `turbo::Bool`: Use `LoopVectorization.@turbo` for faster evaluation.

# Returns
- `output::AbstractVector{T}`: the result, which is a 1D array.
    Any NaN, Inf, or other failure during the evaluation will result in the entire
    output array being set to NaN.
"""
function (tree::Node)(X, operators::OperatorEnum; kws...)
    out, did_finish = eval_tree_array(tree, X, operators; kws...)
    !did_finish && (out .= convert(eltype(out), NaN))
    return out
end
"""
    (tree::Node)(X::AbstractMatrix, operators::GenericOperatorEnum; throw_errors::Bool=true)

# Arguments
- `X::AbstractArray`: The input data to evaluate the tree on.
- `operators::GenericOperatorEnum`: The operators used in the tree.
- `throw_errors::Bool=true`: Whether to throw errors
    if they occur during evaluation. Otherwise,
    MethodErrors will be caught before they happen and 
    evaluation will return `nothing`,
    rather than throwing an error. This is useful in cases
    where you are unsure if a particular tree is valid or not,
    and would prefer to work with `nothing` as an output.

# Returns
- `output`: the result of the evaluation.
    If evaluation failed, `nothing` will be returned for the first argument.
    A `false` complete means an operator was called on input types
    that it was not defined for. You can change this behavior by
    setting `throw_errors=false`.
"""
function (tree::Node)(X, operators::GenericOperatorEnum; kws...)
    out, did_finish = eval_tree_array(tree, X, operators; kws...)
    !did_finish && return nothing
    return out
end
function (tree::Node)(X; kws...)
    ## This will be overwritten by OperatorEnumConstructionModule, and turned
    ## into a depwarn.
    @error "The `tree(X; kws...)` syntax is deprecated. Use `tree(X, operators; kws...)` instead."
end

# Gradients:
function _grad_evaluator(tree::Node, X, operators::OperatorEnum; kws...)
    _, grad, did_complete = eval_grad_tree_array(tree, X, operators; variable=true, kws...)
    !did_complete && (grad .= convert(eltype(grad), NaN))
    return grad
end
function _grad_evaluator(tree::Node, X, operators::GenericOperatorEnum; kws...)
    @error "Gradients are not implemented for `GenericOperatorEnum`."
end
function _grad_evaluator(tree::Node, X; kws...)
    ## This will be overwritten by OperatorEnumConstructionModule, and turned
    ## into a depwarn
    @error "The `tree'(X; kws...)` syntax is deprecated. Use `tree'(X, operators; kws...)` instead."
end

"""
    (tree::Node{T})'(X::AbstractMatrix{T}, operators::OperatorEnum; turbo::Bool=false, variable::Bool=true)

Compute the forward-mode derivative of an expression, using a similar
structure and optimization to eval_tree_array. `variable` specifies whether
we should take derivatives with respect to features (i.e., X), or with respect
to every constant in the expression.

# Arguments
- `X::AbstractMatrix{T}`: The data matrix, with each column being a data point.
- `operators::OperatorEnum`: The operators used to create the `tree`. Note that `operators.enable_autodiff`
    must be `true`. This is needed to create the derivative operations.
- `variable::Bool`: Whether to take derivatives with respect to features (i.e., `X` - with `variable=true`),
    or with respect to every constant in the expression (`variable=false`).
- `turbo::Bool`: Use `LoopVectorization.@turbo` for faster evaluation.

# Returns

- `(evaluation, gradient, complete)::Tuple{AbstractVector{T}, AbstractMatrix{T}, Bool}`: the normal evaluation,
    the gradient, and whether the evaluation completed as normal (or encountered a nan or inf).
"""
Base.adjoint(tree::Node) = ((args...; kws...) -> _grad_evaluator(tree, args...; kws...))

end
