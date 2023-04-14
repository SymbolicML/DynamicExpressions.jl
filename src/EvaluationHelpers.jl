module EvaluationHelpersModule

import Base: adjoint
import ..OperatorEnumModule: AbstractOperatorEnum, OperatorEnum, GenericOperatorEnum
import ..EquationModule: Node
import ..EvaluateEquationModule: eval_tree_array
import ..EvaluateEquationDerivativeModule: eval_grad_tree_array

# Evaluation:
function (tree::Node)(X, operators::OperatorEnum; kws...)
    out, did_finish = eval_tree_array(tree, X, operators; kws...)
    !did_finish && (out .= convert(eltype(out), NaN))
    return out
end
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
Base.adjoint(tree::Node) = ((args...; kws...) -> _grad_evaluator(tree, args...; kws...))

end
