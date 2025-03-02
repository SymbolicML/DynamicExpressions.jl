module SpecialOperatorsModule

using ..OperatorEnumModule: OperatorEnum
using ..EvaluateModule:
    _eval_tree_array, @return_on_nonfinite_array, deg2_eval, ResultOk, get_filled_array
using ..ExpressionModule: AbstractExpression
using ..ExpressionAlgebraModule: @declare_expression_operator

import ..EvaluateModule:
    special_operator, deg2_eval_special, deg1_eval_special, any_special_operators
import ..StringsModule: get_op_name

function any_special_operators(::Union{O,Type{O}}) where {B,U,O<:OperatorEnum{B,U}}
    return any(special_operator, B.types) || any(special_operator, U.types)
end

# Use this to customize evaluation behavior for operators:
@inline special_operator(::Type) = false
@inline special_operator(f) = special_operator(typeof(f))

Base.@kwdef struct AssignOperator <: Function
    target_register::Int
end
@declare_expression_operator((op::AssignOperator), 1)
@inline special_operator(::Type{AssignOperator}) = true
get_op_name(o::AssignOperator) = "[{FEATURE_" * string(o.target_register) * "} =]"

function deg1_eval_special(tree, cX, op::AssignOperator, eval_options, operators)
    result = _eval_tree_array(tree.l, cX, operators, eval_options)
    !result.ok && return result
    @return_on_nonfinite_array(eval_options, result.x)
    target_register = op.target_register
    @inbounds @simd for i in eachindex(axes(cX, 2))
        cX[target_register, i] = result.x[i]
    end
    return result
end

Base.@kwdef struct WhileOperator <: Function
    max_iters::Int = 100
end

@declare_expression_operator((op::WhileOperator), 2)
@inline special_operator(::Type{WhileOperator}) = true
get_op_name(o::WhileOperator) = "while"

# TODO: Need to void any instance of buffer when using while loop.
function deg2_eval_special(tree, cX, op::WhileOperator, eval_options, operators)
    cond = tree.l
    body = tree.r
    mask = trues(size(cX, 2))
    X = @view cX[:, mask]
    # Initialize the result array for all columns
    result_array = get_filled_array(eval_options.buffer, zero(eltype(cX)), cX, axes(cX, 2))
    body_result = ResultOk(result_array, true)

    for _ in 1:(op.max_iters)
        cond_result = _eval_tree_array(cond, X, operators, eval_options)
        !cond_result.ok && return cond_result
        @return_on_nonfinite_array(eval_options, cond_result.x)

        new_mask = cond_result.x .> 0.0
        any(new_mask) || return body_result

        # Track which columns are still active
        mask[mask] .= new_mask
        X = @view cX[:, mask]

        # Evaluate just for active columns
        iter_result = _eval_tree_array(body, X, operators, eval_options)
        !iter_result.ok && return iter_result

        # Update the corresponding elements in the result array
        body_result.x[mask] .= iter_result.x
        @return_on_nonfinite_array(eval_options, body_result.x)
    end

    # We passed max_iters, so this result is invalid
    return ResultOk(body_result.x, false)
end

end
