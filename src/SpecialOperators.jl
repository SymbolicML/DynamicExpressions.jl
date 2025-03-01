module SpecialOperatorsModule

using ..OperatorEnumModule: OperatorEnum
using ..EvaluateModule: _eval_tree_array, @return_on_nonfinite_array, deg2_eval

import ..EvaluateModule:
    special_operator, deg2_eval_special, deg1_eval_special, any_special_operators

function any_special_operators(::Type{OperatorEnum{B,U}}) where {B,U}
    return any(special_operator, B.types) || any(special_operator, U.types)
end

# Use this to customize evaluation behavior for operators:
@inline special_operator(::Type) = false
@inline special_operator(f) = special_operator(typeof(f))

# Base.@kwdef struct WhileOperator <: Function
#     max_iters::Int = 100
# end
Base.@kwdef struct AssignOperator <: Function
    target_register::Int
end

# @inline special_operator(::Type{WhileOperator}) = true
@inline special_operator(::Type{AssignOperator}) = true

# function deg2_eval_special(tree, cX, op::WhileOperator, eval_options)
#     cond = tree.l
#     body = tree.r
#     for _ in 1:(op.max_iters)
#         let cond_result = _eval_tree_array(cond, cX, operators, eval_options)
#             !cond_result.ok && return cond_result
#             @return_on_nonfinite_array(eval_options, cond_result.x)
#         end
#         let body_result = _eval_tree_array(body, cX, operators, eval_options)
#             !body_result.ok && return body_result
#             @return_on_nonfinite_array(eval_options, body_result.x)
#             # TODO: Need to somehow mask instances 
#         end
#     end

#     return get_filled_array(eval_options.buffer, zero(eltype(cX)), cX, axes(cX, 2))
# end
# TODO: Need to void any instance of buffer when using while loop.

function deg1_eval_special(tree, cX, op::AssignOperator, eval_options)
    result = _eval_tree_array(tree.l, cX, operators, eval_options)
    !result.ok && return result
    @return_on_nonfinite_array(eval_options, result.x)
    target_register = op.target_register
    @inbounds @simd for i in eachindex(axes(cX, 2))
        cX[target_register, i] = result.x[i]
    end
    return result
end

end 