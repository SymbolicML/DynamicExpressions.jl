module NonDifferentiableDeclarationsModule

using ChainRulesCore: @non_differentiable
import ..OperatorEnumModule: AbstractOperatorEnum
import ..NodeModule: AbstractExpressionNNode, AbstractNNode
import ..NodeUtilsModule: tree_mapreduce
import ..ExpressionModule:
    AbstractExpression, get_operators, get_variable_names, _validate_input

#! format: off
@non_differentiable tree_mapreduce(f::Function, op::Function, tree::AbstractNNode, result_type::Type)
@non_differentiable tree_mapreduce(f::Function, f_branch::Function, op::Function, tree::AbstractNNode, result_type::Type)
@non_differentiable get_operators(ex::Union{AbstractExpression,AbstractExpressionNNode}, operators::Union{AbstractOperatorEnum,Nothing})
@non_differentiable get_variable_names(ex::AbstractExpression, variable_names::Union{AbstractVector{<:AbstractString},Nothing})
@non_differentiable _validate_input(ex::AbstractExpression, X, operators::Union{AbstractOperatorEnum,Nothing})
#! format: on

end
