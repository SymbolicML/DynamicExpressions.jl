module PatchMethodsModule

using ..OperatorEnumModule: AbstractOperatorEnum
using ..NodeModule: constructorof
using ..ExpressionModule: Expression, get_tree, get_operators
using ..ParametricExpressionModule: ParametricExpression
import ..SimplifyModule: combine_operators, simplify_tree!

# Avoid implementing a generic version for these, as it is less likely to generalize
function combine_operators(
    ex::Union{Expression{T,N},ParametricExpression{T,N}},
    operators::Union{AbstractOperatorEnum,Nothing}=nothing,
) where {T,N}
    return constructorof(typeof(ex))(
        combine_operators(get_tree(ex)::N, get_operators(ex, operators)), ex.metadata
    )
end
function simplify_tree!(
    ex::Union{Expression{T,N},ParametricExpression{T,N}},
    operators::Union{AbstractOperatorEnum,Nothing}=nothing,
) where {T,N}
    return constructorof(typeof(ex))(
        simplify_tree!(get_tree(ex)::N, get_operators(ex, operators)), ex.metadata
    )
end

end
