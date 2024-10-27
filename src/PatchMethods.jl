module PatchMethodsModule

using DynamicExpressions: get_contents, with_contents
using ..OperatorEnumModule: AbstractOperatorEnum
using ..NodeModule: constructorof
using ..ExpressionModule: Expression, get_tree, get_operators
using ..ParametricExpressionModule: ParametricExpression
import ..SimplifyModule: combine_operators!, simplify_tree!

# Avoid implementing a generic version for these, as it is less likely to generalize
function combine_operators!(
    ex::Union{Expression{T,N},ParametricExpression{T,N}},
    operators::Union{AbstractOperatorEnum,Nothing}=nothing,
) where {T,N}
    return with_contents(
        ex, combine_operators!(get_contents(ex), get_operators(ex, operators))
    )
end
function simplify_tree!(
    ex::Union{Expression{T,N},ParametricExpression{T,N}},
    operators::Union{AbstractOperatorEnum,Nothing}=nothing,
) where {T,N}
    return with_contents(ex, simplify_tree!(get_contents(ex), get_operators(ex, operators)))
end

end
