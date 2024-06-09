module ExpressionMathModule

using DispatchDoctor
using ..NodeModule: constructorof
using ..OperatorEnumModule: AbstractOperatorEnum
using ..ExpressionModule: AbstractExpression, get_operators, get_tree, with_tree

for binop in (:+, :-, :*, :/, :^, :max, :min, :copysign, :atan, :mod)
    @eval function Base.$binop(x::E, y::E) where {T,N,E<:AbstractExpression{T,N}}
        i = findfirst(==($(binop)), get_operators(x, nothing).binops)
        if i === nothing
            error("Operator $($binop) not found in `binary_operators` of expression $(x)")
        end
        t = constructorof(N)(; op=i, l=get_tree(x), r=get_tree(y))
        return with_tree(x, t)
    end
end
for unaop in (
    :exp,
    :abs,
    :log,
    :log10,
    :log2,
    :log1p,
    :sqrt,
    :sin,
    :cos,
    :tan,
    :sinh,
    :cosh,
    :tanh,
    :atan,
    :asinh,
    :acosh,
    :atanh,
    :round,
    :floor,
    :ceil,
    :sign,
)
    @eval function Base.$unaop(x::E) where {T,N,E<:AbstractExpression{T,N}}
        i = findfirst(==($(unaop)), get_operators(x, nothing).unaops)
        if i === nothing
            error("Operator $($unaop) not found in `unary_operators` of expression $(x)")
        end
        t = constructorof(N)(; op=i, l=get_tree(x))
        return with_tree(x, t)
    end
end

end
