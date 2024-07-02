module ExpressionMath

using ..ExpressionModule:
    AbstractExpression, get_operators, get_contents, with_contents, constructorof

#! format: off
for op in (
    :*, :/, :+, :-, :^, :÷, :mod,
    :atan, :atand, :copysign, :flipsign,
    :&, :|, :⊻, :xor, ://, :\,
)
    @eval function Base.$op(l::E, r::E) where {E<:AbstractExpression}
        operators = get_operators(l, nothing)
        op_idx = findfirst(==(Base.$op), operators.binops)
        if op_idx === nothing
            throw(ArgumentError("Operator $op not found in operators for expression type $E with binary operators $(operators.binops)"))
        end
        return apply_operator(op_idx, (l, r))
    end
end
for op in (
    :sin, :cos, :tan, :sinh, :cosh, :tanh, :asin, :acos,
    :asinh, :acosh, :atanh, :sec, :csc, :cot, :asec, :acsc, :acot, :sech, :csch,
    :coth, :asech, :acsch, :acoth, :sinc, :cosc, :cosd, :cotd, :cscd, :secd,
    :sinpi, :cospi, :sind, :tand, :acosd, :acotd, :acscd, :asecd, :asind,
    :log, :log2, :log10, :log1p, :exp, :exp2, :exp10, :expm1, :frexp, :exponent,
    :float, :abs, :real, :imag, :conj, :adjoint, :unsigned,
    :nextfloat, :prevfloat, :transpose, :significand,
    :modf, :rem, :floor, :ceil, :round, :trunc,
    :inv, :sqrt, :cbrt, :abs2, :angle, :factorial,
    :(!), :-, :+, :sign,
)
    @eval function Base.$op(l::AbstractExpression)
        operators = get_operators(l, nothing)
        op_idx = findfirst(==(Base.$op), operators.unaops)
        if op_idx === nothing
            throw(ArgumentError("Operator $op not found in operators for expression type $(typeof(l)) with unary operators $(operators.unaryops)"))
        end
        return apply_operator(op_idx, (l,))
    end
end
#! format: on

function apply_operator(
    op::Integer, nodes::NTuple{num,E}
) where {num,T,N,E<:AbstractExpression{T,N}}
    trees = map(get_contents, nodes)
    output_tree = constructorof(N)(; children=trees, op)
    return with_contents(first(nodes), output_tree)
end

end
