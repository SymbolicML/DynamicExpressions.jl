module ExpressionMathModule

using ..NodeModule: AbstractExpressionNode
using ..ExpressionModule:
    AbstractExpression,
    get_operators,
    get_contents,
    get_metadata,
    with_contents,
    constructorof

function insert_operator_index(
    op::Integer, exprs, example_expr::E
) where {T,N<:AbstractExpressionNode{T},E<:AbstractExpression{T,N}}
    _exprs = map(exprs) do expr
        if expr isa AbstractExpression
            # Assume the contents are an expression; otherwise, this
            # needs a custom method!
            expr
        else
            expr = with_contents(copy(example_expr), constructorof(N)(T; val=expr)::N)
        end
    end
    trees = map(t -> get_contents(t)::N, _exprs)
    output_tree = constructorof(N)(; children=trees, op)::N
    return with_contents(first(_exprs), output_tree)
end

function apply_operator(op::F, l::AbstractExpression) where {F<:Function}
    operators = get_operators(l, nothing)
    op_idx = findfirst(==(op), operators.unaops)
    if op_idx === nothing
        throw(
            ArgumentError(
                "Operator $op not found in operators for expression type $(typeof(l)) with unary operators $(operators.unaops)",
            ),
        )
    end
    return insert_operator_index(op_idx, (l,), l)
end
function apply_operator(op::F, l, r) where {F<:Function}
    (operators, example_expr) = if l isa AbstractExpression && r isa AbstractExpression
        @assert typeof(r) === typeof(l)
        (get_operators(l, nothing), l)
    elseif l isa AbstractExpression
        (get_operators(l, nothing), l)
    else
        r::AbstractExpression
        (get_operators(r, nothing), r)
    end
    op_idx = findfirst(==(op), operators.binops)
    if op_idx === nothing
        throw(
            ArgumentError(
                "Operator $op not found in operators for expression type $(typeof(l)) with binary operators $(operators.binops)",
            ),
        )
    end
    return insert_operator_index(op_idx, (l, r), example_expr)
end

"""
    @declare_expression_operator(op, arity)

Declare an operator function for `AbstractExpression` types.

This macro generates a method for the given operator `op` that works with
`AbstractExpression` arguments. The `arity` parameter specifies whether
the operator is unary (1) or binary (2).

# Arguments
- `op`: The operator to be declared (e.g., `Base.sin`, `Base.:+`).
- `arity`: The number of arguments the operator takes (1 for unary, 2 for binary).
"""
macro declare_expression_operator(op, arity)
    @assert arity ∈ (1, 2)
    if arity == 1
        return esc(
            quote
                $op(l::AbstractExpression) = $(apply_operator)($op, l)
            end,
        )
    elseif arity == 2
        return esc(
            quote
                function $op(l::AbstractExpression, r::AbstractExpression)
                    return $(apply_operator)($op, l, r)
                end
                function $op(l::T, r::AbstractExpression{T}) where {T}
                    return $(apply_operator)($op, l, r)
                end
                function $op(l::AbstractExpression{T}, r::T) where {T}
                    return $(apply_operator)($op, l, r)
                end
            end,
        )
    end
end

#! format: off
for op in (
    :sin, :cos, :tan, :sinh, :cosh, :tanh, :asin, :acos,
    :asinh, :acosh, :atanh, :sec, :csc, :cot, :asec, :acsc, :acot, :sech, :csch,
    :coth, :asech, :acsch, :acoth, :sinc, :cosc, :cosd, :cotd, :cscd, :secd,
    :sinpi, :cospi, :sind, :tand, :acosd, :acotd, :acscd, :asecd, :asind,
    :log, :log2, :log10, :log1p, :exp, :exp2, :exp10, :expm1, :frexp, :exponent,
    :float, :abs, :real, :imag, :conj, :unsigned,
    :nextfloat, :prevfloat, :transpose, :significand,
    :modf, :rem, :floor, :ceil, :round, :trunc,
    :inv, :sqrt, :cbrt, :abs2, :angle, :factorial,
    :(!), :-, :+, :sign,
)
    @eval @declare_expression_operator Base.$(op) 1
end
for op in (
    :*, :/, :+, :-, :^, :÷, :mod,
    :atan, :atand, :copysign, :flipsign,
    :&, :|, :⊻, ://, :\,
)
    @eval @declare_expression_operator Base.$(op) 2
end
#! format: on

end
