module ExpressionAlgebraModule

using ..NodeModule: AbstractExpressionNode
using ..ExpressionModule:
    AbstractExpression,
    get_operators,
    get_contents,
    get_metadata,
    with_contents,
    constructorof

function insert_operator_index(
    op::Integer, exprs::Tuple, example_expr::E
) where {T,N<:AbstractExpressionNode{T},E<:AbstractExpression{T,N}}
    _exprs = map(exprs) do expr
        if expr isa AbstractExpression
            # Assume the contents are an expression; otherwise, this
            # needs a custom method!
            expr
        else
            with_contents(copy(example_expr), constructorof(N)(T; val=expr)::N)
        end
    end
    trees = map(get_contents, _exprs)::NTuple{length(_exprs),N}
    output_tree = constructorof(N)(; children=trees, op)::N
    return with_contents(first(_exprs), output_tree)
end

struct MissingOperatorError
    msg::String
end

function Base.showerror(io::IO, e::MissingOperatorError)
    return print(io, e.msg)
end

"""
    declare_operator_alias(op::Function, ::Val{arity})::Function

Define how an internal operator should be matched against user-provided operators in expression trees.

By default, operators match themselves. Override this method to specify that an internal operator
should match a different operator when searching the operator lists in expressions.

For example, to make `safe_sqrt` match `sqrt` user-space:

```julia
DynamicExpressions.declare_operator_alias(safe_sqrt, Val(1)) = sqrt
```

Which would allow a user to write `sqrt(x::Expression)`
and have it match the operator `safe_sqrt` stored in the binary operators
of the expression.
"""
declare_operator_alias(op::F, _) where {F<:Function} = op

function apply_operator(op::F, l::AbstractExpression) where {F<:Function}
    operators = get_operators(l, nothing)
    op_idx = findfirst(
        ==(op), map(Base.Fix2(declare_operator_alias, Val(1)), operators.unaops)
    )
    if op_idx === nothing
        throw(
            MissingOperatorError(
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
    op_idx = findfirst(
        ==(op), map(Base.Fix2(declare_operator_alias, Val(2)), operators.binops)
    )
    if op_idx === nothing
        throw(
            MissingOperatorError(
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
        return esc(quote
            $op(l::AbstractExpression) = $(apply_operator)($op, l)
        end)
    elseif arity == 2
        return esc(quote
            function $op(l::AbstractExpression, r::AbstractExpression)
                return $(apply_operator)($op, l, r)
            end
            function $op(l::T, r::AbstractExpression{T}) where {T}
                return $(apply_operator)($op, l, r)
            end
            function $op(l::AbstractExpression{T}, r::T) where {T}
                return $(apply_operator)($op, l, r)
            end
            # Convenience methods for Number types
            function $op(l::Number, r::AbstractExpression{T}) where {T}
                return $(apply_operator)($op, l, r)
            end
            function $op(l::AbstractExpression{T}, r::Number) where {T}
                return $(apply_operator)($op, l, r)
            end
        end)
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
    :(!), :-, :+, :sign, :identity,
)
    @eval @declare_expression_operator Base.$(op) 1
end
for op in (
    :*, :/, :+, :-, :^, :÷, :mod, :log,
    :atan, :atand, :copysign, :flipsign,
    :&, :|, :⊻, ://, :\, :rem,
    :(>), :(<), :(>=), :(<=), :max, :min,
)
    @eval @declare_expression_operator Base.$(op) 2
end
#! format: on

end
