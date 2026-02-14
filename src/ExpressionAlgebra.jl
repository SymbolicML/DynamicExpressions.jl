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
    declare_operator_alias(op, ::Val{arity})

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
declare_operator_alias(op::F, _) where {F} = op

allow_chaining(@nospecialize(op)) = false
allow_chaining(::typeof(+)) = true
allow_chaining(::typeof(*)) = true

function apply_operator(op::F, args::Vararg{Any,D}) where {F<:Function,D}
    idx = findfirst(e -> e isa AbstractExpression, args)::Int
    example_expr = args[idx]
    E = typeof(example_expr)
    @assert all(e -> !(e isa AbstractExpression) || typeof(e) === E, args)
    operators = get_operators(example_expr, nothing)

    op_idx = if length(operators) >= D
        findfirst(==(op), map(Base.Fix2(declare_operator_alias, Val(D)), operators[D]))
    else
        nothing
    end
    if isnothing(op_idx)
        if allow_chaining(op) && D > 2
            # These operators might get chained by Julia, so we check
            # downward for any matching arity.
            inner = apply_operator(op, args[1:(end - 1)]...)
            return apply_operator(op, inner, args[end])
        end
        throw(
            MissingOperatorError(
                "Operator $op not found in operators for expression type " *
                "$(E) with $(D)-degree operators $(operators[D])",
            ),
        )
    end
    return insert_operator_index(op_idx, args, example_expr)
end

"""
    @declare_expression_operator(op, arity)

Declare an operator function for `AbstractExpression` types.

This macro generates methods for the given operator `op` that work with
`AbstractExpression` arguments.  The `arity` parameter specifies the number
of arguments the operator takes.
"""
macro declare_expression_operator(op, arity)
    syms = [Symbol('x', i) for i in 1:arity]
    AE = :($(AbstractExpression))
    if arity == 1
        return esc(
            quote
                $op($(only(syms))::$(AE)) = $(apply_operator)($op, $(only(syms)))
            end,
        )
    end

    wrappers = (AE, :($(AE){T}), :T, :Number)
    methods = Expr(:block)

    for types in Iterators.product(ntuple(_ -> wrappers, arity)...)
        has_expr = any(
            t -> t == AE || (t isa Expr && t.head == :curly && t.args[1] == AE), types
        )
        has_plain_T = any(==(:T), types)
        has_abstract_expr_T = any(
            t -> t isa Expr && t.head == :curly && t.args[1] == AE && :T in t.args, types
        )
        has_abstract_expr_plain = any(==(AE), types)
        if any((
            !has_expr,
            # ^At least one arg must be an AbstractExpression (avoid type‑piracy)
            has_abstract_expr_plain && has_abstract_expr_T,
            # ^If a plain `T` appears, ensure an `AbstractExpression{T}` is also present
            has_plain_T ⊻ has_abstract_expr_T,
            # ^Do not mix bare `AbstractExpression` with `AbstractExpression{T}`
        ))
            continue
        end

        arglist = [Expr(:(::), syms[i], types[i]) for i in 1:arity]
        signature = Expr(:call, op, arglist...)
        if any(t -> t == :T || (t isa Expr && t.head == :curly && :T in t.args), types)
            signature = Expr(:where, signature, :(T))
        end

        body = Expr(:block, :(return $(apply_operator)($op, $(syms...))))

        fn = Expr(:function, signature, body)

        push!(methods.args, fn)
    end

    return esc(methods)
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
for op in (
    :*, :+, :clamp, :max, :min, :fma, :muladd,
)
    @eval @declare_expression_operator Base.$(op) 3
end
#! format: on

end
