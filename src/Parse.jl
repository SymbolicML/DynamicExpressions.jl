module ParseModule

using ..NodeModule: AbstractExpressionNode, Node
using ..OperatorEnumModule: AbstractOperatorEnum
using ..ExpressionModule: AbstractExpression, Expression

macro parse_expression(ex, kws...)
    # Initialize default values for operators and variable_names
    operators = nothing
    variable_names = nothing
    node_type = Node

    # Iterate over keyword arguments to extract operators and variable_names
    for kw in kws
        if kw.head == :(=)
            if kw.args[1] == :operators
                operators = kw.args[2]
            elseif kw.args[1] == :variable_names
                variable_names = kw.args[2]
            elseif kw.args[1] == :node_type
                node_type = kw.args[2]
            end
        end
    end

    # Ensure that operators and variable_names are provided
    @assert operators !== nothing "The 'operators' keyword argument must be provided."
    @assert variable_names !== nothing "The 'variable_names' keyword argument must be provided."

    # We want to expand the expression in the calling module to parse the functions
    # correctly
    calling_module = __module__
    return esc(
        quote
            let
                node = $(_parse_expression)(
                    $(Meta.quot(ex)),
                    $operators,
                    $variable_names,
                    $node_type,
                    $calling_module,
                )
                $(Expression)(node, $operators, $variable_names)
            end
        end,
    )
end

"""Parse an expression into a Node from a Julia expression."""
function _parse_expression(
    ex::Expr,
    operators::AbstractOperatorEnum,
    variable_names::AbstractVector,
    ::Type{N},
    calling_module,
) where {N<:AbstractExpressionNode}
    head = ex.head
    args = ex.args
    if head == :call
        length(args) ∈ (2, 3) || throw(
            ArgumentError(
                "`parse_expression` only parses 1- or 2-argument operators but got $(ex)",
            ),
        )
        func = Core.eval(calling_module, first(ex.args))
        if length(args) == 2
            op = findfirst(==(func), operators.unaops)
            op === nothing && throw(
                ArgumentError(
                    "Unrecognized operator: $(func) with no matches in enum $(operators.unaops).",
                ),
            )
            return N(;
                op=op::Int,
                l=_parse_expression(args[2], operators, variable_names, N, calling_module),
            )
        else
            op = findfirst(==(func), operators.binops)
            op === nothing && throw(
                ArgumentError(
                    "Unrecognized operator: $(func) with no matches in enum $(operators.binops).",
                ),
            )
            return N(;
                op=op::Int,
                l=_parse_expression(args[2], operators, variable_names, N, calling_module),
                r=_parse_expression(args[3], operators, variable_names, N, calling_module),
            )
        end
    end
end
function _parse_expression(
    ex::Symbol, ::AbstractOperatorEnum, variable_names::AbstractVector, ::Type{N}, _
) where {N<:AbstractExpressionNode}
    return N(ex, variable_names)
end
function _parse_expression(
    val, ::AbstractOperatorEnum, ::AbstractVector, ::Type{N}, _
) where {N<:AbstractExpressionNode}
    return N(; val)
end

end
