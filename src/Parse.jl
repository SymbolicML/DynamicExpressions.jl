module ParseModule

using ArgCheck: @argcheck

using ..NodeModule: AbstractExpressionNode, Node
using ..OperatorEnumModule: AbstractOperatorEnum
using ..OperatorEnumConstructionModule: empty_all_globals!
using ..ExpressionModule: AbstractExpression, Expression

"""
    @parse_expression(expr; operators, variable_names, node_type=Node, evaluate_on=[])

Parse a symbolic expression `expr` into a computational graph where nodes represent operations or variables.

## Arguments

- `expr`: An expression to parse into an `AbstractExpression`.

## Keyword Arguments

- `operators`: An instance of `OperatorEnum` specifying the available unary and binary operators.
- `variable_names`: A list of variable names as strings or symbols that are allowed in the expression.
- `evaluate_on`: A list of external functions to evaluate explicitly when encountered.
- `node_type`: The type of the nodes in the resulting expression tree. Defaults to `Node`.

## Usage

The macro is used to convert a high-level symbolic expression into a structured expression tree that can be manipulated or evaluated. Here are some examples of how to use `parse_expression`:

### Parsing from a custom operator

```julia
julia> my_custom_op(x, y) = x + y^3;

julia> operators = OperatorEnum(binary_operators=[+, -, *, my_custom_op], unary_operators=[sin]);

julia> ex = @parse_expression my_custom_op(x, sin(y) + 0.3) operators=operators variable_names=["x", "y"]
my_custom_op(x, sin(y) + 0.3)

julia> typeof(ex)
Expression{Float64, Node{Float64}, OperatorEnum{Tuple{typeof(+), typeof(-), typeof(*), typeof(my_custom_op)}, Tuple{typeof(sin)}}, Vector{String}}

julia> typeof(ex.tree)
Node{Float64}

julia> ex(ones(2, 1))
1-element Vector{Float64}:
 2.487286478935302
```

### Handling expressions with symbolic variable names

```julia
julia> ex = @parse_expression(
            cos(exp(α - 1)),
            operators=OperatorEnum(binary_operators=[-], unary_operators=[cos, exp]),
            variable_names=[:α],
            node_type=GraphNode
        )
cos(exp(α))

julia> typeof(ex.tree)
GraphNode{Float32}
```

### Using external functions and variables

```
julia> c = 5.0
5.0

julia> show_type(x) = (@show typeof(x); x);

julia> ex = @parse_expression(
           c * 2.5 - show_type(cos(x)),
           operators = OperatorEnum(; binary_operators=[*, -], unary_operators=[cos]),
           variable_names = [:x],
           evaluate_on = [show_type],
       )
typeof(x) = Node{Float32}
(5.0 * 2.5) - cos(x)
```
"""
macro parse_expression(ex, kws...)
    # Initialize default values for operators and variable_names
    operators = nothing
    variable_names = nothing
    node_type = Node
    evaluate_on = nothing

    # Iterate over keyword arguments to extract operators and variable_names
    for kw in kws
        if kw.head == :(=)
            if kw.args[1] == :operators
                operators = kw.args[2]
            elseif kw.args[1] == :variable_names
                variable_names = kw.args[2]
            elseif kw.args[1] == :node_type
                node_type = kw.args[2]
            elseif kw.args[1] == :evaluate_on
                evaluate_on = kw.args[2]
            end
        end
    end

    # Ensure that operators and variable_names are provided
    @argcheck operators !== nothing "The 'operators' keyword argument must be provided."
    @argcheck variable_names !== nothing "The 'variable_names' keyword argument must be provided."

    # We want to expand the expression in the calling module to parse the functions
    # correctly.
    # We also evaluate all expressions in a `let` to ensure internal
    # variable names don't collide.
    calling_module = __module__
    return esc(
        quote
            let (ex, operators, s_variable_names, node_type, evaluate_on) = (
                    $(Meta.quot(ex)), $operators, $variable_names, $node_type, $evaluate_on
                )
                variable_names = if eltype(s_variable_names) isa AbstractString
                    s_variable_names
                else
                    string.(s_variable_names)
                end
                tree = $(parse_expression)(
                    ex, operators, variable_names, node_type, evaluate_on, $calling_module
                )
                $(Expression)(tree, (; operators, variable_names))
            end
        end,
    )
end

"""Parse an expression into a Node from a Julia expression."""
function parse_expression(
    ex::Expr,
    operators::AbstractOperatorEnum,
    variable_names::AbstractVector,
    ::Type{N},
    evaluate_on::Union{Nothing,AbstractVector},
    calling_module,
) where {N<:AbstractExpressionNode}
    empty_all_globals!()
    return _parse_expression(ex, operators, variable_names, N, evaluate_on, calling_module)
end

function _parse_expression(
    ex::Expr,
    operators::AbstractOperatorEnum,
    variable_names::AbstractVector,
    ::Type{N},
    evaluate_on::Union{Nothing,AbstractVector},
    calling_module,
) where {N<:AbstractExpressionNode}
    head = ex.head
    args = ex.args
    if head == :call
        func = try
            Core.eval(calling_module, first(ex.args))
        catch
            throw(
                ArgumentError(
                    "Failed to evaluate function `$(first(ex.args))` within `$(calling_module)`. " *
                    "Make sure the function is defined in that module.",
                ),
            )
            () -> ()
        end
        if length(args) == 2 && (op = findfirst(==(func), operators.unaops)) !== nothing
            # Regular unary operator
            return N(;
                op=op::Int,
                l=_parse_expression(
                    args[2], operators, variable_names, N, evaluate_on, calling_module
                ),
            )
        elseif length(args) == 3 && (op = findfirst(==(func), operators.binops)) !== nothing
            # Regular binary operator
            return N(;
                op=op::Int,
                l=_parse_expression(
                    args[2], operators, variable_names, N, evaluate_on, calling_module
                ),
                r=_parse_expression(
                    args[3], operators, variable_names, N, evaluate_on, calling_module
                ),
            )
        elseif length(args) > 3 &&
            func in (+, -) &&
            (op = findfirst(==(func), operators.binops)) !== nothing
            # Either + or - but used with more than two arguments
            inner = N(;
                op=op::Int,
                l=_parse_expression(
                    args[2], operators, variable_names, N, evaluate_on, calling_module
                ),
                r=_parse_expression(
                    args[3], operators, variable_names, N, evaluate_on, calling_module
                ),
            )
            for arg in args[4:end]
                inner = N(;
                    op=op::Int,
                    l=inner,
                    r=_parse_expression(
                        arg, operators, variable_names, N, evaluate_on, calling_module
                    ),
                )
            end
            return inner
        elseif evaluate_on !== nothing && func in evaluate_on
            # External function
            func(
                map(
                    arg -> _parse_expression(
                        arg, operators, variable_names, N, evaluate_on, calling_module
                    ),
                    args[2:end],
                )...,
            )
        else
            matching_s = let
                s = if length(args) == 2
                    "`" * string(operators.unaops) * "`"
                elseif length(args) == 3
                    "`" * string(operators.binops) * "`"
                else
                    ""
                end
                if evaluate_on !== nothing
                    if length(s) > 0
                        s *= " or " * "`" * string(evaluate_on) * "`"
                    else
                        s *= "`" * string(evaluate_on) * "`"
                    end
                end
                s
            end
            throw(
                ArgumentError(
                    "Unrecognized operator: `$(func)` with no matches in $(matching_s). " *
                    "If you meant to call an external function, please pass the function to the `evaluate_on` keyword argument.",
                ),
            )
            N()
        end
    else
        @argcheck head in (:vect, :tuple)
        return N(; val=Core.eval(calling_module, ex))
    end
end
function _parse_expression(
    ex::Symbol,
    operators::AbstractOperatorEnum,
    variable_names::AbstractVector,
    ::Type{N},
    evaluate_on::Union{Nothing,AbstractVector},
    calling_module,
) where {N<:AbstractExpressionNode}
    i = findfirst(==(string(ex)), variable_names)
    if i !== nothing
        return N(; feature=i)
    else
        # If symbol not found in variable_names, then try interpolating
        evaluated = Core.eval(calling_module, ex)
        return _parse_expression(
            evaluated, operators, variable_names, N, evaluate_on, calling_module
        )
    end
end
function _parse_expression(
    val,
    ::AbstractOperatorEnum,
    ::AbstractVector,
    ::Type{N},
    ::Union{Nothing,AbstractVector},
    _,
) where {N<:AbstractExpressionNode}
    return N(; val)
end

end
