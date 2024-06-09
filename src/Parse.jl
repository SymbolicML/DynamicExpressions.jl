module ParseModule

using DispatchDoctor: @unstable

using ..NodeModule: AbstractExpressionNode, Node, constructorof
using ..OperatorEnumModule: AbstractOperatorEnum
using ..OperatorEnumConstructionModule: OperatorEnum, empty_all_globals!
using ..ExpressionModule:
    AbstractExpression,
    Expression,
    default_node,
    get_operators,
    get_variable_names,
    node_type

"""
    @parse_expression(expr; operators, variable_names, node_type=Node, evaluate_on=[])

(Experimental) Parse a symbolic expression `expr` into a computational graph where nodes represent operations or variables.

## Arguments

- `expr`: An expression to parse into an `AbstractExpression`.

## Keyword Arguments

- `operators`: An instance of `AbstractOperatorEnum` specifying the available unary and binary operators.
- `variable_names`: A list of variable names as strings or symbols that are allowed in the expression.
- `evaluate_on`: A list of external functions to evaluate explicitly when encountered.
- `expression_type`: The type of the resulting expression. Defaults to `Expression`.
- `node_type`: The type of the nodes in the resulting expression tree. Defaults to `default_node(expression_type)`.
- `binary_operators`: Convenience syntax for creating an `OperatorEnum`.
- `unary_operators`: Convenience syntax for creating an `OperatorEnum`.

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
    parsed_kws = _parse_kws(kws)
    return esc(
        :($(parse_expression)(
            $(Meta.quot(ex));
            operators=$(parsed_kws.operators),
            binary_operators=nothing,
            unary_operators=nothing,
            variable_names=$(parsed_kws.variable_names),
            node_type=$(parsed_kws.node_type),
            expression_type=$(parsed_kws.expression_type),
            evaluate_on=$(parsed_kws.evaluate_on),
            $(parsed_kws.extra_metadata)...,
        )),
    )
end

@unstable function _parse_kws(kws)
    # Initialize default values for operators and variable_names
    operators = nothing
    variable_names = nothing
    expression_type = Expression
    node_type = nothing
    evaluate_on = nothing
    extra_metadata = ()
    binops = nothing
    unaops = nothing

    # Iterate over keyword arguments to extract operators and variable_names
    for kw in kws
        if kw isa Symbol
            if kw == :operators
                operators = kw
                continue
            elseif kw == :variable_names
                variable_names = kw
                continue
            elseif kw == :node_type
                node_type = kw
                continue
            elseif kw == :expression_type
                expression_type = kw
                continue
            elseif kw == :evaluate_on
                evaluate_on = kw
                continue
            elseif kw == :extra_metadata
                extra_metadata = kw
                continue
            elseif kw == :binary_operators
                binops = kw
                continue
            elseif kw == :unary_operators
                unaops = kw
                continue
            end
        elseif kw isa Expr && kw.head == :(=)
            if kw.args[1] == :operators
                operators = kw.args[2]
                continue
            elseif kw.args[1] == :variable_names
                variable_names = kw.args[2]
                continue
            elseif kw.args[1] == :node_type
                node_type = kw.args[2]
                continue
            elseif kw.args[1] == :expression_type
                expression_type = kw.args[2]
                continue
            elseif kw.args[1] == :evaluate_on
                evaluate_on = kw.args[2]
                continue
            elseif kw.args[1] == :extra_metadata
                extra_metadata = kw.args[2]
                continue
            elseif kw.args[1] == :binary_operators
                binops = kw.args[2]
                continue
            elseif kw.args[1] == :unary_operators
                unaops = kw.args[2]
                continue
            end
        end
        throw(
            ArgumentError(
                "Unrecognized argument: `$kw`. The available arguments are `operators`, `variable_names`, `node_type`, `expression_type`, `evaluate_on`, and `extra_metadata`.",
            ),
        )
    end
    node_type = node_type === nothing ? :($(default_node)($expression_type)) : node_type

    if operators === nothing
        @assert(
            binops !== nothing || unaops !== nothing,
            "You must specify the operators using either `operators`, or `binary_operators` and `unary_operators`"
        )
        operators = :($(OperatorEnum)(;
            binary_operators=$(binops === nothing ? :(Function[]) : binops),
            unary_operators=$(unaops === nothing ? :(Function[]) : unaops),
        ))
    else
        @assert (binops === nothing && unaops === nothing)
    end

    return (;
        operators, variable_names, node_type, expression_type, evaluate_on, extra_metadata
    )
end

"""Parse an expression Julia `Expr` object."""
@unstable function parse_expression(
    ex;
    operators::Union{AbstractOperatorEnum,Nothing}=nothing,
    binary_operators::Union{Vector{<:Function},Nothing}=nothing,
    unary_operators::Union{Vector{<:Function},Nothing}=nothing,
    variable_names::Union{Vector,Nothing}=nothing,
    expression_type::Type{E}=Expression,
    node_type::Type{N}=default_node(expression_type),
    evaluate_on::Union{Nothing,AbstractVector}=nothing,
    kws...,
) where {N<:AbstractExpressionNode,E<:AbstractExpression}
    empty_all_globals!(; force=false)
    let variable_names = if variable_names === nothing
            nothing
        elseif eltype(variable_names) <: AbstractString
            variable_names
        else
            map(string, variable_names)
        end,
        operators = if operators === nothing
            OperatorEnum(; binary_operators, unary_operators)
        else
            operators
        end

        tree = _parse_expression(ex, operators, variable_names, N, E, evaluate_on; kws...)
        return constructorof(E)(tree; operators, variable_names, kws...)
    end
end
# function parse_expression(ex, prototype::AbstractExpression; evaluate_on=nothing)
#     operators = get_operators(prototype, nothing)
#     variable_names = get_variable_names(prototype, nothing)
#     E = typeof(prototype)
#     N = node_type(prototype)
#     return _parse_expression(ex, operators, variable_names, N, E; evaluate_on)
# end

"""An empty module for evaluation without collisions."""
module EmptyModule end

@unstable function _parse_expression(
    ex::Expr,
    operators::AbstractOperatorEnum,
    variable_names::Union{AbstractVector{<:AbstractString},Nothing},
    ::Type{N},
    ::Type{E},
    evaluate_on::Union{Nothing,AbstractVector};
    kws...,
) where {N<:AbstractExpressionNode,E<:AbstractExpression}
    ex.head != :call && throw(
        ArgumentError(
            "Unrecognized expression type: `Expr(:$(ex.head), ...)`. " *
            "Please only pass a function call or a variable.",
        ),
    )
    args = ex.args
    func = try
        Core.eval(EmptyModule, first(ex.args))
    catch
        throw(
            ArgumentError("Tried to interpolate function `$(first(ex.args))` but failed."),
        )
    end::Function
    return _parse_expression(
        func, args, operators, variable_names, N, E, evaluate_on; kws...
    )
end
@unstable function _parse_expression(
    func::F,
    args,
    operators::AbstractOperatorEnum,
    variable_names::Union{AbstractVector{<:AbstractString},Nothing},
    ::Type{N},
    ::Type{E},
    evaluate_on::Union{Nothing,AbstractVector};
    kws...,
)::N where {F<:Function,N<:AbstractExpressionNode,E<:AbstractExpression}
    if length(args) == 2 && func ∈ operators.unaops
        # Regular unary operator
        op = findfirst(==(func), operators.unaops)::Int
        return N(;
            op=op::Int,
            l=_parse_expression(
                args[2], operators, variable_names, N, E, evaluate_on; kws...
            ),
        )
    elseif length(args) == 3 && func ∈ operators.binops
        # Regular binary operator
        op = findfirst(==(func), operators.binops)::Int
        return N(;
            op=op::Int,
            l=_parse_expression(
                args[2], operators, variable_names, N, E, evaluate_on; kws...
            ),
            r=_parse_expression(
                args[3], operators, variable_names, N, E, evaluate_on; kws...
            ),
        )
    elseif length(args) > 3 && func in (+, -, *) && func ∈ operators.binops
        # Either + or - but used with more than two arguments
        op = findfirst(==(func), operators.binops)::Int
        inner = N(;
            op=op::Int,
            l=_parse_expression(
                args[2], operators, variable_names, N, E, evaluate_on; kws...
            ),
            r=_parse_expression(
                args[3], operators, variable_names, N, E, evaluate_on; kws...
            ),
        )
        for arg in args[4:end]
            inner = N(;
                op=op::Int,
                l=inner,
                r=_parse_expression(
                    arg, operators, variable_names, N, E, evaluate_on; kws...
                ),
            )
        end
        return inner
    elseif evaluate_on !== nothing && func in evaluate_on
        # External function
        func(
            map(
                arg -> _parse_expression(
                    arg, operators, variable_names, N, E, evaluate_on; kws...
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
    end
end
@unstable function _parse_expression(
    ex,
    operators::AbstractOperatorEnum,
    variable_names::Union{AbstractVector{<:AbstractString},Nothing},
    node_type::Type{<:AbstractExpressionNode},
    expression_type::Type{<:AbstractExpression},
    evaluate_on::Union{Nothing,AbstractVector};
    kws...,
)
    return parse_leaf(ex, variable_names, node_type, expression_type; kws...)
end

@unstable function parse_leaf(
    ex,
    variable_names,
    node_type::Type{<:AbstractExpressionNode},
    expression_type::Type{<:AbstractExpression};
    kws...,
)
    if ex isa AbstractExpression
        throw(
            ArgumentError(
                "Cannot parse an expression as a value in another expression. " *
                "Instead, you should unpack it into the tree (and make sure they " *
                "have the same metadata where relevant).",
            ),
        )
    end

    if ex isa Symbol
        i = variable_names === nothing ? nothing : findfirst(==(string(ex)), variable_names)
        if i === nothing
            throw(
                ArgumentError(
                    "Variable `$(ex)` not found in `variable_names`. " *
                    "Consider interpolating with \$ if passing a value.",
                ),
            )
        end
        return node_type(; feature=i::Int)
    elseif ex isa AbstractExpressionNode
        return ex
    else
        return node_type(; val=ex)
    end
end

end
