module ParseModule

using DispatchDoctor: @unstable

using ..NodeModule: AbstractExpressionNode, Node, constructorof
using ..OperatorEnumModule: AbstractOperatorEnum
using ..OperatorEnumConstructionModule: OperatorEnum, empty_all_globals!
using ..ExpressionModule:
    AbstractExpression,
    Expression,
    default_node_type,
    get_operators,
    get_variable_names,
    node_type
using ..ExpressionAlgebraModule: declare_operator_alias

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
- `node_type`: The type of the nodes in the resulting expression tree. Defaults to `default_node_type(expression_type)`.
- `binary_operators`: Convenience syntax for creating an `OperatorEnum`.
- `unary_operators`: Convenience syntax for creating an `OperatorEnum`.

## Usage

The macro is used to convert a high-level symbolic expression into a structured expression tree that can be manipulated or evaluated. Here are some examples of how to use `parse_expression`:

### Parsing from a custom operator

```julia
julia> my_custom_op(x, y) = x + y^3;

julia> operators = OperatorEnum(1 => (sin,), 2 => (+, -, *, my_custom_op));

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
            operators=OperatorEnum(1 => (cos, exp), 2 => (-,)),
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
           operators = OperatorEnum(1 => (cos,), 2 => (*, -)),
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
            eval_module=$(parsed_kws.eval_module),
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
    eval_module = nothing
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
            elseif kw == :eval_module
                eval_module = kw
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
            elseif kw.args[1] == :eval_module
                eval_module = kw.args[2]
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
                "Unrecognized argument: `$kw`. The available arguments are `operators`, `variable_names`, `node_type`, `expression_type`, `evaluate_on`, `eval_module`, and `extra_metadata`.",
            ),
        )
    end
    node_type =
        node_type === nothing ? :($(default_node_type)($expression_type)) : node_type

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
        operators,
        variable_names,
        node_type,
        expression_type,
        evaluate_on,
        eval_module,
        extra_metadata,
    )
end

_replace_imaginary_unit_symbol(ex) = ex
@unstable _replace_imaginary_unit_symbol(ex::Symbol) = ex === :im ? im : ex
@unstable function _replace_imaginary_unit_symbol(ex::Expr)
    args = map(_replace_imaginary_unit_symbol, ex.args)
    # Fold constant arithmetic involving the imaginary unit, so that the
    # normalized string form of a complex constant (`a + b*im`) parses back
    # into a single constant leaf:
    if ex.head == :call && length(args) == 3 && first(args) isa Symbol
        op, l, r = args
        if op === :+
            (l isa Real && r isa Complex && iszero(real(r))) && return l + r
            (l isa Complex && iszero(real(l)) && r isa Real) && return l + r
        elseif op === :-
            (l isa Real && r isa Complex && iszero(real(r))) && return l - r
        elseif op === :*
            (l isa Real && r isa Complex && iszero(real(r))) && return l * r
            (l isa Complex && iszero(real(l)) && r isa Real) && return l * r
        end
    end
    return Expr(ex.head, args...)
end

@unstable function _normalize_expression_for_parse(
    ex, variable_names::Union{AbstractVector{<:AbstractString},Nothing}
)
    if variable_names !== nothing && ("im" in variable_names)
        return ex
    end
    return _replace_imaginary_unit_symbol(ex)
end

"""Parse an expression Julia `Expr` object."""
@unstable function parse_expression(
    ex;
    operators::Union{AbstractOperatorEnum,Nothing}=nothing,
    binary_operators::Union{Vector{<:Function},Nothing}=nothing,
    unary_operators::Union{Vector{<:Function},Nothing}=nothing,
    variable_names::Union{AbstractVector,Nothing}=nothing,
    expression_type::Type{E}=Expression,
    node_type::Type{N}=default_node_type(expression_type),
    evaluate_on::Union{Nothing,AbstractVector}=nothing,
    eval_module::Union{Module,Nothing}=nothing,
    kws...,
) where {N<:AbstractExpressionNode,E<:AbstractExpression}
    empty_all_globals!(; force=false)
    let variable_names = if variable_names === nothing
            nothing
        elseif eltype(variable_names) <: AbstractString
            variable_names
        else
            map(v -> string(v)::String, variable_names)
        end,
        operators = if operators === nothing
            OperatorEnum(;
                binary_operators=if binary_operators === nothing
                    Function[]
                else
                    binary_operators
                end,
                unary_operators=if unary_operators === nothing
                    Function[]
                else
                    unary_operators
                end,
            )
        else
            operators
        end

        ex = _normalize_expression_for_parse(ex, variable_names)
        parser = ExpressionParser(
            operators, variable_names, N, E, evaluate_on, eval_module, NamedTuple(kws)
        )
        tree = parser(ex)
        return constructorof(E)(tree; operators, variable_names, kws...)
    end
end

@unstable parse_expression(ex::String; kws...) = parse_expression(Meta.parse(ex); kws...)

"""
Find an operator function by its name in the OperatorEnum, considering the arity.
Throws appropriate errors for ambiguous or missing matches.
"""
@unstable function _find_operator_by_name(func_symbol, degree, operators)
    matches = Tuple{Function,Int}[]

    for arity in 1:length(operators.ops)
        for op in operators.ops[arity]
            if nameof(op) == func_symbol
                push!(matches, (op, arity))
            end
        end
    end

    if isempty(matches)
        throw(
            ArgumentError(
                "Tried to interpolate function `$(func_symbol)` but failed. " *
                "Function not found in operators.",
            ),
        )
    end

    arity_matches = filter(m -> m[2] == degree, matches)

    if length(arity_matches) > 1
        throw(
            ArgumentError(
                "Ambiguous operator `$(func_symbol)` with arity $(degree). " *
                "Multiple matches found: $(arity_matches)",
            ),
        )
    elseif length(arity_matches) == 0
        available_arities = [m[2] for m in matches]
        throw(
            ArgumentError(
                "Operator `$(func_symbol)` found but not with arity $(degree). " *
                "Available arities: $(available_arities)",
            ),
        )
    end

    return arity_matches[1][1]::Function
end

"""An empty module for evaluation without collisions."""
module EmptyModule end

"""
    ExpressionParser

Internal carrier for the fixed arguments of the parsing recursion.
Calling `parser(ex)` parses `ex` into a node.
"""
struct ExpressionParser{
    O<:AbstractOperatorEnum,
    V<:Union{AbstractVector{<:AbstractString},Nothing},
    N<:AbstractExpressionNode,
    E<:AbstractExpression,
    EV<:Union{Nothing,AbstractVector},
    M<:Union{Module,Nothing},
    K<:NamedTuple,
}
    operators::O
    variable_names::V
    node_type::Type{N}
    expression_type::Type{E}
    evaluate_on::EV
    eval_module::M
    kws::K
end

@unstable function (p::ExpressionParser)(ex)
    return _parse_expression(p, ex)
end

@unstable function _parse_expression(p::ExpressionParser, ex::Expr)
    (; operators, evaluate_on, eval_module) = p
    if ex.head == :call
        args = ex.args
        callee = first(args)
        if eval_module !== nothing && _references_variable(callee, p.variable_names)
            throw(
                ArgumentError(
                    "Cannot use a declared variable in the callee `$(callee)`. " *
                    "Declared variables cannot be called in an expression.",
                ),
            )
        end
        func = if eval_module === nothing
            try
                Core.eval(EmptyModule, callee)
            catch
                nothing
            end
        else
            # Only pure name resolution: computed callees and value property
            # accesses are left for (single) evaluation during constant folding.
            _try_resolve(eval_module, callee)
        end
        if func === nothing ||
            (eval_module !== nothing && !_matches_operator(func, operators, evaluate_on))
            named = try
                _find_operator_by_name(callee, length(args) - 1, operators)
            catch
                eval_module === nothing && rethrow()
                nothing
            end
            named === nothing || (func = named)
        end
        if eval_module === nothing || _matches_operator(func, operators, evaluate_on)
            return _parse_expression(p, func::Function, args)
        end
    elseif eval_module === nothing
        throw(
            ArgumentError(
                "Unrecognized expression type: `Expr(:$(ex.head), ...)`. " *
                "Please only pass a function call or a variable.",
            ),
        )
    end
    if _references_variable(ex, p.variable_names)
        throw(
            ArgumentError(
                "Cannot evaluate `$(ex)` as a constant since it references variables. " *
                "If it is meant as an operator call, pass the operator via `operators` " *
                "or `evaluate_on`.",
            ),
        )
    end
    val = try
        Core.eval(eval_module, ex)
    catch
        throw(
            ArgumentError(
                "Failed to evaluate `$(ex)` as a constant in `$(eval_module)`. " *
                "It is not an operator call, so it must evaluate in the given module.",
            ),
        )
    end
    if val isa Symbol
        # Already a value; must not be re-interpreted as a variable name
        return p.node_type(; val)
    end
    return parse_leaf(
        val, p.variable_names, p.node_type, p.expression_type; eval_module, p.kws...
    )
end

@unstable function _matches_operator(func, operators, evaluate_on)
    return func isa Function && (
        (evaluate_on !== nothing && func in evaluate_on) ||
        any(1:length(operators.ops)) do arity
            any(
                op -> op == func || declare_operator_alias(op, Val(arity)) == func,
                operators[arity],
            )
        end
    )
end

"""Resolve a literal or (dotted) name in `mod` without running arbitrary code."""
_try_resolve(mod::Module, s::Symbol) = isdefined(mod, s) ? getproperty(mod, s) : nothing
_try_resolve(::Module, q::QuoteNode) = q.value
function _try_resolve(mod::Module, ex::Expr)
    (ex.head == :. && length(ex.args) == 2) || return nothing
    base = _try_resolve(mod, ex.args[1])
    base isa Module || return nothing
    name = ex.args[2]
    s = name isa QuoteNode ? name.value : name
    s isa Symbol || return nothing
    return isdefined(base, s) ? getproperty(base, s) : nothing
end
_try_resolve(::Module, x) = x

function _references_variable(ex, variable_names)
    return _references_variable(ex, variable_names, String[])
end
function _references_variable(ex::Symbol, variable_names, bound)
    return string(ex) ∉ bound && variable_names !== nothing && string(ex) in variable_names
end
function _references_variable(ex::Expr, variable_names, bound)
    if ex.head == :generator
        bound = copy(bound)
        for spec in ex.args[2:end]
            specs = spec isa Expr && spec.head == :filter ? spec.args[2:end] : (spec,)
            for s in specs
                s isa Expr && s.head == :(=) || return true
                _references_variable(s.args[2], variable_names, bound) && return true
                _collect_bound!(bound, s.args[1])
            end
            if spec isa Expr && spec.head == :filter
                _references_variable(spec.args[1], variable_names, bound) && return true
            end
        end
        return _references_variable(ex.args[1], variable_names, bound)
    elseif ex.head == :(=) || ex.head == :kw
        # A plain identifier target binds a name; compound targets read theirs
        _lhs_reads(ex.args[1], variable_names, bound) && return true
        return _references_variable(ex.args[2], variable_names, bound)
    elseif ex.head == :let
        bound = copy(bound)
        bindings = ex.args[1]
        for s in
            (bindings isa Expr && bindings.head == :block ? bindings.args : (bindings,))
            if s isa Expr && s.head == :(=)
                _references_variable(s.args[2], variable_names, bound) && return true
                _collect_bound!(bound, s.args[1])
            elseif s isa Symbol
                _collect_bound!(bound, s)
            else
                return true  # unknown binding form; reject conservatively
            end
        end
        return _references_variable(ex.args[2], variable_names, bound)
    elseif ex.head == :->
        bound = copy(bound)
        _collect_bound!(bound, ex.args[1])
        return _references_variable(ex.args[2], variable_names, bound)
    else
        return any(arg -> _references_variable(arg, variable_names, bound), ex.args)
    end
end
_references_variable(_, _, _) = false

_collect_bound!(bound, name::Symbol) = push!(bound, string(name))
function _collect_bound!(bound, ex::Expr)
    foreach(arg -> _collect_bound!(bound, arg), ex.args)
    return bound
end
_collect_bound!(bound, _) = bound

_lhs_reads(::Symbol, _, _) = false
function _lhs_reads(ex::Expr, variable_names, bound)
    if ex.head == :tuple || ex.head == :parameters
        return any(arg -> _lhs_reads(arg, variable_names, bound), ex.args)
    else
        return _references_variable(ex, variable_names, bound)
    end
end
_lhs_reads(_, _, _) = false
@unstable function _parse_expression(
    p::ExpressionParser{<:Any,<:Any,N}, func::F, args
)::N where {F<:Function,N<:AbstractExpressionNode}
    (; operators, evaluate_on) = p
    degree = length(args) - 1
    if degree <= length(operators.ops) && (
        op_idx = findfirst(
            op -> op == func || declare_operator_alias(op, Val(degree)) == func,
            operators[degree],
        );
        !isnothing(op_idx)
    )
        return N(; op=op_idx::Int, children=map(p, (args[2:end]...,)))
    end

    # Handle chaining for +, -, * operators
    if degree > 2 &&
        func ∈ (+, -, *) &&
        (
            op_idx = findfirst(
                op -> op == func || declare_operator_alias(op, Val(2)) == func, operators[2]
            );
            !isnothing(op_idx)
        )
        inner = N(; op=op_idx::Int, children=(p(args[2]), p(args[3])))
        for arg in args[4:end]
            inner = N(; op=op_idx::Int, children=(inner, p(arg)))
        end
        return inner
    end

    if evaluate_on !== nothing && func in evaluate_on
        # External function
        func(map(p, args[2:end])...)
    else
        matching_s = let
            s = if degree <= length(operators.ops)
                join(('`', operators[degree], '`'))
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
@unstable function _parse_expression(p::ExpressionParser, ex)
    return parse_leaf(
        ex,
        p.variable_names,
        p.node_type,
        p.expression_type;
        eval_module=p.eval_module,
        p.kws...,
    )
end

@unstable function parse_leaf(
    ex,
    variable_names,
    node_type::Type{<:AbstractExpressionNode},
    expression_type::Type{<:AbstractExpression};
    eval_module::Union{Module,Nothing}=nothing,
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
            if eval_module !== nothing
                return node_type(; val=Core.eval(eval_module, ex))
            end
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
