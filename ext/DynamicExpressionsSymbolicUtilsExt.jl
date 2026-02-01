module DynamicExpressionsSymbolicUtilsExt

using DynamicExpressions:
    AbstractExpression, get_tree, get_operators, get_variable_names, default_node_type
using DynamicExpressions.NodeModule:
    AbstractExpressionNode, Node, constructorof, DEFAULT_NODE_TYPE, get_child
using DynamicExpressions.OperatorEnumModule: AbstractOperatorEnum
using DynamicExpressions.UtilsModule: deprecate_varmap

using SymbolicUtils
using SymbolicUtils: BasicSymbolic, SymReal, iscall, issym, isconst, unwrap_const

import DynamicExpressions.ExtensionInterfaceModule: node_to_symbolic, symbolic_to_node
import DynamicExpressions.ValueInterfaceModule: is_valid

const SYMBOLIC_UTILS_TYPES = Union{<:Number,BasicSymbolic}
const SUPPORTED_OPS = (cos, sin, exp, cot, tan, csc, sec, +, -, *, /)

macro return_on_false(flag, retval)
    :(
        if !$(esc(flag))
            return ($(esc(retval)), false)
        end
    )
end

function is_valid(x::BasicSymbolic)
    return if iscall(x)
        all(is_valid.([SymbolicUtils.operation(x); SymbolicUtils.arguments(x)]))
    else
        true
    end
end
subs_bad(x) = is_valid(x) ? x : Inf

_sym_number(sym::Symbol) = SymbolicUtils.Sym{SymReal}(sym; type=Number)

function _sym_fn(sym::Symbol, degree::Integer)
    degree_int = Int(degree)
    dtypes = ntuple(_ -> Number, degree_int)
    ftype = Base.unwrap_unionall(SymbolicUtils.FnType{Tuple{dtypes...},Number})
    return SymbolicUtils.Sym{SymReal}(sym; type=ftype)
end

function parse_tree_to_eqs(
    tree::AbstractExpressionNode{T,2},
    operators::AbstractOperatorEnum,
    index_functions::Bool=false,
) where {T}
    if tree.degree == 0
        # Return constant if needed
        tree.constant && return subs_bad(tree.val)
        return _sym_number(Symbol("x$(tree.feature)"))
    end
    # Collect the next children
    # TODO: Type instability!
    children =
        tree.degree == 2 ? (get_child(tree, 1), get_child(tree, 2)) : (get_child(tree, 1),)
    # Get the operation
    op = tree.degree == 2 ? operators.binops[tree.op] : operators.unaops[tree.op]
    # Convert children to symbolic form
    sym_children = map(x -> parse_tree_to_eqs(x, operators, index_functions), children)

    # Only a small subset of functions have symbolic methods in SymbolicUtils.
    # For unsupported operators:
    # - when `index_functions=true`, we encode them as function-like symbols so they
    #   can be round-tripped back to operators via their name/arity.
    # - when `index_functions=false`, we throw a clear error, since attempting to
    #   construct a SymbolicUtils term headed by an arbitrary function object can
    #   fail with a MethodError.
    if !(op ∈ SUPPORTED_OPS)
        if index_functions
            op = _sym_fn(Symbol(op), tree.degree)
            return subs_bad(op(sym_children...))
        else
            throw(error("Unsupported operation $(op) in SymbolicUtils conversion"))
        end
    end

    return subs_bad(op(sym_children...))
end

function convert_to_function(x::BasicSymbolic, operators::AbstractOperatorEnum)
    if issym(x) && SymbolicUtils.symtype(x) <: SymbolicUtils.FnType
        st = Base.unwrap_unionall(SymbolicUtils.symtype(x))
        sig = st.parameters[1]
        degree = length(sig.parameters)
        fname = nameof(x)
        if degree == 1
            ind = findoperation(fname, operators.unaops)
            return operators.unaops[ind]
        elseif degree == 2
            ind = findoperation(fname, operators.binops)
            return operators.binops[ind]
        else
            throw(AssertionError("Function $(String(fname)) has degree > 2 !"))
        end
    end
    return x
end

# For normal operators, simply return the function itself:
convert_to_function(x, operators::AbstractOperatorEnum) = x

# Split equation
function split_eq(
    op,
    args,
    operators::AbstractOperatorEnum,
    ::Type{N}=Node;
    variable_names::Union{AbstractVector{<:AbstractString},Nothing}=nothing,
    # Deprecated:
    varMap=nothing,
) where {N<:AbstractExpressionNode}
    variable_names = deprecate_varmap(variable_names, varMap, :split_eq)
    !(op ∈ (sum, prod, +, *)) && throw(error("Unsupported operation $op in expression!"))
    if Symbol(op) == Symbol(sum)
        ind = findoperation(+, operators.binops)
    elseif Symbol(op) == Symbol(prod)
        ind = findoperation(*, operators.binops)
    else
        ind = findoperation(op, operators.binops)
    end
    return constructorof(N)(;
        op=ind,
        l=convert(N, args[1], operators; variable_names),
        r=convert(N, op(args[2:end]...), operators; variable_names),
    )
end

function findoperation(op, ops)
    for (i, oi) in enumerate(ops)
        Symbol(oi) == Symbol(op) && return i
    end
    throw(error("Operation $(op) in expression not found in operations $(ops)!"))
end

function Base.convert(
    ::typeof(BasicSymbolic),
    tree::Union{AbstractExpression,AbstractExpressionNode},
    operators::Union{AbstractOperatorEnum,Nothing}=nothing;
    variable_names::Union{AbstractVector{<:AbstractString},Nothing}=nothing,
    index_functions::Bool=false,
    # Deprecated:
    varMap=nothing,
)
    variable_names = deprecate_varmap(variable_names, varMap, :convert)
    return node_to_symbolic(
        tree, get_operators(tree, operators); variable_names, index_functions
    )
end

function Base.convert(
    ::Type{N}, x::Number, operators::AbstractOperatorEnum; kws...
) where {N<:AbstractExpressionNode}
    return constructorof(N)(; val=DEFAULT_NODE_TYPE(x))
end

function Base.convert(
    ::Type{N},
    expr::BasicSymbolic,
    operators::AbstractOperatorEnum;
    variable_names::Union{AbstractVector{<:AbstractString},Nothing}=nothing,
) where {N<:AbstractExpressionNode}
    variable_names = deprecate_varmap(variable_names, nothing, :convert)
    # Handle constants (v4 wraps numbers in Const variant)
    if isconst(expr)
        return constructorof(N)(; val=DEFAULT_NODE_TYPE(unwrap_const(expr)))
    end
    # Handle symbols (variables)
    if issym(expr)
        exprname = nameof(expr)
        if variable_names === nothing
            s = String(exprname)
            # Verify it is of the format "x{num}":
            @assert(
                occursin(r"^x\d+$", s),
                "Variable name $s is not of the format x{num}. Please provide the `variable_names` explicitly."
            )
            return constructorof(N)(s)
        end
        return constructorof(N)(String(exprname), variable_names)
    end
    # Handle function calls
    if !iscall(expr)
        error("Unknown symbolic expression type: $(typeof(expr))")
    end

    # First, we remove integer powers:
    y, good_return = multiply_powers(expr)
    if good_return
        expr = y
    end

    op = convert_to_function(SymbolicUtils.operation(expr), operators)
    args = SymbolicUtils.arguments(expr)

    length(args) > 2 && return split_eq(op, args, operators, N; variable_names)
    ind = if length(args) == 2
        findoperation(op, operators.binops)
    else
        findoperation(op, operators.unaops)
    end

    if length(args) == 2
        children = map(x -> convert(N, x, operators; variable_names), (args[1], args[2]))
        return constructorof(N)(; op=ind, children)
    else
        children = map(x -> convert(N, x, operators; variable_names), (only(args),))
        return constructorof(N)(; op=ind, children)
    end
end

_node_type(::Type{<:AbstractExpression{T,N}}) where {T,N<:AbstractExpressionNode} = N
_node_type(::Type{E}) where {E<:AbstractExpression} = default_node_type(E)

function Base.convert(
    ::Type{E},
    x::Union{BasicSymbolic,Number},
    operators::AbstractOperatorEnum;
    variable_names::Union{AbstractVector{<:AbstractString},Nothing}=nothing,
    kws...,
) where {E<:AbstractExpression}
    N = _node_type(E)
    tree = convert(N, x, operators; variable_names)
    return constructorof(E)(tree; operators, variable_names, kws...)
end

"""
    node_to_symbolic(tree::AbstractExpressionNode, operators::AbstractOperatorEnum;
                variable_names::Union{AbstractVector{<:AbstractString}, Nothing}=nothing,
                index_functions::Bool=false)

The interface to SymbolicUtils.jl. Passing a tree to this function
will generate a symbolic equation in SymbolicUtils.jl format.

## Arguments

- `tree::AbstractExpressionNode`: The equation to convert.
- `operators::AbstractOperatorEnum`: OperatorEnum, which contains the operators used in the equation.
- `variable_names::Union{AbstractVector{<:AbstractString}, Nothing}=nothing`: What variable names to use for
    each feature. Default is [x1, x2, x3, ...].
- `index_functions::Bool=false`: Whether to generate special names for the
    operators, which then allows one to convert back to a `AbstractExpressionNode` format
    using `symbolic_to_node`.
"""
function node_to_symbolic(
    tree::AbstractExpressionNode{T,2},
    operators::AbstractOperatorEnum;
    variable_names::Union{AbstractVector{<:AbstractString},Nothing}=nothing,
    index_functions::Bool=false,
    # Deprecated:
    varMap=nothing,
) where {T}
    variable_names = deprecate_varmap(variable_names, varMap, :node_to_symbolic)
    expr = subs_bad(parse_tree_to_eqs(tree, operators, index_functions))
    # Check for NaN and Inf
    @assert is_valid(expr) "The recovered equation contains NaN or Inf."
    # Return if no variable_names is given
    variable_names === nothing && return expr
    # Create a substitution tuple
    subs = Dict(
        [
            _sym_number(Symbol("x$(i)")) => _sym_number(Symbol(variable_names[i])) for
            i in 1:length(variable_names)
        ]...,
    )
    return substitute(expr, subs)
end
function node_to_symbolic(
    tree::AbstractExpression,
    operators::Union{AbstractOperatorEnum,Nothing}=nothing;
    variable_names::Union{AbstractVector{<:AbstractString},Nothing}=nothing,
    kws...,
)
    return node_to_symbolic(
        get_tree(tree),
        get_operators(tree, operators);
        variable_names=get_variable_names(tree, variable_names),
        kws...,
    )
end

function symbolic_to_node(
    eqn::BasicSymbolic,
    operators::AbstractOperatorEnum,
    ::Type{N}=Node;
    variable_names::Union{AbstractVector{<:AbstractString},Nothing}=nothing,
    # Deprecated:
    varMap=nothing,
) where {N<:AbstractExpressionNode}
    variable_names = deprecate_varmap(variable_names, varMap, :symbolic_to_node)
    return convert(N, eqn, operators; variable_names=variable_names)
end

function multiply_powers(eqn::Number)::Tuple{SYMBOLIC_UTILS_TYPES,Bool}
    return eqn, true
end

function multiply_powers(eqn::BasicSymbolic)::Tuple{SYMBOLIC_UTILS_TYPES,Bool}
    if !iscall(eqn)
        return eqn, true
    end
    op = SymbolicUtils.operation(eqn)
    return multiply_powers(eqn, op)
end

function multiply_powers(
    eqn::BasicSymbolic, op::F
)::Tuple{SYMBOLIC_UTILS_TYPES,Bool} where {F}
    args = SymbolicUtils.arguments(eqn)
    nargs = length(args)
    if nargs == 1
        l, complete = multiply_powers(args[1])
        @return_on_false complete eqn
        @return_on_false is_valid(l) eqn
        return op(l), true
    elseif op == ^
        l, complete = multiply_powers(args[1])
        @return_on_false complete eqn
        @return_on_false is_valid(l) eqn
        n = args[2]
        # In SymbolicUtils v4, integer constants are wrapped in Const
        n_val = if isconst(n)
            unwrap_const(n)
        elseif typeof(n) <: Integer
            n
        else
            nothing
        end
        if n_val !== nothing && typeof(n_val) <: Integer
            if n_val == 1
                return l, true
            elseif n_val == -1
                return 1.0 / l, true
            elseif n_val > 1
                return reduce(*, [l for i in 1:n_val]), true
            elseif n_val < -1
                return reduce(/, vcat([1], [l for i in 1:abs(n_val)])), true
            else
                return 1.0, true
            end
        else
            r, complete2 = multiply_powers(args[2])
            @return_on_false complete2 eqn
            return l^r, true
        end
    elseif nargs == 2
        l, complete = multiply_powers(args[1])
        @return_on_false complete eqn
        @return_on_false is_valid(l) eqn
        r, complete2 = multiply_powers(args[2])
        @return_on_false complete2 eqn
        @return_on_false is_valid(r) eqn
        return op(l, r), true
    else
        # return tree_mapreduce(multiply_powers, op, args)
        # ## reduce(op, map(multiply_powers, args))
        out = map(multiply_powers, args) #vector of tuples
        for i in 1:size(out, 1)
            @return_on_false out[i][2] eqn
            @return_on_false is_valid(out[i][1]) eqn
        end
        cumulator = out[1][1]
        for i in 2:size(out, 1)
            cumulator = op(cumulator, out[i][1])
            @return_on_false is_valid(cumulator) eqn
        end
        return cumulator, true
    end
end

end
