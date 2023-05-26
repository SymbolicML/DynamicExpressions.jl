module DynamicExpressionsSymbolicUtilsExt

export node_to_symbolic, symbolic_to_node

import Base: convert
using SymbolicUtils
if isdefined(Base, :get_extension)
    import ..DynamicExpressions.EquationModule: Node, DEFAULT_NODE_TYPE
    import ..DynamicExpressions.OperatorEnumModule: AbstractOperatorEnum
    import ..DynamicExpressions.UtilsModule: isgood, isbad, @return_on_false
else
    import ..EquationModule: Node, DEFAULT_NODE_TYPE
    import ..OperatorEnumModule: AbstractOperatorEnum
    import ..UtilsModule: isgood, isbad, @return_on_false
end

const SYMBOLIC_UTILS_TYPES = Union{<:Number,SymbolicUtils.Symbolic{<:Number}}

const SUPPORTED_OPS = (cos, sin, exp, cot, tan, csc, sec, +, -, *, /)

function isgood(x::SymbolicUtils.Symbolic)
    return if SymbolicUtils.istree(x)
        all(isgood.([SymbolicUtils.operation(x); SymbolicUtils.arguments(x)]))
    else
        true
    end
end
subs_bad(x) = isgood(x) ? x : Inf

function parse_tree_to_eqs(
    tree::Node{T}, operators::AbstractOperatorEnum, index_functions::Bool=false
) where {T}
    if tree.degree == 0
        # Return constant if needed
        tree.constant && return subs_bad(tree.val::T)
        return SymbolicUtils.Sym{LiteralReal}(Symbol("x$(tree.feature)"))
    end
    # Collect the next children
    children = tree.degree >= 2 ? (tree.l, tree.r) : (tree.l,)
    # Get the operation
    op = tree.degree > 1 ? operators.binops[tree.op] : operators.unaops[tree.op]
    # Create an N tuple of Numbers for each argument
    dtypes = map(x -> Number, 1:(tree.degree))
    #
    if !(op ∈ SUPPORTED_OPS) && index_functions
        op = SymbolicUtils.Sym{(SymbolicUtils.FnType){Tuple{dtypes...},Number}}(Symbol(op))
    end

    return subs_bad(
        op(map(x -> parse_tree_to_eqs(x, operators, index_functions), children)...)
    )
end

# For operators which are indexed, we need to convert them back
# using the string:
function convert_to_function(
    x::SymbolicUtils.Sym{SymbolicUtils.FnType{T,Number}}, operators::AbstractOperatorEnum
) where {T<:Tuple}
    degree = length(T.types)
    if degree == 1
        ind = findoperation(x.name, operators.unaops)
        return operators.unaops[ind]
    elseif degree == 2
        ind = findoperation(x.name, operators.binops)
        return operators.binops[ind]
    else
        throw(AssertionError("Function $(String(x.name)) has degree > 2 !"))
    end
end

# For normal operators, simply return the function itself:
convert_to_function(x, operators::AbstractOperatorEnum) = x

# Split equation
function split_eq(
    op,
    args,
    operators::AbstractOperatorEnum;
    varMap::Union{Array{String,1},Nothing}=nothing,
)
    !(op ∈ (sum, prod, +, *)) && throw(error("Unsupported operation $op in expression!"))
    if Symbol(op) == Symbol(sum)
        ind = findoperation(+, operators.binops)
    elseif Symbol(op) == Symbol(prod)
        ind = findoperation(*, operators.binops)
    else
        ind = findoperation(op, operators.binops)
    end
    return Node(
        ind,
        convert(Node, args[1], operators; varMap=varMap),
        convert(Node, op(args[2:end]...), operators; varMap=varMap),
    )
end

function findoperation(op, ops)
    for (i, oi) in enumerate(ops)
        Symbol(oi) == Symbol(op) && return i
    end
    throw(error("Operation $(op) in expression not found in operations $(ops)!"))
end

function convert(
    ::typeof(SymbolicUtils.Symbolic),
    tree::Node,
    operators::AbstractOperatorEnum;
    varMap::Union{Array{String,1},Nothing}=nothing,
    index_functions::Bool=false,
)
    return node_to_symbolic(tree, operators; varMap=varMap, index_functions=index_functions)
end

function convert(
    ::typeof(Node),
    x::Number,
    operators::AbstractOperatorEnum;
    varMap::Union{Array{String,1},Nothing}=nothing,
)
    return Node(; val=DEFAULT_NODE_TYPE(x))
end

function convert(
    ::typeof(Node),
    expr::SymbolicUtils.Symbolic,
    operators::AbstractOperatorEnum;
    varMap::Union{Array{String,1},Nothing}=nothing,
)
    if !SymbolicUtils.istree(expr)
        varMap === nothing && return Node(String(expr.name))
        return Node(String(expr.name), varMap)
    end

    # First, we remove integer powers:
    y, good_return = multiply_powers(expr)
    if good_return
        expr = y
    end

    op = convert_to_function(SymbolicUtils.operation(expr), operators)
    args = SymbolicUtils.arguments(expr)

    length(args) > 2 && return split_eq(op, args, operators; varMap=varMap)
    ind = if length(args) == 2
        findoperation(op, operators.binops)
    else
        findoperation(op, operators.unaops)
    end

    return Node(ind, map(x -> convert(Node, x, operators; varMap=varMap), args)...)
end

"""
    node_to_symbolic(tree::Node, operators::AbstractOperatorEnum;
                varMap::Union{Array{String, 1}, Nothing}=nothing,
                index_functions::Bool=false)

The interface to SymbolicUtils.jl. Passing a tree to this function
will generate a symbolic equation in SymbolicUtils.jl format.

## Arguments

- `tree::Node`: The equation to convert.
- `operators::AbstractOperatorEnum`: OperatorEnum, which contains the operators used in the equation.
- `varMap::Union{Array{String, 1}, Nothing}=nothing`: What variable names to use for
    each feature. Default is [x1, x2, x3, ...].
- `index_functions::Bool=false`: Whether to generate special names for the
    operators, which then allows one to convert back to a `Node` format
    using `symbolic_to_node`.
    (CURRENTLY UNAVAILABLE - See https://github.com/MilesCranmer/SymbolicRegression.jl/pull/84).
"""
function node_to_symbolic(
    tree::Node,
    operators::AbstractOperatorEnum;
    varMap::Union{Array{String,1},Nothing}=nothing,
    index_functions::Bool=false,
)
    expr = subs_bad(parse_tree_to_eqs(tree, operators, index_functions))
    # Check for NaN and Inf
    @assert isgood(expr) "The recovered equation contains NaN or Inf."
    # Return if no varMap is given
    varMap === nothing && return expr
    # Create a substitution tuple
    subs = Dict(
        [
            SymbolicUtils.Sym{LiteralReal}(Symbol("x$(i)")) =>
                SymbolicUtils.Sym{LiteralReal}(Symbol(varMap[i])) for i in 1:length(varMap)
        ]...,
    )
    return substitute(expr, subs)
end

function symbolic_to_node(
    eqn::T, operators::AbstractOperatorEnum; varMap::Union{Array{String,1},Nothing}=nothing
)::Node where {T<:SymbolicUtils.Symbolic}
    return convert(Node, eqn, operators; varMap=varMap)
end

function multiply_powers(eqn::Number)::Tuple{SYMBOLIC_UTILS_TYPES,Bool}
    return eqn, true
end

function multiply_powers(eqn::SymbolicUtils.Symbolic)::Tuple{SYMBOLIC_UTILS_TYPES,Bool}
    if !SymbolicUtils.istree(eqn)
        return eqn, true
    end
    op = SymbolicUtils.operation(eqn)
    return multiply_powers(eqn, op)
end

function multiply_powers(
    eqn::SymbolicUtils.Symbolic, op::F
)::Tuple{SYMBOLIC_UTILS_TYPES,Bool} where {F}
    args = SymbolicUtils.arguments(eqn)
    nargs = length(args)
    if nargs == 1
        l, complete = multiply_powers(args[1])
        @return_on_false complete eqn
        @return_on_false isgood(l) eqn
        return op(l), true
    elseif op == ^
        l, complete = multiply_powers(args[1])
        @return_on_false complete eqn
        @return_on_false isgood(l) eqn
        n = args[2]
        if typeof(n) <: Int
            if n == 1
                return l, true
            elseif n == -1
                return 1.0 / l, true
            elseif n > 1
                return reduce(*, [l for i in 1:n]), true
            elseif n < -1
                return reduce(/, vcat([1], [l for i in 1:abs(n)])), true
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
        @return_on_false isgood(l) eqn
        r, complete2 = multiply_powers(args[2])
        @return_on_false complete2 eqn
        @return_on_false isgood(r) eqn
        return op(l, r), true
    else
        # return tree_mapreduce(multiply_powers, op, args)
        # ## reduce(op, map(multiply_powers, args))
        out = map(multiply_powers, args) #vector of tuples
        for i in 1:size(out, 1)
            @return_on_false out[i][2] eqn
            @return_on_false isgood(out[i][1]) eqn
        end
        cumulator = out[1][1]
        for i in 2:size(out, 1)
            cumulator = op(cumulator, out[i][1])
            @return_on_false isgood(cumulator) eqn
        end
        return cumulator, true
    end
end

end
