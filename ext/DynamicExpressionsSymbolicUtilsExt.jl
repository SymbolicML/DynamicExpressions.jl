module DynamicExpressionsSymbolicUtilsExt

import Base: convert
#! format: off
if isdefined(Base, :get_extension)
    using SymbolicUtils
    import SymbolicUtils: istree, operation, arguments, similarterm, symtype, issym, arity, metadata, simplify
    import DynamicExpressions.EquationModule: Node, DEFAULT_NODE_TYPE
    import DynamicExpressions.OperatorEnumModule: AbstractOperatorEnum
    import DynamicExpressions.UtilsModule: isgood, isbad, @return_on_false, deprecate_varmap, mustfindfirst
    import DynamicExpressions.ExtensionInterfaceModule: node_to_symbolic, symbolic_to_node
    import DynamicExpressions.SelfContainedEquationModule: SelfContainedNode
else
    using ..SymbolicUtils
    import ..SymbolicUtils: istree, operation, arguments, similarterm, symtype, issym, arity, metadata, simplify
    import ..DynamicExpressions.EquationModule: Node, DEFAULT_NODE_TYPE
    import ..DynamicExpressions.OperatorEnumModule: AbstractOperatorEnum
    import ..DynamicExpressions.UtilsModule: isgood, isbad, @return_on_false, deprecate_varmap, mustfindfirst
    import ..DynamicExpressions.ExtensionInterfaceModule: node_to_symbolic, symbolic_to_node
    import ..DynamicExpressions.SelfContainedEquationModule: SelfContainedNode
end
#! format: on

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
    variable_names::Union{Array{String,1},Nothing}=nothing,
    # Deprecated:
    varMap=nothing,
)
    variable_names = deprecate_varmap(variable_names, varMap, :split_eq)
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
        convert(Node, args[1], operators; variable_names=variable_names),
        convert(Node, op(args[2:end]...), operators; variable_names=variable_names),
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
    variable_names::Union{Array{String,1},Nothing}=nothing,
    index_functions::Bool=false,
    # Deprecated:
    varMap=nothing,
)
    variable_names = deprecate_varmap(variable_names, varMap, :convert)
    return node_to_symbolic(
        tree, operators; variable_names=variable_names, index_functions=index_functions
    )
end

function convert(::typeof(Node), x::Number, operators::AbstractOperatorEnum; kws...)
    return Node(; val=DEFAULT_NODE_TYPE(x))
end

function convert(
    ::typeof(Node),
    expr::SymbolicUtils.Symbolic,
    operators::AbstractOperatorEnum;
    variable_names::Union{Array{String,1},Nothing}=nothing,
)
    variable_names = deprecate_varmap(variable_names, nothing, :convert)
    if !SymbolicUtils.istree(expr)
        variable_names === nothing && return Node(String(expr.name))
        return Node(String(expr.name), variable_names)
    end

    # First, we remove integer powers:
    y, good_return = multiply_powers(expr)
    if good_return
        expr = y
    end

    op = convert_to_function(SymbolicUtils.operation(expr), operators)
    args = SymbolicUtils.arguments(expr)

    length(args) > 2 && return split_eq(op, args, operators; variable_names=variable_names)
    ind = if length(args) == 2
        findoperation(op, operators.binops)
    else
        findoperation(op, operators.unaops)
    end

    return Node(
        ind, map(x -> convert(Node, x, operators; variable_names=variable_names), args)...
    )
end

"""
    node_to_symbolic(tree::Node, operators::AbstractOperatorEnum;
                variable_names::Union{Array{String, 1}, Nothing}=nothing,
                index_functions::Bool=false)

The interface to SymbolicUtils.jl. Passing a tree to this function
will generate a symbolic equation in SymbolicUtils.jl format.

## Arguments

- `tree::Node`: The equation to convert.
- `operators::AbstractOperatorEnum`: OperatorEnum, which contains the operators used in the equation.
- `variable_names::Union{Array{String, 1}, Nothing}=nothing`: What variable names to use for
    each feature. Default is [x1, x2, x3, ...].
- `index_functions::Bool=false`: Whether to generate special names for the
    operators, which then allows one to convert back to a `Node` format
    using `symbolic_to_node`.
    (CURRENTLY UNAVAILABLE - See https://github.com/MilesCranmer/SymbolicRegression.jl/pull/84).
"""
function node_to_symbolic(
    tree::Node,
    operators::AbstractOperatorEnum;
    variable_names::Union{Array{String,1},Nothing}=nothing,
    index_functions::Bool=false,
    # Deprecated:
    varMap=nothing,
)
    variable_names = deprecate_varmap(variable_names, varMap, :node_to_symbolic)
    expr = subs_bad(parse_tree_to_eqs(tree, operators, index_functions))
    # Check for NaN and Inf
    @assert isgood(expr) "The recovered equation contains NaN or Inf."
    # Return if no variable_names is given
    variable_names === nothing && return expr
    # Create a substitution tuple
    subs = Dict(
        [
            SymbolicUtils.Sym{LiteralReal}(Symbol("x$(i)")) =>
                SymbolicUtils.Sym{LiteralReal}(Symbol(variable_names[i])) for
            i in 1:length(variable_names)
        ]...,
    )
    return substitute(expr, subs)
end

function symbolic_to_node(
    eqn::SymbolicUtils.Symbolic,
    operators::AbstractOperatorEnum;
    variable_names::Union{Array{String,1},Nothing}=nothing,
    # Deprecated:
    varMap=nothing,
)::Node
    variable_names = deprecate_varmap(variable_names, varMap, :symbolic_to_node)
    return convert(Node, eqn, operators; variable_names=variable_names)
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

###############################################
# Direct Simplification Interface #############
###############################################

arity(x::SelfContainedNode) = x.tree.degree
istree(x::SelfContainedNode) = arity(x) > 0
symtype(::S) where {T,S<:SelfContainedNode{T}} = T
function operation(x::SelfContainedNode)
    if arity(x) == 1
        return x.operators.unaops[x.tree.op]
    elseif arity(x) == 2
        return x.operators.binops[x.tree.op]
    else
        error("Unexpected degree $(x.tree.degree).")
    end
end
function unsorted_arguments(x::S) where {T,S<:SelfContainedNode{T}}
    if arity(x) == 0
        return Any[]
    elseif arity(x) == 1
        return Any[isconstant(x.tree.l) ? x.tree.l.val::T : S(x.tree.l, x.operators)]
    elseif arity(x) == 2
        return Any[
            isconstant(x.tree.l) ? x.tree.l.val::T : S(x.tree.l, x.operators),
            isconstant(x.tree.r) ? x.tree.r.val::T : S(x.tree.r, x.operators),
        ]
    end
end
function arguments(x::S) where {T,S<:SelfContainedNode{T}}
    return unsorted_arguments(x)
end
function similarterm(
    t::S, f::F, args::AbstractArray, symtype=nothing; kws...
)::S where {T,S<:SelfContainedNode{T},F<:Function}
    if length(args) > 2
        l = similarterm(t, f, args[begin:(begin + 1)], symtype; kws...)
        return similarterm(t, f, [l, args[(begin + 2):end]...], symtype; kws...)
    end
    if length(args) == 1
        op_index = mustfindfirst(f, t.operators.unaops)
        new_node = Node(op_index, to_node(T, op_index, args[1]))
        return S(new_node, t.operators)
    elseif length(args) == 2
        op_index = mustfindfirst(f, t.operators.binops)
        new_node = if all(isconstant, args)
            to_node(T, op_index, f(args...))
        else
            Node(op_index, [to_node(T, op_index, arg) for arg in args]...)
        end
        return S(new_node, t.operators)
    else
        error("Unexpected length $(length(args)).")
    end
end

# Helper functions for interface
isconstant(x::SelfContainedNode) = isconstant(x.tree)
isconstant(x::Node) = x.degree == 0 && x.constant
isconstant(::Number) = true
to_node(::Type{T}, op_index, x::SelfContainedNode{T}) where {T} = Node(op_index, x.tree)
to_node(::Type{T}, op_index, x::Number) where {T} = Node(T; val=x)

function simplify(x::Node, operators::AbstractOperatorEnum, args...; kws...)
    return simplify(SelfContainedNode(x, operators), args...; kws...)
end

end
