module EquationModule

import ..OperatorEnumModule: AbstractOperatorEnum
import ..UtilsModule: @memoize_on, @with_memoize, deprecate_varmap

const DEFAULT_NODE_TYPE = Float32

"""
    AbstractNode

Abstract type for binary trees. Must have the following fields:

- `degree::Integer`: Degree of the node. Either 0, 1, or 2. If 1,
    then `l` needs to be defined as the left child. If 2,
    then `r` also needs to be defined as the right child.
- `l::AbstractNode`: Left child of the current node. Should only be
    defined if `degree >= 1`; otherwise, leave it undefined (see the
    the constructors of `Node{T}` for an example).
    Don't use `nothing` to represent an undefined value
    as it will incur a large performance penalty.
- `r::AbstractNode`: Right child of the current node. Should only
    be defined if `degree == 2`.
"""
abstract type AbstractNode end

#! format: off
"""
    Node{T}

Node defines a symbolic expression stored in a binary tree.
A single `Node` instance is one "node" of this tree, and
has references to its children. By tracing through the children
nodes, you can evaluate or print a given expression.

# Fields

- `degree::UInt8`: Degree of the node. 0 for constants, 1 for
    unary operators, 2 for binary operators.
- `constant::Bool`: Whether the node is a constant.
- `val::T`: Value of the node. If `degree==0`, and `constant==true`,
    this is the value of the constant. It has a type specified by the
    overall type of the `Node` (e.g., `Float64`).
- `feature::UInt16`: Index of the feature to use in the
    case of a feature node. Only used if `degree==0` and `constant==false`. 
    Only defined if `degree == 0 && constant == false`.
- `op::UInt8`: If `degree==1`, this is the index of the operator
    in `operators.unaops`. If `degree==2`, this is the index of the
    operator in `operators.binops`. In other words, this is an enum
    of the operators, and is dependent on the specific `OperatorEnum`
    object. Only defined if `degree >= 1`
- `l::Node{T}`: Left child of the node. Only defined if `degree >= 1`.
    Same type as the parent node.
- `r::Node{T}`: Right child of the node. Only defined if `degree == 2`.
    Same type as the parent node. This is to be passed as the right
    argument to the binary operator.
"""
mutable struct Node{T} <: AbstractNode
    degree::UInt8  # 0 for constant/variable, 1 for cos/sin, 2 for +/* etc.
    constant::Bool  # false if variable
    val::Union{T,Nothing}  # If is a constant, this stores the actual value
    # ------------------- (possibly undefined below)
    feature::UInt16  # If is a variable (e.g., x in cos(x)), this stores the feature index.
    op::UInt8  # If operator, this is the index of the operator in operators.binops, or operators.unaops
    l::Node{T}  # Left child node. Only defined for degree=1 or degree=2.
    r::Node{T}  # Right child node. Only defined for degree=2. 

    #################
    ## Constructors:
    #################
    Node(d::Integer, c::Bool, v::_T) where {_T} = new{_T}(UInt8(d), c, v)
    Node(::Type{_T}, d::Integer, c::Bool, v::_T) where {_T} = new{_T}(UInt8(d), c, v)
    Node(::Type{_T}, d::Integer, c::Bool, v::Nothing, f::Integer) where {_T} = new{_T}(UInt8(d), c, v, UInt16(f))
    Node(d::Integer, c::Bool, v::Nothing, f::Integer, o::Integer, l::Node{_T}) where {_T} = new{_T}(UInt8(d), c, v, UInt16(f), UInt8(o), l)
    Node(d::Integer, c::Bool, v::Nothing, f::Integer, o::Integer, l::Node{_T}, r::Node{_T}) where {_T} = new{_T}(UInt8(d), c, v, UInt16(f), UInt8(o), l, r)

end
################################################################################
#! format: on

include("base.jl")

"""
    Node([::Type{T}]; val=nothing, feature::Union{Integer,Nothing}=nothing) where {T}

Create a leaf node: either a constant, or a variable.

# Arguments:

- `::Type{T}`, optionally specify the type of the
    node, if not already given by the type of
    `val`.
- `val`, if you are specifying a constant, pass
    the value of the constant here.
- `feature::Integer`, if you are specifying a variable,
    pass the index of the variable here.
"""
function Node(;
    val::T1=nothing, feature::T2=nothing
)::Node where {T1,T2<:Union{Integer,Nothing}}
    if T1 <: Nothing && T2 <: Nothing
        error("You must specify either `val` or `feature` when creating a leaf node.")
    elseif !(T1 <: Nothing || T2 <: Nothing)
        error(
            "You must specify either `val` or `feature` when creating a leaf node, not both.",
        )
    elseif T2 <: Nothing
        return Node(0, true, val)
    else
        return Node(DEFAULT_NODE_TYPE, 0, false, nothing, feature)
    end
end
function Node(
    ::Type{T}; val::T1=nothing, feature::T2=nothing
)::Node{T} where {T,T1,T2<:Union{Integer,Nothing}}
    if T1 <: Nothing && T2 <: Nothing
        error("You must specify either `val` or `feature` when creating a leaf node.")
    elseif !(T1 <: Nothing || T2 <: Nothing)
        error(
            "You must specify either `val` or `feature` when creating a leaf node, not both.",
        )
    elseif T2 <: Nothing
        if !(T1 <: T)
            # Only convert if not already in the type union.
            val = convert(T, val)
        end
        return Node(T, 0, true, val)
    else
        return Node(T, 0, false, nothing, feature)
    end
end

"""
    Node(op::Integer, l::Node)

Apply unary operator `op` (enumerating over the order given) to `Node` `l`
"""
Node(op::Integer, l::Node{T}) where {T} = Node(1, false, nothing, 0, op, l)

"""
    Node(op::Integer, l::Node, r::Node)

Apply binary operator `op` (enumerating over the order given) to `Node`s `l` and `r`
"""
function Node(op::Integer, l::Node{T1}, r::Node{T2}) where {T1,T2}
    # Get highest type:
    if T1 != T2
        T = promote_type(T1, T2)
        l = convert(Node{T}, l)
        r = convert(Node{T}, r)
    end
    return Node(2, false, nothing, 0, op, l, r)
end

"""
    Node(var_string::String)

Create a variable node, using the format `"x1"` to mean feature 1
"""
Node(var_string::String) = Node(; feature=parse(UInt16, var_string[2:end]))

"""
    Node(var_string::String, variable_names::Array{String, 1})

Create a variable node, using a user-passed format
"""
function Node(var_string::String, variable_names::Array{String,1})
    return Node(;
        feature=[
            i for (i, _variable) in enumerate(variable_names) if _variable == var_string
        ][1]::Int,
    )
end

"""
    set_node!(tree::Node{T}, new_tree::Node{T}) where {T}

Set every field of `tree` equal to the corresponding field of `new_tree`.
"""
function set_node!(tree::Node{T}, new_tree::Node{T}) where {T}
    tree.degree = new_tree.degree
    if new_tree.degree == 0
        tree.constant = new_tree.constant
        if new_tree.constant
            tree.val = new_tree.val::T
        else
            tree.feature = new_tree.feature
        end
    else
        tree.op = new_tree.op
        tree.l = new_tree.l
        if new_tree.degree == 2
            tree.r = new_tree.r
        end
    end
    return nothing
end

const OP_NAMES = Dict(
    "safe_log" => "log",
    "safe_log2" => "log2",
    "safe_log10" => "log10",
    "safe_log1p" => "log1p",
    "safe_acosh" => "acosh",
    "safe_sqrt" => "sqrt",
    "safe_pow" => "^",
)

dispatch_op_name(::Val{2}, ::Nothing, idx) = "binary_operator[" * string(idx) * ']'
dispatch_op_name(::Val{1}, ::Nothing, idx) = "unary_operator[" * string(idx) * ']'
function dispatch_op_name(::Val{2}, operators::AbstractOperatorEnum, idx)
    return get_op_name(operators.binops[idx])
end
function dispatch_op_name(::Val{1}, operators::AbstractOperatorEnum, idx)
    return get_op_name(operators.unaops[idx])
end

@generated function get_op_name(op::F) where {F}
    try
        # Bit faster to just cache the name of the operator:
        op_s = string(F.instance)
        out = get(OP_NAMES, op_s, op_s)
        return :($out)
    catch
    end
    return quote
        op_s = string(op)
        out = get(OP_NAMES, op_s, op_s)
        return out
    end
end

@inline function strip_brackets(s)
    if startswith(s, '(') && endswith(s, ')')
        start_cut = nextind(s, firstindex(s))
        end_cut = prevind(s, lastindex(s))
        return s[start_cut:end_cut]
    else
        return s
    end
end

# Can overload these for custom behavior:
needs_brackets(val::Real) = false
needs_brackets(val::AbstractArray) = false
needs_brackets(val::Complex) = true
needs_brackets(val) = true

function string_constant(val)
    if needs_brackets(val)
        "(" * string(val) * ")"
    else
        string(val)
    end
end

function string_variable(feature, variable_names)
    if variable_names === nothing || feature > lastindex(variable_names)
        return "x" * string(feature)
    else
        return variable_names[feature]
    end
end

"""
    string_tree(tree::Node, operators::AbstractOperatorEnum[; bracketed, variable_names, f_variable, f_constant])

Convert an equation to a string.

# Arguments
- `tree`: the tree to convert to a string
- `operators`: the operators used to define the tree

# Keyword Arguments
- `f_variable`: (optional) function to convert a variable to a string, with arguments `(feature::UInt8, variable_names)`.
- `f_constant`: (optional) function to convert a constant to a string, with arguments `(val,)`
- `variable_names::Union{Array{String, 1}, Nothing}=nothing`: (optional) what variables to print for each feature.
"""
function string_tree(
    tree::Node{T},
    operators::Union{AbstractOperatorEnum,Nothing}=nothing;
    f_variable::F1=string_variable,
    f_constant::F2=string_constant,
    variable_names::Union{Array{String,1},Nothing}=nothing,
    preserve_sharing=false,
    # Deprecated
    varMap=nothing,
)::String where {T,F1<:Function,F2<:Function}
    variable_names = deprecate_varmap(variable_names, varMap, :string_tree)
    raw_output = tree_mapreduce(
        leaf -> if leaf.constant
            f_constant(leaf.val::T)
        else
            f_variable(leaf.feature, variable_names)
        end,
        branch -> if branch.degree == 1
            dispatch_op_name(Val(1), operators, branch.op)
        else
            dispatch_op_name(Val(2), operators, branch.op)
        end,
        (parent, children...) ->
            if length(children) > 1 && parent in ("+", "-", "*", "/", "^")
                '(' * join(children, ' ' * parent * ' ') * ')'
            else
                children = map(strip_brackets, children)
                parent * '(' * join(children, ", ") * ')'
            end,
        tree,
        String;
        preserve_sharing,
        f_on_shared=(c, is_shared) -> is_shared ? '{' * c * '}' : c,
    )
    return strip_brackets(raw_output)
end

# Print an equation
function print_tree(
    io::IO,
    tree::Node,
    operators::AbstractOperatorEnum;
    f_variable::F1=string_variable,
    f_constant::F2=string_constant,
    variable_names::Union{Array{String,1},Nothing}=nothing,
    # Deprecated
    varMap=nothing,
) where {F1<:Function,F2<:Function}
    variable_names = deprecate_varmap(variable_names, varMap, :print_tree)
    return println(io, string_tree(tree, operators; f_variable, f_constant, variable_names))
end

function print_tree(
    tree::Node,
    operators::AbstractOperatorEnum;
    f_variable::F1=string_variable,
    f_constant::F2=string_constant,
    variable_names::Union{Array{String,1},Nothing}=nothing,
    # Deprecated
    varMap=nothing,
) where {F1<:Function,F2<:Function}
    variable_names = deprecate_varmap(variable_names, varMap, :print_tree)
    return println(string_tree(tree, operators; f_variable, f_constant, variable_names))
end

end
