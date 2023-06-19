module EquationModule

import ..OperatorEnumModule: AbstractOperatorEnum
import ..UtilsModule: @memoize_on, @with_memoize, deprecate_varmap

const DEFAULT_NODE_TYPE = Float32

"""
    Node{T}

Node defines a symbolic expression stored in a binary tree.
A single `Node` instance is one "node" of this tree, and
has references to its children. By tracing through the children
nodes, you can evaluate or print a given expression.

# Fields

- `degree::Int`: Degree of the node. 0 for constants, 1 for
    unary operators, 2 for binary operators.
- `constant::Bool`: Whether the node is a constant.
- `val::T`: Value of the node. If `degree==0`, and `constant==true`,
    this is the value of the constant. It has a type specified by the
    overall type of the `Node` (e.g., `Float64`).
- `feature::Int` (optional): Index of the feature to use in the
    case of a feature node. Only used if `degree==0` and `constant==false`. 
    Only defined if `degree == 0 && constant == false`.
- `op::Int`: If `degree==1`, this is the index of the operator
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
mutable struct Node{T}
    degree::Int  # 0 for constant/variable, 1 for cos/sin, 2 for +/* etc.
    constant::Bool  # false if variable
    val::Union{T,Nothing}  # If is a constant, this stores the actual value
    # ------------------- (possibly undefined below)
    feature::Int  # If is a variable (e.g., x in cos(x)), this stores the feature index.
    op::Int  # If operator, this is the index of the operator in operators.binary_operators, or operators.unary_operators
    l::Node{T}  # Left child node. Only defined for degree=1 or degree=2.
    r::Node{T}  # Right child node. Only defined for degree=2. 

    #################
    ## Constructors:
    #################
    Node(d::Int, c::Bool, v::_T) where {_T} = new{_T}(d, c, v)
    Node(::Type{_T}, d::Int, c::Bool, v::_T) where {_T} = new{_T}(d, c, v)
    Node(::Type{_T}, d::Int, c::Bool, v::Nothing, f::Int) where {_T} = new{_T}(d, c, v, f)
    function Node(d::Int, c::Bool, v::Nothing, f::Int, o::Int, l::Node{_T}) where {_T}
        return new{_T}(d, c, v, f, o, l)
    end
    function Node(
        d::Int, c::Bool, v::Nothing, f::Int, o::Int, l::Node{_T}, r::Node{_T}
    ) where {_T}
        return new{_T}(d, c, v, f, o, l, r)
    end
end
################################################################################

include("base.jl")

"""
    Node([::Type{T}]; val=nothing, feature::Int=nothing) where {T}

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
    Node(op::Int, l::Node)

Apply unary operator `op` (enumerating over the order given) to `Node` `l`
"""
Node(op::Int, l::Node{T}) where {T} = Node(1, false, nothing, 0, op, l)

"""
    Node(op::Int, l::Node, r::Node)

Apply binary operator `op` (enumerating over the order given) to `Node`s `l` and `r`
"""
function Node(op::Int, l::Node{T1}, r::Node{T2}) where {T1,T2}
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
Node(var_string::String) = Node(; feature=parse(Int, var_string[2:end]))

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

function get_op_name(op::String)
    return get(OP_NAMES, op, op)
end

function string_op(
    ::Val{2}, op::F, tree::Node, args...; bracketed, kws...
)::String where {F}
    op_name = get_op_name(string(op))
    if op_name in ["+", "-", "*", "/", "^"]
        l = string_tree(tree.l, args...; bracketed=false, kws...)
        r = string_tree(tree.r, args...; bracketed=false, kws...)
        if bracketed
            return "$l $op_name $r"
        else
            return "($l $op_name $r)"
        end
    else
        l = string_tree(tree.l, args...; bracketed=true, kws...)
        r = string_tree(tree.r, args...; bracketed=true, kws...)
        return "$op_name($l, $r)"
    end
end
function string_op(
    ::Val{1}, op::F, tree::Node, args...; bracketed, kws...
)::String where {F}
    op_name = get_op_name(string(op))
    l = string_tree(tree.l, args...; bracketed=true, kws...)
    return "$(op_name)($l)"
end

function string_constant(val, bracketed::Bool)
    does_not_need_brackets = (typeof(val) <: Union{Real,AbstractArray})
    if does_not_need_brackets || bracketed
        string(val)
    else
        "(" * string(val) * ")"
    end
end

function string_variable(feature, variable_names)
    if variable_names === nothing
        return "x$(feature)"
    else
        return @inbounds(variable_names[feature])
    end
end

"""
    string_tree(tree::Node, operators::AbstractOperatorEnum[; bracketed, variable_names, f_variable, f_constant])

Convert an equation to a string.

# Arguments
- `tree`: the tree to convert to a string
- `operators`: the operators used to define the tree

# Keyword Arguments
- `bracketed`: (optional) whether to put brackets around the outside.
- `f_variable`: (optional) function to convert a variable to a string, of the form `(feature::Int, variable_names)`.
- `f_constant`: (optional) function to convert a constant to a string, of the form `(val, bracketed::Bool)`
- `variable_names::Union{Array{String, 1}, Nothing}=nothing`: (optional) what variables to print for each feature.
"""
function string_tree(
    tree::Node{T},
    operators::AbstractOperatorEnum;
    bracketed::Bool=false,
    f_variable::F1=string_variable,
    f_constant::F2=string_constant,
    variable_names::Union{Array{String,1},Nothing}=nothing,
    # Deprecated
    varMap=nothing,
)::String where {T,F1<:Function,F2<:Function}
    variable_names = deprecate_varmap(variable_names, varMap, :string_tree)
    if tree.degree == 0
        if !tree.constant
            return f_variable(tree.feature, variable_names)
        else
            return f_constant(tree.val::T, bracketed)
        end
    elseif tree.degree == 1
        return string_op(
            Val(1),
            operators.unaops[tree.op],
            tree,
            operators;
            bracketed,
            f_variable,
            f_constant,
            variable_names,
        )
    else
        return string_op(
            Val(2),
            operators.binops[tree.op],
            tree,
            operators;
            bracketed,
            f_variable,
            f_constant,
            variable_names,
        )
    end
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
