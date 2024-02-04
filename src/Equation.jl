module EquationModule

import ..OperatorEnumModule: AbstractOperatorEnum
import ..UtilsModule: @memoize_on, @with_memoize, deprecate_varmap, Undefined

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

"""
    AbstractExpressionNode{T} <: AbstractNode

Abstract type for nodes that represent an expression.
Along with the fields required for `AbstractNode`,
this additionally must have fields for:

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
```
"""
abstract type AbstractExpressionNode{T} <: AbstractNode end

#! format: off
"""
    Node{T} <: AbstractExpressionNode{T}

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

# Constructors

## Leafs

    Node(; val=nothing, feature::Union{Integer,Nothing}=nothing)
    Node{T}(; val=nothing, feature::Union{Integer,Nothing}=nothing) where {T}

Create a leaf node: either a constant, or a variable.

- `::Type{T}`, optionally specify the type of the
    node, if not already given by the type of
    `val`.
- `val`, if you are specifying a constant, pass
    the value of the constant here.
- `feature::Integer`, if you are specifying a variable,
    pass the index of the variable here.

You can also create a leaf node from variable names:

    Node(; var_string::String, variable_names::Array{String,1})
    Node{T}(; var_string::String, variable_names::Array{String,1}) where {T}

## Unary operator

    Node(op::Integer, l::Node)

Apply unary operator `op` (enumerating over the order given in `OperatorEnum`)
to `Node` `l`.

## Binary operator

    Node(op::Integer, l::Node, r::Node)

Apply binary operator `op` (enumerating over the order given in `OperatorEnum`)
to `Node`s `l` and `r`.
"""
mutable struct Node{T} <: AbstractExpressionNode{T}
    degree::UInt8  # 0 for constant/variable, 1 for cos/sin, 2 for +/* etc.
    constant::Bool  # false if variable
    val::T  # If is a constant, this stores the actual value
    # ------------------- (possibly undefined below)
    feature::UInt16  # If is a variable (e.g., x in cos(x)), this stores the feature index.
    op::UInt8  # If operator, this is the index of the operator in operators.binops, or operators.unaops
    l::Node{T}  # Left child node. Only defined for degree=1 or degree=2.
    r::Node{T}  # Right child node. Only defined for degree=2. 

    #################
    ## Constructors:
    #################
    Node{_T}() where {_T} = new{_T}()
end

"""
    GraphNode{T} <: AbstractExpressionNode{T}

Exactly the same as `Node{T}`, but with the assumption that some
nodes will be shared. All copies of this graph-like structure will
be performed with this assumption, to preserve structure of the graph.

# Examples

```julia
julia> operators = OperatorEnum(;
           binary_operators=[+, -, *], unary_operators=[cos, sin]
       );

julia> x = GraphNode(feature=1)
x1

julia> y = sin(x) + x
sin(x1) + {x1}

julia> cos(y) * y
cos(sin(x1) + {x1}) * {(sin(x1) + {x1})}
```

Note how the `{}` indicates a node is shared, and this
is the same node as seen earlier in the string.

This has the same constructors as `Node{T}`. Shared nodes
are created simply by using the same node in multiple places
when constructing or setting properties.
"""
mutable struct GraphNode{T} <: AbstractExpressionNode{T}
    degree::UInt8  # 0 for constant/variable, 1 for cos/sin, 2 for +/* etc.
    constant::Bool  # false if variable
    val::T  # If is a constant, this stores the actual value
    # ------------------- (possibly undefined below)
    feature::UInt16  # If is a variable (e.g., x in cos(x)), this stores the feature index.
    op::UInt8  # If operator, this is the index of the operator in operators.binops, or operators.unaops
    l::GraphNode{T}  # Left child node. Only defined for degree=1 or degree=2.
    r::GraphNode{T}  # Right child node. Only defined for degree=2. 

    GraphNode{_T}() where {_T} = new{_T}()
end

################################################################################
#! format: on

Base.eltype(::Type{<:AbstractExpressionNode{T}}) where {T} = T
Base.eltype(::AbstractExpressionNode{T}) where {T} = T

constructorof(::Type{N}) where {N<:AbstractNode} = Base.typename(N).wrapper
constructorof(::Type{<:Node}) = Node
constructorof(::Type{<:GraphNode}) = GraphNode

function with_type_parameters(::Type{N}, ::Type{T}) where {N<:AbstractExpressionNode,T}
    return constructorof(N){T}
end
with_type_parameters(::Type{<:Node}, ::Type{T}) where {T} = Node{T}
with_type_parameters(::Type{<:GraphNode}, ::Type{T}) where {T} = GraphNode{T}

function default_allocator(::Type{N}, ::Type{T}) where {N<:AbstractExpressionNode,T}
    return with_type_parameters(N, T)()
end
default_allocator(::Type{<:Node}, ::Type{T}) where {T} = Node{T}()
default_allocator(::Type{<:GraphNode}, ::Type{T}) where {T} = GraphNode{T}()

"""Trait declaring whether nodes share children or not."""
preserve_sharing(::Type{<:AbstractNode}) = false
preserve_sharing(::Type{<:Node}) = false
preserve_sharing(::Type{<:GraphNode}) = true

include("base.jl")

@inline function (::Type{N})(
    ::Type{T1}=Undefined;
    val=nothing,
    feature=nothing,
    op=nothing,
    l=nothing,
    r=nothing,
    allocator=default_allocator,
) where {T1,N<:AbstractExpressionNode}
    return node_factory(N, T1, val, feature, op, l, r, allocator)
end

"""Create a constant leaf."""
@inline function node_factory(
    ::Type{N}, ::Type{T1}, val::T2, ::Nothing, ::Nothing, ::Nothing, ::Nothing, allocator
) where {N,T1,T2}
    T = node_factory_type(N, T1, T2)
    n = allocator(N, T)
    n.degree = 0
    n.constant = true
    n.val = convert(T, val)
    return n
end
"""Create a variable leaf, to store data."""
@inline function node_factory(
    ::Type{N},
    ::Type{T1},
    ::Nothing,
    feature::Integer,
    ::Nothing,
    ::Nothing,
    ::Nothing,
    allocator,
) where {N,T1}
    T = node_factory_type(N, T1, DEFAULT_NODE_TYPE)
    n = allocator(N, T)
    n.degree = 0
    n.constant = false
    n.feature = feature
    return n
end
"""Create a unary operator node."""
@inline function node_factory(
    ::Type{N},
    ::Type{T1},
    ::Nothing,
    ::Nothing,
    op::Integer,
    l::AbstractExpressionNode{T2},
    ::Nothing,
    allocator,
) where {N,T1,T2}
    @assert l isa N
    T = T2  # Always prefer existing nodes, so we don't mess up references from conversion
    n = allocator(N, T)
    n.degree = 1
    n.op = op
    n.l = l
    return n
end
"""Create a binary operator node."""
@inline function node_factory(
    ::Type{N},
    ::Type{T1},
    ::Nothing,
    ::Nothing,
    op::Integer,
    l::AbstractExpressionNode{T2},
    r::AbstractExpressionNode{T3},
    allocator,
) where {N,T1,T2,T3}
    T = promote_type(T2, T3)
    n = allocator(N, T)
    n.degree = 2
    n.op = op
    n.l = T2 === T ? l : convert(with_type_parameters(N, T), l)
    n.r = T3 === T ? r : convert(with_type_parameters(N, T), r)
    return n
end
@inline function node_factory_type(::Type{N}, ::Type{T1}, ::Type{T2}) where {N,T1,T2}
    if T1 === Undefined && N isa UnionAll
        T2
    elseif T1 === Undefined
        eltype(N)
    elseif N isa UnionAll
        T1
    else
        eltype(N)
    end
end

function (::Type{N})(
    op::Integer, l::AbstractExpressionNode
) where {N<:AbstractExpressionNode}
    return N(; op=op, l=l)
end
function (::Type{N})(
    op::Integer, l::AbstractExpressionNode, r::AbstractExpressionNode
) where {N<:AbstractExpressionNode}
    return N(; op=op, l=l, r=r)
end
function (::Type{N})(var_string::String) where {N<:AbstractExpressionNode}
    Base.depwarn(
        "Creating a node using a string is deprecated and will be removed in a future version.",
        :string_tree,
    )
    return N(; feature=parse(UInt16, var_string[2:end]))
end
function (::Type{N})(
    var_string::String, variable_names::Array{String,1}
) where {N<:AbstractExpressionNode}
    i = findfirst(==(var_string), variable_names)::Int
    return N(; feature=i)
end

function Base.promote_rule(::Type{Node{T1}}, ::Type{Node{T2}}) where {T1,T2}
    return Node{promote_type(T1, T2)}
end
function Base.promote_rule(::Type{GraphNode{T1}}, ::Type{Node{T2}}) where {T1,T2}
    return GraphNode{promote_type(T1, T2)}
end
function Base.promote_rule(::Type{GraphNode{T1}}, ::Type{GraphNode{T2}}) where {T1,T2}
    return GraphNode{promote_type(T1, T2)}
end

# TODO: Verify using this helps with garbage collection
create_dummy_node(::Type{N}) where {N<:AbstractExpressionNode} = N()

"""
    set_node!(tree::AbstractExpressionNode{T}, new_tree::AbstractExpressionNode{T}) where {T}

Set every field of `tree` equal to the corresponding field of `new_tree`.
"""
function set_node!(tree::AbstractExpressionNode, new_tree::AbstractExpressionNode)
    # First, ensure we free some memory:
    if new_tree.degree < 2 && tree.degree == 2
        tree.r = create_dummy_node(typeof(tree))
    end
    if new_tree.degree < 1 && tree.degree >= 1
        tree.l = create_dummy_node(typeof(tree))
    end

    tree.degree = new_tree.degree
    if new_tree.degree == 0
        tree.constant = new_tree.constant
        if new_tree.constant
            tree.val = new_tree.val::eltype(new_tree)
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

end
