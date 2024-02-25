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


    Node([T]; val=nothing, feature=nothing, op=nothing, l=nothing, r=nothing, children=nothing, allocator=default_allocator)
    Node{T}(; val=nothing, feature=nothing, op=nothing, l=nothing, r=nothing, children=nothing, allocator=default_allocator)

Create a new node in an expression tree. If `T` is not specified in either the type or the
first argument, it will be inferred from the value of `val` passed or `l` and/or `r`.
If it cannot be inferred from these, it will default to `Float32`.

The `children` keyword can be used instead of `l` and `r` and should be a tuple of children. This
is to permit the use of splatting in constructors.

You may also construct nodes via the convenience operators generated by creating an `OperatorEnum`.

You may also choose to specify a default memory allocator for the node other than simply `Node{T}()`
in the `allocator` keyword argument.
"""
mutable struct Node{T} <: AbstractExpressionNode{T}
    const degree::UInt8  # 0 for constant/variable, 1 for cos/sin, 2 for +/* etc.
    const constant::Union{Bool,Nothing}  # false if variable
    val::Union{T,Nothing}  # If is a constant, this stores the actual value
    const feature::Union{UInt16,Nothing}  # If is a variable (e.g., x in cos(x)), this stores the feature index.
    const op::Union{UInt8,Nothing}  # If operator, this is the index of the operator in operators.binops, or operators.unaops
    l::Union{Node{T},Nothing}  # Left child node. Only defined for degree=1 or degree=2.
    r::Union{Node{T},Nothing}  # Right child node. Only defined for degree=2. 
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
    const degree::UInt8  # 0 for constant/variable, 1 for cos/sin, 2 for +/* etc.
    const constant::Union{Bool,Nothing}  # false if variable
    val::Union{T,Nothing}  # If is a constant, this stores the actual value
    const feature::Union{UInt16,Nothing}  # If is a variable (e.g., x in cos(x)), this stores the feature index.
    const op::Union{UInt8,Nothing}  # If operator, this is the index of the operator in operators.binops, or operators.unaops
    l::GraphNode{T}  # Left child node. Only defined for degree=1 or degree=2.
    r::GraphNode{T}  # Right child node. Only defined for degree=2. 
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

default_allocator(::Type{N}, args...) where {T,N<:AbstractExpressionNode{T}} = N(args...)

"""Trait declaring whether nodes share children or not."""
preserve_sharing(::Type{<:AbstractNode}) = false
preserve_sharing(::Type{<:Node}) = false
preserve_sharing(::Type{<:GraphNode}) = true

include("base.jl")

#! format: off
@inline function (::Type{N})(
    ::Type{T1}=Undefined; val=nothing, feature=nothing, op=nothing, l=nothing, r=nothing, children=nothing, allocator::F=default_allocator,
) where {T1,N<:AbstractExpressionNode,F}
    if children !== nothing
        @assert l === nothing && r === nothing
        if length(children) == 1
            return node_factory(N, T1, val, feature, op, only(children), nothing, allocator)
        else
            return node_factory(N, T1, val, feature, op, children..., allocator)
        end
    end
    return node_factory(N, T1, val, feature, op, l, r, allocator)
end
"""Create a constant leaf."""
@inline function node_factory(
    ::Type{N}, ::Type{T1}, val::T2, ::Nothing, ::Nothing, ::Nothing, ::Nothing, allocator::F,
) where {N,T1,T2,F}
    T = node_factory_type(N, T1, T2)
    NT = with_type_parameters(N, T)
    return allocator(NT, 0, true, convert(T, val), nothing, nothing, nothing, nothing)
end
"""Create a variable leaf, to store data."""
@inline function node_factory(
    ::Type{N}, ::Type{T1}, ::Nothing, feature::Integer, ::Nothing, ::Nothing, ::Nothing, allocator::F,
) where {N,T1,F}
    T = node_factory_type(N, T1, DEFAULT_NODE_TYPE)
    NT = with_type_parameters(N, T)
    return allocator(NT, 0, false, nothing, feature, nothing, nothing, nothing)
end
"""Create a unary operator node."""
@inline function node_factory(
    ::Type{N}, ::Type{T1}, ::Nothing, ::Nothing, op::Integer, l::AbstractExpressionNode{T2}, ::Nothing, allocator::F,
) where {N,T1,T2,F}
    @assert l isa N
    T = T2  # Always prefer existing nodes, so we don't mess up references from conversion
    NT = with_type_parameters(N, T)
    return allocator(NT, 1, nothing, nothing, nothing, op, l, nothing)
end
"""Create a binary operator node."""
@inline function node_factory(
    ::Type{N}, ::Type{T1}, ::Nothing, ::Nothing, op::Integer, l::AbstractExpressionNode{T2}, r::AbstractExpressionNode{T3}, allocator::F,
) where {N,T1,T2,T3,F}
    T = promote_type(T2, T3)
    NT = with_type_parameters(N, T)
    l = T2 === T ? l : convert(NT, l)
    r = T3 === T ? r : convert(NT, r)
    return allocator(NT, 2, nothing, nothing, nothing, op, l, r)
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
#! format: on

@inline function Base.getproperty(node::AbstractExpressionNode{T}, name::Symbol) where {T}
    # We assert that the field is always defined, for type stability in code
    if name == :constant
        return getfield(node, :constant)::Bool
    elseif name == :val
        return getfield(node, :val)::T
    elseif name == :feature
        return getfield(node, :feature)::UInt16
    elseif name == :op
        return getfield(node, :op)::UInt8
    elseif name in (:l, :r)
        return getfield(node, name)::typeof(node)
    else
        return getfield(node, name)
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

end
