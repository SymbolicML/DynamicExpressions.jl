module NodeModule

using DispatchDoctor: @unstable

import ..OperatorEnumModule: AbstractOperatorEnum
import ..UtilsModule: deprecate_varmap, Undefined

const DEFAULT_NODE_TYPE = Float32

"""
    AbstractNode{D}

Abstract type for D-arity trees. Must have the following fields:

- `degree::Integer`: Degree of the node. Either 0, 1, or 2. If 1,
    then `l` needs to be defined as the left child. If 2,
    then `r` also needs to be defined as the right child.
- `children`: A collection of D references to children nodes.

# Deprecated fields

- `l::AbstractNode{D}`: Left child of the current node. Should only be
    defined if `degree >= 1`; otherwise, leave it undefined (see the
    the constructors of [`Node{T}`](@ref) for an example).
    Don't use `nothing` to represent an undefined value
    as it will incur a large performance penalty.
- `r::AbstractNode{D}`: Right child of the current node. Should only
    be defined if `degree == 2`.
"""
abstract type AbstractNode{D} end

"""
    AbstractExpressionNode{T,D} <: AbstractNode{D}

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

# Interface

See [`NodeInterface`](@ref DynamicExpressions.InterfacesModule.NodeInterface) for a full description
of the interface implementation, as well as tests to verify correctness.

You *must* define `CustomNode{_T} where {_T} = new{_T}()` for each custom node type.

In addition, you *may* choose to define the following functions, to override
the defaults behavior, in particular if you wish to add additional fields
to your type.

- `leaf_copy` and `branch_copy`
- `leaf_equal` and `branch_equal`
- `leaf_hash` and `branch_hash`
- `preserve_sharing`

You likely do not need to, but you could choose to override the following:

- `constructorof`
- `with_type_parameters`

"""
abstract type AbstractExpressionNode{T,D} <: AbstractNode{D} end

for N in (:Node, :GraphNode)
    @eval mutable struct $N{T,D} <: AbstractExpressionNode{T,D}
        degree::UInt8  # 0 for constant/variable, 1 for cos/sin, 2 for +/* etc.
        constant::Bool  # false if variable
        val::T  # If is a constant, this stores the actual value
        feature::UInt16  # (Possibly undefined) If is a variable (e.g., x in cos(x)), this stores the feature index.
        op::UInt8  # (Possibly undefined) If operator, this is the index of the operator in the degree-specific operator enum
        children::NTuple{D,Base.RefValue{$N{T,D}}}  # Children nodes

        #################
        ## Constructors:
        #################
        $N{_T,_D}() where {_T,_D} = new{_T,_D::Int}()
    end
end

#! format: off
"""
    Node{T,D} <: AbstractExpressionNode{T,D}

Node defines a symbolic expression stored in a binary tree.
A single `Node` instance is one "node" of this tree, and
has references to its children. By tracing through the children
nodes, you can evaluate or print a given expression.

# Fields

- `degree::UInt8`: Degree of the node. 0 for constants, 1 for
    unary operators, 2 for binary operators, etc. Maximum of `D`.
- `constant::Bool`: Whether the node is a constant.
- `val::T`: Value of the node. If `degree==0`, and `constant==true`,
    this is the value of the constant. It has a type specified by the
    overall type of the `Node` (e.g., `Float64`).
- `feature::UInt16`: Index of the feature to use in the
    case of a feature node. Only defined if `degree == 0 && constant == false`.
- `op::UInt8`: If `degree==1`, this is the index of the operator
    in `operators.unaops`. If `degree==2`, this is the index of the
    operator in `operators.binops`. In other words, this is an enum
    of the operators, and is dependent on the specific `OperatorEnum`
    object. Only defined if `degree >= 1`
- `children::NTuple{D,Base.RefValue{Node{T,D}}}`: Children of the node. Only defined up to `degree`

# Constructors


    Node([T]; val=nothing, feature=nothing, op=nothing, children=nothing, allocator=default_allocator)
    Node{T}(; val=nothing, feature=nothing, op=nothing, children=nothing, allocator=default_allocator)

Create a new node in an expression tree. If `T` is not specified in either the type or the
first argument, it will be inferred from the value of `val` passed or the children.
The `children` keyword is used to pass in a collection of children nodes.

You may also construct nodes via the convenience operators generated by creating an `OperatorEnum`.

You may also choose to specify a default memory allocator for the node other than simply `Node{T}()`
in the `allocator` keyword argument.
"""
Node


"""
    GraphNode{T,D} <: AbstractExpressionNode{T,D}

Exactly the same as [`Node{T,D}`](@ref), but with the assumption that some
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

This has the same constructors as [`Node{T}`](@ref). Shared nodes
are created simply by using the same node in multiple places
when constructing or setting properties.
"""
GraphNode

@inline function Base.getproperty(n::Union{Node,GraphNode}, k::Symbol)
    if k == :l
        # TODO: Should a depwarn be raised here? Or too slow?
        return getfield(n, :children)[1][]
    elseif k == :r
        return getfield(n, :children)[2][]
    else
        return getfield(n, k)
    end
end
#! format: off
@inline function Base.setproperty!(n::Union{Node,GraphNode}, k::Symbol, v)
    if k == :l
        # TODO: Should a depwarn be raised here? Or too slow?
        if isdefined(n, :children)
            getfield(n, :children)[1][] = v
        else
            setfield!(n, :children, ntuple(i -> i == 1 ? Ref(v) : Ref{typeof(n)}(), Val(max_degree(typeof(n)))))
            v
        end
    elseif k == :r
        if isdefined(n, :children)
            getfield(n, :children)[2][] = v
        else
            setfield!(n, :children, ntuple(i -> i == 2 ? Ref(v) : Ref{typeof(n)}(), Val(max_degree(typeof(n)))))
            v
        end
    elseif k == :degree
        setfield!(n, :degree, convert(UInt8, v))
    elseif k == :constant
        setfield!(n, :constant, convert(Bool, v))
    elseif k == :feature
        setfield!(n, :feature, convert(UInt16, v))
    elseif k == :op
        setfield!(n, :op, convert(UInt8, v))
    elseif k == :val
        setfield!(n, :val, convert(eltype(n), v))
    elseif k == :children
        setfield!(n, :children, v)
    else
        error("Invalid property: $k")
    end
end
#! format: on

################################################################################
#! format: on

Base.eltype(::Type{<:AbstractExpressionNode{T}}) where {T} = T
Base.eltype(::AbstractExpressionNode{T}) where {T} = T

max_degree(::Type{<:AbstractNode}) = 2  # Default
max_degree(::Type{<:AbstractNode{D}}) where {D} = D

@unstable constructorof(::Type{N}) where {N<:Node} = Node{T,max_degree(N)} where {T}
@unstable constructorof(::Type{N}) where {N<:GraphNode} =
    GraphNode{T,max_degree(N)} where {T}

with_type_parameters(::Type{N}, ::Type{T}) where {N<:Node,T} = Node{T,max_degree(N)}
function with_type_parameters(::Type{N}, ::Type{T}) where {N<:GraphNode,T}
    return GraphNode{T,max_degree(N)}
end

# with_degree(::Type{N}, ::Val{D}) where {T,N<:Node{T},D} = Node{T,D}
# with_degree(::Type{N}, ::Val{D}) where {T,N<:GraphNode{T},D} = GraphNode{T,D}

function default_allocator(::Type{N}, ::Type{T}) where {N<:AbstractExpressionNode,T}
    return with_type_parameters(N, T)()
end

"""Trait declaring whether nodes share children or not."""
preserve_sharing(::Union{Type{<:AbstractNode},AbstractNode}) = false
preserve_sharing(::Union{Type{<:GraphNode},GraphNode}) = true

include("base.jl")

#! format: off
@inline function (::Type{N})(
    ::Type{T1}=Undefined; val=nothing, feature=nothing, op=nothing, l=nothing, r=nothing, children=nothing, allocator::F=default_allocator,
) where {T1,N<:AbstractExpressionNode{T} where T,F}
    _children = if l !== nothing && r === nothing
        @assert children === nothing
        (l,)
    elseif l !== nothing && r !== nothing
        @assert children === nothing
        (l, r)
    else
        children
    end
    validate_not_all_defaults(N, val, feature, op, _children)
    return node_factory(N, T1, val, feature, op, _children, allocator)
end
function validate_not_all_defaults(::Type{N}, val, feature, op, children) where {N<:AbstractExpressionNode}
    return nothing
end
function validate_not_all_defaults(::Type{N}, val, feature, op, children) where {T,N<:AbstractExpressionNode{T}}
    if val === nothing && feature === nothing && op === nothing && children === nothing
        error(
            "Encountered the call for $N() inside the generic constructor. "
            * "Did you forget to define `$(Base.typename(N).wrapper){T,D}() where {T,D} = new{T,D}()`?"
        )
    end
    return nothing
end
"""Create a constant leaf."""
@inline function node_factory(
    ::Type{N}, ::Type{T1}, val::T2, ::Nothing, ::Nothing, ::Nothing, allocator::F,
) where {N,T1,T2,F}
    T = node_factory_type(N, T1, T2)
    n = allocator(N, T)
    n.degree = 0
    n.constant = true
    n.val = convert(T, val)
    return n
end
"""Create a variable leaf, to store data."""
@inline function node_factory(
    ::Type{N}, ::Type{T1}, ::Nothing, feature::Integer, ::Nothing, ::Nothing, allocator::F,
) where {N,T1,F}
    T = node_factory_type(N, T1, DEFAULT_NODE_TYPE)
    n = allocator(N, T)
    n.degree = 0
    n.constant = false
    n.feature = feature
    return n
end
"""Create an operator node."""
@inline function node_factory(
    ::Type{N}, ::Type, ::Nothing, ::Nothing, op::Integer, children::Tuple, allocator::F,
) where {N<:AbstractExpressionNode,F}
    T = promote_type(map(eltype, children)...)  # Always prefer existing nodes, so we don't mess up references from conversion
    D2 = length(children)
    @assert D2 <= max_degree(N)
    NT = with_type_parameters(N, T)
    n = allocator(N, T)
    n.degree = D2
    n.op = op
    n.children = ntuple(i -> i <= D2 ? Ref(convert(NT, children[i])) : Ref{NT}(), Val(max_degree(N)))
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
#! format: on

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
    var_string::String, variable_names::AbstractVector{String}
) where {N<:AbstractExpressionNode}
    i = findfirst(==(var_string), variable_names)::Int
    return N(; feature=i)
end

function Base.promote_rule(::Type{Node{T1,D}}, ::Type{Node{T2,D}}) where {T1,T2,D}
    return Node{promote_type(T1, T2),D}
end
function Base.promote_rule(::Type{GraphNode{T1,D}}, ::Type{Node{T2,D}}) where {T1,T2,D}
    return GraphNode{promote_type(T1, T2),D}
end
function Base.promote_rule(::Type{GraphNode{T1,D}}, ::Type{GraphNode{T2,D}}) where {T1,T2,D}
    return GraphNode{promote_type(T1, T2),D}
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
