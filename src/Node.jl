module NodeModule

using DispatchDoctor: @unstable

import ..UtilsModule: deprecate_varmap, Undefined, Nullable

const DEFAULT_NODE_TYPE = Float32
const DEFAULT_MAX_DEGREE = 2

"""
    AbstractNode{D}

Abstract type for trees which can have up to `D` children per node.
Must have the following fields:

- `degree::UInt8`: Degree of the node. This should be a value
    between 0 and `D`, inclusive.
- `children`: A collection of up to `D` children nodes. The number
    of children which are _active_ is given by the `degree` field,
    but for type stability reasons, you can have inactive children.

# Deprecated fields

- `l::AbstractNode{D}`: Left child of the current node. Should only be
    defined if `degree >= 1`; otherwise, leave it undefined (see the
    the constructors of [`Node{T}`](@ref) for an example).
    Don't use `nothing` to represent an undefined value
    as it will incur a large performance penalty.
- `r::AbstractNode{D}`: Right child of the current node. Should only
    be defined if `degree >= 2`.
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

You *must* define `CustomNode{_T,_D}() where {_T,_D} = new{_T,_D}()` for each custom node type,
as well as `constructorof` and `with_type_parameters`.

In addition, you *may* choose to define the following functions, to override
the defaults behavior, in particular if you wish to add additional fields
to your type.

- `leaf_copy` and `branch_copy`
- `leaf_convert` and `branch_convert`
- `leaf_equal` and `branch_equal`
- `leaf_hash` and `branch_hash`
- `preserve_sharing`
"""
abstract type AbstractExpressionNode{T,D} <: AbstractNode{D} end

for N in (:Node, :GraphNode)
    @eval mutable struct $N{T,D} <: AbstractExpressionNode{T,D}
        degree::UInt8  # 0 for constant/variable, 1 for cos/sin, 2 for +/* etc.
        constant::Bool  # false if variable
        val::T  # If is a constant, this stores the actual value
        feature::UInt16  # (Possibly undefined) If is a variable (e.g., x in cos(x)), this stores the feature index.
        op::UInt8  # (Possibly undefined) If operator, this is the index of the operator in the degree-specific operator enum
        children::NTuple{D,Nullable{$N{T,D}}}

        #################
        ## Constructors:
        #################
        $N{_T,_D}() where {_T,_D} = new{_T,_D::Int}()
        $N{_T}() where {_T} = $N{_T,DEFAULT_MAX_DEGREE}()
        # TODO: Test with this disabled to spot any unintended uses
    end
end

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
- `children::NTuple{D,Node{T,D}}`: Children of the node. Only defined up to `degree`

For accessing and modifying children, use [`get_child`](@ref), [`set_child!`](@ref),
[`get_children`](@ref), and [`set_children!`](@ref).

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
julia> operators = OperatorEnum(1 => (cos, sin), 2 => (+, -, *));

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

"""
    poison_node(n::AbstractNode)

Return a placeholder for unused child slots, ensuring type stability.
Accessing this node should trigger some kind of noticable error
(e.g., default returns itself, which causes infinite recursion).
"""
function poison_node(n::AbstractNode)
    return Nullable(true, n)
end

"""
    get_children(node::AbstractNode)

Return the raw `.children` tuple of a node. Some of these
children may be "poisoned" nodes which you should not access,
as they will trigger infinite recursion errors. Ensure to
only access children only up to the `.degree` of the node.
"""
@inline function unsafe_get_children(node::AbstractNode)
    return getfield(node, :children)
end

"""
    get_children(node::AbstractNode, n::Integer)
    get_children(node::AbstractNode, ::Val{n})

Return a tuple of exactly `n` children of the node. You should
use the `.degree` field of a node to determine the number of children
to return. Typically this is done within a `Base.Cartesian.@nif` statement
for total type stability.
"""
@unstable @inline function get_children(node::AbstractNode{D}, n::Integer) where {D}
    return get_children(node, Val(n))
end
@inline function get_children(node::AbstractNode{D}, ::Val{n}) where {D,n}
    cs = unsafe_get_children(node)
    return ntuple(i -> cs[i][], Val(Int(n)))
end

"""
    get_child(node::AbstractNode, i::Integer)

Return the `i`-th child of a node (1-indexed).
"""
@inline function get_child(n::AbstractNode{D}, i::Int) where {D}
    return unsafe_get_children(n)[i][]
end

"""
    set_child!(node::AbstractNode, child::AbstractNode, i::Integer)

Replace the `i`-th child of a node (1-indexed) with the given child node.
Returns the new child. Updates the children tuple in-place.
"""
@inline function set_child!(n::AbstractNode{D}, child::AbstractNode{D}, i::Int) where {D}
    set_children!(n, Base.setindex(unsafe_get_children(n), Nullable(false, child), i))
    return child
end

"""
    set_children!(node::AbstractNode, children::Tuple)

Replace all children of a node with the given tuple. If fewer children are
provided than the node's maximum degree, remaining slots are filled with poison nodes.
"""
@inline function set_children!(
    n::AbstractNode{D}, children::Union{Tuple,AbstractVector{<:AbstractNode{D}}}
) where {D}
    D2 = length(children)
    if D === D2
        n.children = ntuple(i -> _ensure_nullable(@inbounds(children[i])), Val(D))
    else
        poison = poison_node(n)
        # We insert poison at the end of the tuple so that
        # errors will appear loudly if accessed.
        # This poison should be efficient to insert. So
        # for simplicity, we can just use poison := Nullable(true, n)
        # which will raise an UndefRefError if accessed.
        n.children = ntuple(
            i -> i <= D2 ? _ensure_nullable(@inbounds(children[i])) : poison, Val(D)
        )
    end
end
@inline _ensure_nullable(x) = Nullable(false, x)
@inline _ensure_nullable(x::Nullable) = x

macro make_accessors(node_type)
    esc(
        quote
            @inline function Base.getproperty(n::$node_type, k::Symbol)
                if k == :l
                    # TODO: Should a depwarn be raised here? Or too slow?
                    return $(get_child)(n, 1)
                elseif k == :r
                    return $(get_child)(n, 2)
                else
                    return getfield(n, k)
                end
            end
            @inline function Base.setproperty!(n::$node_type, k::Symbol, v)
                if k == :l
                    if isdefined(n, :children)
                        $(set_child!)(n, v, 1)
                    else
                        $(set_children!)(n, (v,))
                        v
                    end
                elseif k == :r
                    $(set_child!)(n, v, 2)
                else
                    T = fieldtype(typeof(n), k)
                    if v isa T
                        setfield!(n, k, v)
                    else
                        setfield!(n, k, convert(T, v))
                    end
                end
            end
        end,
    )
end

@make_accessors Node
@make_accessors GraphNode
# TODO: Disable the `.l` accessors eventually, once the codebase is fully generic

################################################################################
#! format: on

Base.eltype(::Type{<:AbstractExpressionNode{T}}) where {T} = T
Base.eltype(::AbstractExpressionNode{T}) where {T} = T

#! format: off
# COV_EXCL_START
# These are marked unstable due to issues discussed on
# https://github.com/JuliaLang/julia/issues/55147
@unstable max_degree(::Type{<:AbstractNode}) = DEFAULT_MAX_DEGREE
@unstable max_degree(::Type{<:AbstractNode{D}}) where {D} = D
@unstable max_degree(node::AbstractNode) = max_degree(typeof(node))

has_max_degree(::Type{<:AbstractNode}) = false
has_max_degree(::Type{<:AbstractNode{D}}) where {D} = true
has_eltype(::Type{<:AbstractExpressionNode}) = false
has_eltype(::Type{<:AbstractExpressionNode{T}}) where {T} = true
# COV_EXCL_STOP
#! format: on

@unstable function node_wrapper(::Type{N}) where {N<:AbstractExpressionNode}
    return Base.typename(N).wrapper
end
@unstable function constructorof(::Type{N}) where {N<:AbstractExpressionNode}
    return node_wrapper(N){T,max_degree(N)} where {T}
end
function with_type_parameters(::Type{N}, ::Type{T}) where {N<:AbstractExpressionNode,T}
    return node_wrapper(N){T,max_degree(N)}
end
@unstable function with_max_degree(::Type{N}, ::Val{D}) where {N<:AbstractExpressionNode,D}
    if has_eltype(N)
        return node_wrapper(N){eltype(N),D}
    else
        return node_wrapper(N){T,D} where {T}
    end
end
@unstable function with_default_max_degree(::Type{N}) where {N<:AbstractNode}
    return with_max_degree(N, Val(max_degree(N)))
end

function default_allocator(::Type{N}, ::Type{T}) where {N<:AbstractExpressionNode,T}
    return with_type_parameters(N, T)()
end

"""Trait declaring whether nodes share children or not."""
preserve_sharing(::Union{Type{<:AbstractNode},AbstractNode}) = false  # COV_EXCL_LINE
preserve_sharing(::Union{Type{<:GraphNode},GraphNode}) = true  # COV_EXCL_LINE

include("base.jl")

#! format: off
@inline function (::Type{N})(
    ::Type{T1}=Undefined; val=nothing, feature=nothing, op=nothing, l=nothing, r=nothing, children=nothing, allocator::F=default_allocator,
) where {T1,N<:AbstractExpressionNode{T} where T,F}
    _children = if !isnothing(l) && isnothing(r)
        @assert isnothing(children)
        (l,)
    elseif !isnothing(l) && !isnothing(r)
        @assert isnothing(children)
        (l, r)
    else
        children
    end
    if all_defaults(N, val, feature, op, _children)
        return make_default(N, T1)
    end
    return node_factory(N, T1, val, feature, op, _children, allocator)
end
function make_default(::Type{N}, ::Type{T1}) where {T1,N<:AbstractExpressionNode}
    if has_max_degree(N)
        error(
            "Encountered the call for $N() inside the generic constructor. "
            * "Did you forget to define `$(Base.typename(N).wrapper){T,D}() where {T,D} = new{T,D}()`?"
        )
    end
    if T1 === Undefined
        return with_default_max_degree(N)()
    else
        return with_type_parameters(with_default_max_degree(N), T1)()
    end
end
function all_defaults(::Type{N}, val, feature, op, children) where {N<:AbstractExpressionNode}
    return all(isnothing, (val, feature, op, children))
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
    ::Type{N}, ::Type, ::Nothing, ::Nothing, op::Integer, children::Union{Tuple,AbstractVector}, allocator::F,
) where {N<:AbstractExpressionNode,F}
    T = defines_eltype(N) ? eltype(N) : promote_type(map(eltype, children)...)
    defines_eltype(N) && @assert T === promote_type(T, map(eltype, children)...)
    D2 = length(children)
    @assert D2 <= max_degree(N)
    NT = with_type_parameters(N, T)
    n = allocator(N, T)
    n.degree = D2
    n.op = op
    set_children!(n, children)
    return n
end

@inline function node_factory_type(::Type{N}, ::Type{T1}, ::Type{T2}) where {N,T1,T2}
    if T1 === Undefined && !defines_eltype(N)
        T2
    elseif T1 === Undefined
        eltype(N)
    elseif !defines_eltype(N)
        T1
    else
        eltype(N)
    end
end
defines_eltype(::Type{<:AbstractExpressionNode}) = false  # COV_EXCL_LINE
defines_eltype(::Type{<:AbstractExpressionNode{T}}) where {T} = true  # COV_EXCL_LINE
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

"""
    set_node!(tree::AbstractExpressionNode{T}, new_tree::AbstractExpressionNode{T}) where {T}

Set every field of `tree` equal to the corresponding field of `new_tree`.
"""
function set_node!(tree::AbstractExpressionNode, new_tree::AbstractExpressionNode)
    tree.degree = new_tree.degree
    if new_tree.degree == 0
        set_leaf!(tree, new_tree)
    else
        set_branch!(tree, new_tree)
    end
    return nothing
end
function set_leaf!(tree::AbstractExpressionNode, new_leaf::AbstractExpressionNode)
    tree.constant = new_leaf.constant
    if new_leaf.constant
        tree.val = new_leaf.val::eltype(new_leaf)
    else
        tree.feature = new_leaf.feature
    end
    return nothing
end
function set_branch!(tree::AbstractExpressionNode, new_branch::AbstractExpressionNode)
    tree.op = new_branch.op
    set_children!(tree, unsafe_get_children(new_branch))
    return nothing
end

end
