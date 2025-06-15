"""This module defines a user-facing `Expression` type"""
module ExpressionModule

using DispatchDoctor: @unstable

using ..NodeModule: AbstractExpressionNode, Node
using ..OperatorEnumModule: AbstractOperatorEnum, OperatorEnum
using ..UtilsModule: Undefined
using ..ChainRulesModule: NodeTangent

import ..NodeModule:
    copy_node, set_node!, count_nodes, tree_mapreduce, constructorof, max_degree
import ..NodeUtilsModule:
    preserve_sharing,
    count_constant_nodes,
    count_depth,
    index_constant_nodes,
    has_operators,
    has_constants,
    count_scalar_constants,
    get_scalar_constants,
    set_scalar_constants!
import ..NodePreallocationModule: copy_into!, allocate_container
import ..EvaluateModule: eval_tree_array, differentiable_eval_tree_array
import ..EvaluateDerivativeModule: eval_grad_tree_array
import ..EvaluationHelpersModule: _grad_evaluator
import ..StringsModule: string_tree, print_tree
import ..ChainRulesModule: extract_gradient
import ..SimplifyModule: combine_operators, simplify_tree!

"""A wrapper for a named tuple to avoid piracy."""
struct Metadata{NT<:NamedTuple}
    _data::NT
end
unpack_metadata(x::Metadata) = getfield(x, :_data)

Base.propertynames(x::Metadata) = propertynames(unpack_metadata(x))
@unstable @inline function Base.getproperty(x::Metadata, f::Symbol)
    return getproperty(unpack_metadata(x), f)
end
Base.show(io::IO, x::Metadata) = print(io, "Metadata(", unpack_metadata(x), ")")
@inline _copy(x) = copy(x)
@inline _copy(x::NamedTuple) = copy_named_tuple(x)
@inline _copy(::Nothing) = nothing
@inline function copy_named_tuple(nt::NamedTuple)
    return NamedTuple{keys(nt)}(map(_copy, values(nt)))
end
@inline function Base.copy(metadata::Metadata)
    return Metadata(_copy(unpack_metadata(metadata)))
end
@inline Base.:(==)(x::Metadata, y::Metadata) = unpack_metadata(x) == unpack_metadata(y)
@inline Base.hash(x::Metadata, h::UInt) = hash(unpack_metadata(x), h)

"""
    AbstractExpression{T,N}

(Experimental) Abstract type for user-facing expression types, which contain
both the raw expression tree operating on a value type of `T`,
as well as associated metadata to evaluate and render the expression.

See [`ExpressionInterface`](@ref DynamicExpressions.InterfacesModule.ExpressionInterface) for a full description
of the interface implementation, as well as tests to verify correctness.

If you wish to use `@parse_expression`, you can also
customize the parsing behavior with

- `parse_leaf`
"""
abstract type AbstractExpression{T,N} end

"""
    Expression{T, N, D} <: AbstractExpression{T, N}

(Experimental) Defines a high-level, user-facing, expression type that encapsulates an
expression tree (like `Node`) along with associated metadata for evaluation and rendering.

# Fields

- `tree::N`: The root node of the raw expression tree.
- `metadata::Metadata{D}`: A named tuple of settings for the expression,
    such as the operators and variable names.

# Constructors

- `Expression(tree::AbstractExpressionNode, metadata::NamedTuple)`: Construct from the fields
- `@parse_expression(expr, operators=operators, variable_names=variable_names, node_type=Node)`: Parse a Julia expression with a given context and create an Expression object.

# Usage

This type is intended for end-users to interact with and manipulate expressions at a high level,
abstracting away the complexities of the underlying expression tree operations.
"""
struct Expression{T,N<:AbstractExpressionNode{T},D<:NamedTuple} <: AbstractExpression{T,N}
    tree::N
    metadata::Metadata{D}
end

@inline function Expression(tree::AbstractExpressionNode{T}; metadata...) where {T}
    d = (; metadata...)
    return Expression(tree, Metadata(d))
end

has_node_type(::Union{E,Type{E}}) where {N,E<:AbstractExpression{<:Any,N}} = true  # COV_EXCL_LINE
has_node_type(::Union{E,Type{E}}) where {E<:AbstractExpression} = false  # COV_EXCL_LINE
node_type(::Union{E,Type{E}}) where {N,E<:AbstractExpression{<:Any,N}} = N
function max_degree(::Union{E,Type{E}}) where {E<:AbstractExpression}
    return has_node_type(E) ? max_degree(node_type(E)) : max_degree(Node)
end
@unstable default_node_type(_) = Node
default_node_type(::Type{N}) where {T,N<:AbstractExpression{T}} = Node{T,max_degree(N)}

########################################################
# Abstract interface ###################################
########################################################
function get_operators(
    ex::AbstractExpression, operators::Union{AbstractOperatorEnum,Nothing}=nothing
)
    return error("`get_operators` function must be implemented for $(typeof(ex)) types.")
end
function get_variable_names(
    ex::AbstractExpression,
    variable_names::Union{Nothing,AbstractVector{<:AbstractString}}=nothing,
)
    return error(
        "`get_variable_names` function must be implemented for $(typeof(ex)) types."
    )
end

function get_tree(ex::AbstractExpression)
    return error("`get_tree` function must be implemented for $(typeof(ex)) types.")
end

function Base.copy(ex::AbstractExpression; break_sharing::Val=Val(false))
    return error("`copy` function must be implemented for $(typeof(ex)) types.")
end
function get_scalar_constants(ex::AbstractExpression)
    return error(
        "`get_scalar_constants` function must be implemented for $(typeof(ex)) types."
    )
end
function set_scalar_constants!(ex::AbstractExpression{T}, constants, refs) where {T}
    return error(
        "`set_scalar_constants!` function must be implemented for $(typeof(ex)) types."
    )
end
function extract_gradient(gradient, ex::AbstractExpression)
    # Should match `get_scalar_constants`
    return error(
        "`extract_gradient` function must be implemented for $(typeof(ex)) types with $(typeof(gradient)) gradient.",
    )
end
function get_contents(ex::AbstractExpression)
    return error("`get_contents` function must be implemented for $(typeof(ex)) types.")
end
function get_metadata(ex::AbstractExpression)
    return error("`get_metadata` function must be implemented for $(typeof(ex)) types.")
end
########################################################

"""
    get_operators(ex::AbstractExpression, operators::Union{Nothing,Any})

which will return the operators to be passed to internal functions
such as `eval_tree_array` or `string_tree`, either from the expression itself,
or `cur_operators` if it is not `nothing`. If left as default,
it requires `cur_operators` to not be `nothing`.
`cur_operators` would typically be an `OperatorEnum`.
"""
get_operators

"""
    get_variable_names(ex::AbstractExpression, variable_names::Union{Nothing,AbstractVector{<:AbstractString}})

The same as `operators`, but for variable names.
"""
get_variable_names

"""
    get_tree(ex::AbstractExpression)

A method that extracts the expression tree from `AbstractExpression`
and should return an `AbstractExpressionNode`.
"""
get_tree

"""
    get_contents(ex::AbstractExpression)

Get the contents of the expression, which might be a plain
`AbstractExpressionNode`, or some combination of them, or other data.
This should include everything other than that returned by [`get_metadata`](@ref).
"""
get_contents

"""
    get_metadata(ex::AbstractExpression)

Get the metadata of the expression, which might be a plain
`NamedTuple`, or some combination of them, or other data.
This should include everything other than that returned by [`get_contents`](@ref).
"""
get_metadata

"""
    with_contents(ex::AbstractExpression, tree::AbstractExpressionNode)
    with_contents(ex::AbstractExpression, tree::AbstractExpression)

Create a new expression based on `ex` but with a different `tree`
"""
function with_contents(ex::AbstractExpression, tree::AbstractExpression)
    return with_contents(ex, get_contents(tree))
end
function with_contents(ex::AbstractExpression, tree)
    return constructorof(typeof(ex))(tree, get_metadata(ex))
end
function get_contents(ex::Expression)
    return ex.tree
end

"""
    with_metadata(ex::AbstractExpression, metadata)
    with_metadata(ex::AbstractExpression; metadata...)

Create a new expression based on `ex` but with a different `metadata`.
"""
function with_metadata(ex::AbstractExpression; metadata...)
    return with_metadata(
        ex, Metadata((; unpack_metadata(get_metadata(ex))..., metadata...))
    )
end
function with_metadata(ex::AbstractExpression, metadata::Metadata)
    return constructorof(typeof(ex))(get_contents(ex), metadata)
end
function get_metadata(ex::Expression)
    return ex.metadata
end

function preserve_sharing(::Union{E,Type{E}}) where {T,N,E<:AbstractExpression{T,N}}
    return preserve_sharing(N)
end

function get_operators(
    tree::AbstractExpressionNode, operators::Union{AbstractOperatorEnum,Nothing}=nothing
)
    if operators === nothing
        throw(ArgumentError("`operators` must be provided for $(typeof(tree)) types."))
    else
        return operators
    end
end
function get_operators(
    ex::Expression, operators::Union{AbstractOperatorEnum,Nothing}=nothing
)
    return operators === nothing ? ex.metadata.operators : operators
end
function get_variable_names(
    ex::Expression, variable_names::Union{Nothing,AbstractVector{<:AbstractString}}=nothing
)
    return if variable_names !== nothing
        variable_names
    elseif hasproperty(ex.metadata, :variable_names)
        ex.metadata.variable_names
    else
        nothing
    end
end
function get_tree(ex::Expression)
    return ex.tree
end
function get_tree(tree::AbstractExpressionNode)
    return tree
end
function Base.copy(ex::Expression; break_sharing::Val=Val(false))
    return Expression(copy(ex.tree; break_sharing), copy(ex.metadata))
end
function Base.hash(ex::AbstractExpression, h::UInt)
    return hash(get_contents(ex), hash(get_metadata(ex), h))
end
function Base.:(==)(x::AbstractExpression, y::AbstractExpression)
    return get_contents(x) == get_contents(y) && get_metadata(x) == get_metadata(y)
end

# Overload all methods on AbstractExpressionNode that return an aggregation, or can
# return an entire tree. Methods that only return the nodes are *not* overloaded, so
# that the user must use the low-level interface.

#! format: off
@unstable constructorof(::Type{E}) where {E<:AbstractExpression} = Base.typename(E).wrapper
@unstable constructorof(::Type{<:Expression}) = Expression
copy_node(ex::AbstractExpression; kws...) = copy(ex)
count_nodes(ex::AbstractExpression; kws...) = count_nodes(get_tree(ex); kws...)

function tree_mapreduce(
    f::F,
    op::G,
    ex::AbstractExpression,
    result_type::Type{RT}=Undefined;
    kws...,
) where {F<:Function,G<:Function,RT}
    return tree_mapreduce(f, op, get_tree(ex), RT; kws...)
end
function tree_mapreduce(
    f_leaf::F,
    f_branch::G,
    op::H,
    ex::AbstractExpression,
    result_type::Type{RT}=Undefined;
    kws...,
) where {F<:Function,G<:Function,H<:Function,RT}
    return tree_mapreduce(f_leaf, f_branch, op, get_tree(ex), RT; kws...)
end

count_constant_nodes(ex::AbstractExpression) = count_constant_nodes(get_tree(ex))
count_depth(ex::AbstractExpression) = count_depth(get_tree(ex))
index_constant_nodes(ex::AbstractExpression, ::Type{T}=UInt16) where {T} = index_constant_nodes(get_tree(ex), T)
has_operators(ex::AbstractExpression) = has_operators(get_tree(ex))
has_constants(ex::AbstractExpression) = has_constants(get_tree(ex))
Base.isempty(ex::AbstractExpression) = isempty(get_tree(ex))
#! format: on

function count_scalar_constants(ex::AbstractExpression)
    return count_scalar_constants(get_tree(ex))
end
function get_scalar_constants(ex::Expression)
    return get_scalar_constants(get_tree(ex))
end
function set_scalar_constants!(ex::Expression{T}, constants, refs) where {T}
    return set_scalar_constants!(get_tree(ex), constants, refs)
end
function extract_gradient(
    gradient::@NamedTuple{tree::NT, metadata::Nothing}, ex::Expression{T,N}
) where {T,N<:AbstractExpressionNode{T},NT<:NodeTangent{T,N}}
    # TODO: This messy gradient type is produced by ChainRules. There is probably a better way to do this.
    return extract_gradient(gradient.tree, get_tree(ex))
end

"""
    string_tree(
        ex::AbstractExpression,
        operators::Union{AbstractOperatorEnum,Nothing}=nothing;
        variable_names=nothing,
        kws...
    )

Convert an expression to a string representation.

This method unpacks the operators and variable names from the expression and calls [`string_tree`](@ref StringsModule.string_tree) for `AbstractExpressionNode`.

# Arguments

- `ex`: The expression to convert to a string.
- `operators`: (Optional) Operators to use. If `nothing`, operators are obtained from the expression.
- `variable_names`: (Optional) Variable names to use in the string representation. If `nothing`, variable names are obtained from the expression.
- `kws...`: Additional keyword arguments.

# Returns

- A string representation of the expression.

"""
function string_tree(
    ex::AbstractExpression,
    operators::Union{AbstractOperatorEnum,Nothing}=nothing;
    variable_names=nothing,
    kws...,
)
    return string_tree(
        get_tree(ex),
        get_operators(ex, operators);
        variable_names=get_variable_names(ex, variable_names),
        kws...,
    )
end
for io in ((), (:(io::IO),))
    @eval function print_tree(
        $(io...),
        ex::AbstractExpression,
        operators::Union{AbstractOperatorEnum,Nothing}=nothing;
        variable_names=nothing,
        kws...,
    )
        return println($(io...), string_tree(ex, operators; variable_names, kws...))
    end
end

function Base.show(io::IO, ex::AbstractExpression)
    return print(io, string_tree(ex))
end

function max_feature(ex::AbstractExpression)
    return tree_mapreduce(
        leaf -> leaf.constant ? zero(UInt16) : leaf.feature,
        branch -> zero(UInt16),
        max,
        get_tree(ex),
        UInt16;
        break_sharing=Val(true),
    )
end

function _validate_input(
    ex::AbstractExpression, X, operators::Union{AbstractOperatorEnum,Nothing}
)
    if get_operators(ex, operators) isa OperatorEnum
        @assert X isa AbstractMatrix
        @assert max_feature(ex) <= size(X, 1)
    end
    return nothing
end

"""
    eval_tree_array(
        ex::AbstractExpression,
        cX::AbstractMatrix,
        operators::Union{AbstractOperatorEnum,Nothing}=nothing;
        kws...
    )

Evaluate an expression over a given input data matrix.

This method unpacks the operators from the expression and calls [`eval_tree_array`](@ref EvaluateModule.eval_tree_array) for `AbstractExpressionNode`.

# Arguments

- `ex`: The expression to evaluate.
- `cX`: The input data matrix.
- `operators`: (Optional) Operators to use. If `nothing`, operators are obtained from the expression.
- `kws...`: Additional keyword arguments.

# Returns

- A tuple `(output, complete)` indicating the result and success of the evaluation.

"""
function eval_tree_array(
    ex::AbstractExpression,
    cX::AbstractMatrix,
    operators::Union{AbstractOperatorEnum,Nothing}=nothing;
    kws...,
)
    _validate_input(ex, cX, operators)
    return eval_tree_array(get_tree(ex), cX, get_operators(ex, operators); kws...)
end

# skipped (not used much)
#  - eval_diff_tree_array
#  - differentiable_eval_tree_array

"""
    eval_grad_tree_array(
        ex::AbstractExpression,
        cX::AbstractMatrix,
        operators::Union{AbstractOperatorEnum,Nothing}=nothing;
        kws...
    )

Compute the forward-mode derivative of an expression.

This method unpacks the operators from the expression and calls [`eval_grad_tree_array`](@ref EvaluateDerivativeModule.eval_grad_tree_array) for `AbstractExpressionNode`.

# Arguments

- `ex`: The expression to evaluate.
- `cX`: The data matrix.
- `operators`: (Optional) Operators to use. If `nothing`, operators are obtained from the expression.
- `kws...`: Additional keyword arguments.

# Returns

- A tuple `(output, gradient, complete)` indicating the result, gradient, and success of the evaluation.

"""
function eval_grad_tree_array(
    ex::AbstractExpression,
    cX::AbstractMatrix,
    operators::Union{AbstractOperatorEnum,Nothing}=nothing;
    kws...,
)
    _validate_input(ex, cX, operators)
    return eval_grad_tree_array(get_tree(ex), cX, get_operators(ex, operators); kws...)
end

function Base.adjoint(ex::AbstractExpression)
    return ((args...; kws...) -> _grad_evaluator(ex, args...; kws...))
end
function _grad_evaluator(
    ex::AbstractExpression,
    cX::AbstractMatrix,
    operators::Union{AbstractOperatorEnum,Nothing}=nothing;
    variable=Val(true),
    kws...,
)
    _validate_input(ex, cX, operators)
    return _grad_evaluator(get_tree(ex), cX, get_operators(ex, operators); variable, kws...)
end

"""
    (ex::AbstractExpression)(X, operators::Union{AbstractOperatorEnum,Nothing}=nothing; kws...)

Evaluate the expression `ex` over the input data `X`.

This method unpacks the operators from the expression and calls the corresponding method for `AbstractExpressionNode`.

# Arguments

- `X`: The input data to evaluate the expression on.
- `operators`: (Optional) Operators to use. If `nothing`, operators are obtained from the expression.
- `kws...`: Additional keyword arguments.

# Returns

- The result of evaluating the expression over the input data `X`.

"""
function (ex::AbstractExpression)(
    X, operators::Union{AbstractOperatorEnum,Nothing}=nothing; kws...
)
    _validate_input(ex, X, operators)
    return get_tree(ex)(X, get_operators(ex, operators); kws...)
end

# We don't require users to overload this, as it's not part of the required interface.
# Also, there's no way to generally do this from the required interface, so for backwards
# compatibility, we just return nothing.
# COV_EXCL_START
function copy_into!(::Nothing, src::AbstractExpression)
    return copy(src)
end
function allocate_container(::AbstractExpression, ::Union{Nothing,Integer}=nothing)
    return nothing
end
# COV_EXCL_STOP
function allocate_container(prototype::Expression, n::Union{Nothing,Integer}=nothing)
    return (; tree=allocate_container(get_contents(prototype), n))
end
function copy_into!(dest::NamedTuple, src::Expression)
    tree = copy_into!(dest.tree, get_contents(src))
    return with_contents(src, tree)
end

end
