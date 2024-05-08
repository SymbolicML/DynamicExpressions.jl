"""This module defines a user-facing `Expression` type"""
module ExpressionModule

using ..NodeModule: AbstractExpressionNode
using ..OperatorEnumModule: AbstractOperatorEnum, OperatorEnum
using ..UtilsModule: Undefined

"""A wrapper for a named tuple to avoid piracy."""
struct Metadata{NT<:NamedTuple}
    _data::NT
end
_data(x::Metadata) = getfield(x, :_data)

Base.propertynames(x::Metadata) = propertynames(_data(x))
@inline Base.getproperty(x::Metadata, f::Symbol) = getproperty(_data(x), f)
Base.show(io::IO, x::Metadata) = print(io, "Metadata(", _data(x), ")")
@inline function Base.copy(metadata::Metadata)
    nt = _data(metadata)
    copied_nt = NamedTuple{keys(nt)}(map(copy, values(nt)))
    return Metadata(copied_nt)
end
@inline Base.:(==)(x::Metadata, y::Metadata) = _data(x) == _data(y)
@inline Base.hash(x::Metadata, h::UInt) = hash(_data(x), h)

"""
    AbstractExpression{T}

Abstract type for user-facing expression types, which contain
both the raw expression tree operating on a value type of `T`,
as well as associated metadata to evaluate and render the expression.

# Interface

## Required methods

- `get_tree`
- `get_operators`
- `get_variable_names`
- `Base.copy`
- `Base.hash`
- `Base.:(==)`

## Optional methods

Many of these optional methods will use
the three required methods, but for custom behavior,
you can overload them.

- `count_nodes`
- `count_constants`
- `count_depth`
- `index_constants`
- `has_operators`
- `has_constants`
- `get_constants`
- `set_constants!`
- `string_tree`
- `eval_tree_array`
- `eval_grad_tree_array`
- `Optim.optimize`
- `_grad_evaluator`
- `(ex::AbstractExpression)(X, operators=nothing; kws...)`

If you wish to use `@parse_expression`, you can also
customize the parsing behavior with

- `parse_leaf`
"""
abstract type AbstractExpression{T} end

"""
    Expression{T, N, D} <: AbstractExpression{T}

Defines a high level, user-facing, expression type that encapsulates an
expression tree (like `Node`) along with associated metadata for evaluation and rendering.

# Fields

- `tree::N`: The root node of the raw expression tree.
- `metadata::Metadata{D}`: A named tuple of settings for the expression,
   such as the operators and variable names.

# Constructors

- Expression(tree::AbstractExpressionNode, metadata::NamedTuple): Construct from the fields
- @parse_expression(expr, operators=operators, variable_names=variable_names, node_type=Node): Parse a Julia expression with a given context and create an Expression object.

# Usage

This type is intended for end-users to interact with and manipulate expressions at a high level,
abstracting away the complexities of the underlying expression tree operations.
"""
struct Expression{T,N<:AbstractExpressionNode{T},D<:NamedTuple} <: AbstractExpression{T}
    tree::N
    metadata::Metadata{D}
end

@inline function Expression(tree::AbstractExpressionNode{T}; metadata...) where {T}
    d = (; metadata...)
    return Expression(tree, Metadata(d))
end

########################################################
# Abstract interface ###################################
########################################################
"""
    get_operators(ex::AbstractExpression, operators::Union{Nothing,Any})

which will return the operators to be passed to internal functions
such as `eval_tree_array` or `string_tree`, either from the expression itself,
or `cur_operators` if it is not `nothing`. If left as default,
it requires `cur_operators` to not be `nothing`.
`cur_operators` would typically be an `OperatorEnum`.
"""
function get_operators(ex::AbstractExpression, operators)
    return error("`get_operators` function must be implemented for $(typeof(ex)) types.")
end

"""
    get_variable_names(ex::AbstractExpression, variable_names::Union{Nothing,AbstractVector{<:AbstractString}})

The same as `operators`, but for variable names.
"""
function get_variable_names(ex::AbstractExpression, variable_names)
    return error(
        "`get_variable_names` function must be implemented for $(typeof(ex)) types."
    )
end

"""
    get_tree(ex::AbstractExpression)

A method that extracts the expression tree from `AbstractExpression`
and should return an `AbstractExpressionNode`.
"""
function get_tree(ex::AbstractExpression)
    return error("`get_tree` function must be implemented for $(typeof(ex)) types.")
end

function Base.copy(ex::AbstractExpression; break_sharing::Val=Val(false))
    return error("`copy` function must be implemented for $(typeof(ex)) types.")
end
function Base.hash(ex::AbstractExpression, h::UInt)
    return error("`hash` function must be implemented for $(typeof(ex)) types.")
end
function Base.:(==)(x::AbstractExpression, y::AbstractExpression)
    return error("`==` function must be implemented for $(typeof(x)) types.")
end
########################################################

function get_operators(ex::Expression, operators)
    return operators === nothing ? ex.metadata.operators : operators
end
function get_variable_names(ex::Expression, variable_names)
    return variable_names === nothing ? ex.metadata.variable_names : variable_names
end
function get_tree(ex::Expression)
    return ex.tree
end
function Base.copy(ex::Expression; break_sharing::Val=Val(false))
    return Expression(copy(ex.tree; break_sharing), copy(ex.metadata))
end
function Base.hash(ex::Expression, h::UInt)
    return hash(ex.tree, hash(ex.metadata, h))
end
function Base.:(==)(x::Expression, y::Expression)
    return x.tree == y.tree && x.metadata == y.metadata
end

# Overload all methods on AbstractExpressionNode that return an aggregation, or can
# return an entire tree. Methods that only return the nodes are *not* overloaded, so
# that the user must use the low-level interface.

import ..NodeModule: copy_node, set_node!, count_nodes, tree_mapreduce, constructorof

#! format: off
constructorof(::Type{E}) where {E<:AbstractExpression} = Base.typename(E).wrapper
constructorof(::Type{<:Expression}) = Expression
copy_node(ex::AbstractExpression; kws...) = copy(ex)
count_nodes(ex::AbstractExpression; kws...) = count_nodes(get_tree(ex); kws...)

function tree_mapreduce(
    f::Function,
    op::Function,
    ex::AbstractExpression,
    result_type::Type=Undefined;
    kws...,
)
    return tree_mapreduce(f, op, get_tree(ex), result_type; kws...)
end
function tree_mapreduce(
    f_leaf::Function,
    f_branch::Function,
    op::Function,
    ex::AbstractExpression,
    result_type::Type=Undefined;
    kws...,
)
    return tree_mapreduce(f_leaf, f_branch, op, get_tree(ex), result_type; kws...)
end

#! format: on

import ..NodeUtilsModule:
    count_constants,
    count_depth,
    index_constants,
    has_operators,
    has_constants,
    get_constants,
    set_constants!

#! format: off
count_constants(ex::AbstractExpression; kws...) = count_constants(get_tree(ex); kws...)
count_depth(ex::AbstractExpression; kws...) = count_depth(get_tree(ex); kws...)
index_constants(ex::AbstractExpression, ::Type{T}=UInt16) where {T} = index_constants(get_tree(ex), T)
has_operators(ex::AbstractExpression) = has_operators(get_tree(ex))
has_constants(ex::AbstractExpression) = has_constants(get_tree(ex))
get_constants(ex::AbstractExpression) = get_constants(get_tree(ex))
set_constants!(ex::AbstractExpression{T}, constants::AbstractVector{T}) where {T} = set_constants!(get_tree(ex), constants)
#! format: on

import ..StringsModule: string_tree, print_tree

function string_tree(
    ex::AbstractExpression, operators=nothing; variable_names=nothing, kws...
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
        $(io...), ex::AbstractExpression, operators=nothing; variable_names=nothing, kws...
    )
        return println($(io...), string_tree(ex, operators; variable_names, kws...))
    end
end

function Base.show(io::IO, ::MIME"text/plain", ex::AbstractExpression)
    return print(io, string_tree(ex))
end

import ..EvaluateModule: eval_tree_array, differentiable_eval_tree_array

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

function _validate_input(ex::AbstractExpression, X, operators)
    if get_operators(ex, operators) isa OperatorEnum
        @assert X isa AbstractMatrix
        @assert max_feature(ex) <= size(X, 1)
    end
    return nothing
end

function eval_tree_array(
    ex::AbstractExpression, cX::AbstractMatrix, operators=nothing; kws...
)
    _validate_input(ex, cX, operators)
    return eval_tree_array(get_tree(ex), cX, get_operators(ex, operators); kws...)
end

import ..EvaluateDerivativeModule: eval_grad_tree_array

# skipped (not used much)
#  - eval_diff_tree_array
#  - differentiable_eval_tree_array

function eval_grad_tree_array(
    ex::AbstractExpression, cX::AbstractMatrix, operators=nothing; kws...
)
    _validate_input(ex, cX, operators)
    return eval_grad_tree_array(get_tree(ex), cX, get_operators(ex, operators); kws...)
end

import ..EvaluationHelpersModule: _grad_evaluator

function Base.adjoint(ex::AbstractExpression)
    return ((args...; kws...) -> _grad_evaluator(ex, args...; kws...))
end
function _grad_evaluator(
    ex::AbstractExpression,
    cX::AbstractMatrix,
    operators=nothing;
    variable=Val(true),
    kws...,
)
    _validate_input(ex, cX, operators)
    return _grad_evaluator(get_tree(ex), cX, get_operators(ex, operators); variable, kws...)
end
function (ex::AbstractExpression)(X, operators=nothing; kws...)
    _validate_input(ex, X, operators)
    return get_tree(ex)(X, get_operators(ex, operators); kws...)
end

import ..SimplifyModule: combine_operators, simplify_tree!

# Avoid implementing a generic version for these, as it is less likely to generalize
function combine_operators(ex::Expression, operators=nothing; kws...)
    return Expression(
        combine_operators(get_tree(ex), get_operators(ex, operators); kws...), ex.metadata
    )
end
function simplify_tree!(ex::Expression, operators=nothing; kws...)
    return Expression(
        simplify_tree!(get_tree(ex), get_operators(ex, operators); kws...), ex.metadata
    )
end

end
