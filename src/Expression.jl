"""This module defines a user-facing `Expression` type"""
module ExpressionModule

using ArgCheck: @argcheck

using ..NodeModule: AbstractExpressionNode
using ..OperatorEnumModule: AbstractOperatorEnum
using ..UtilsModule: Undefined

"""
    AbstractExpression{T,N<:AbstractExpressionNode{T}}

Abstract type for user-facing expression types, which contain
both the raw expression tree, as well as associated metadata
to evaluate and render the expression. You should avoid using
these types for internal operations, as they result in extra allocations.

These types must have the field

- `tree::N`: the raw expression tree

It must declare the method `options(ex::AbstractExpression)` which
should return the relevant metadata to be passed to internal functions
such as `eval_tree_array` or `string_tree`
"""
abstract type AbstractExpression{T,N<:AbstractExpressionNode{T}} end

function options(::AbstractExpression)
    throw(
        ArgumentError(
            "`options` function must be implemented for specific AbstractExpression types."
        ),
    )
end

"""
    Expression{T, N, O, V} <: AbstractExpression{T, N}

Defines a high level, user-facing, expression type that encapsulates an
expression tree (like `Node`) along with associated metadata for evaluation and rendering.

# Fields

- `tree::N`: The root node of the raw expression tree.
- `operators::O`: Operators indexed by the expression tree.
- `variable_names::V`: A vector of variable names used when printing the expression.

# Constructors

- Expression(tree::AbstractExpressionNode, operators::AbstractOperatorEnum, variable_names::AbstractVector): Construct from the fields
- @parse_expression(expr, operators=operators, variable_names=variable_names, node_type=Node): Parse a Julia expression with a given context and create an Expression object.

# Usage

This type is intended for end-users to interact with and manipulate expressions at a high level,
abstracting away the complexities of the underlying expression tree operations.
"""
struct Expression{
    T,
    N<:AbstractExpressionNode{T},
    O<:AbstractOperatorEnum,
    V<:AbstractVector{<:AbstractString},
} <: AbstractExpression{T,N}
    tree::N
    operators::O
    variable_names::V

    function Expression(
        tree::AbstractExpressionNode{_T},
        operators::AbstractOperatorEnum,
        variable_names::AbstractVector{_S},
    ) where {_T,_S}
        variable_names = isa(_S, AbstractString) ? variable_names : string.(variable_names)
        return new{_T,typeof(tree),typeof(operators),typeof(variable_names)}(
            tree, operators, variable_names
        )
    end
end

options(ex::Expression) = ex.operators

import ..NodeModule: constructorof
constructorof(::Type{<:Expression}) = Expression

"""Create a new expression with a given tree but all other fields forwarded."""
@generated function with_tree(tree::N, ex::AbstractExpression{T,N}) where {T,N}
    fields = fieldnames(ex)
    out = quote
        constructorof(typeof(ex))()
    end
    expr_args = last(out.args).args
    for field in fields
        if field == :tree
            push!(expr_args, :(tree))
        else
            push!(expr_args, :(ex.$field))
        end
    end
    return out
end

# Overload all methods on AbstractExpressionNode that return an aggregation, or can
# return an entire tree. Methods that only return the nodes are *not* overloaded, so
# that the user must use the low-level interface.

import ..NodeModule: preserve_sharing, copy_node, set_node!, count_nodes, tree_mapreduce

#! format: off
preserve_sharing(::Type{<:AbstractExpression{T,N}}) where {T,N} = preserve_sharing(N)
copy_node(ex::AbstractExpression; kws...) = with_tree(copy_node(ex.tree; kws...), ex)
set_node!(ex::AbstractExpression, node::AbstractExpressionNode) = (set_node!(ex.tree, node); ex)
count_nodes(ex::AbstractExpression; kws...) = count_nodes(ex.tree; kws...)

function tree_mapreduce(
    f::Function,
    op::Function,
    ex::AbstractExpression,
    result_type::Type=Undefined;
    kws...,
)
    return tree_mapreduce(f, op, ex.tree, result_type; kws...)
end
function tree_mapreduce(
    f_leaf::Function,
    f_branch::Function,
    op::Function,
    ex::AbstractExpression,
    result_type::Type=Undefined;
    kws...,
)
    return tree_mapreduce(f_leaf, f_branch, op, ex.tree, result_type; kws...)
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
count_constants(ex::AbstractExpression; kws...) = count_constants(ex.tree; kws...)
count_depth(ex::AbstractExpression; kws...) = count_depth(ex.tree; kws...)
index_constants(ex::AbstractExpression, ::Type{T}=UInt16) where {T} = index_constants(ex.tree, T)
has_operators(ex::AbstractExpression) = has_operators(ex.tree)
has_constants(ex::AbstractExpression) = has_constants(ex.tree)
get_constants(ex::AbstractExpression) = get_constants(ex.tree)
set_constants!(ex::AbstractExpression, constants) = set_constants!(ex.tree, constants)
#! format: on

import ..StringsModule: string_tree, print_tree

function string_tree(ex::AbstractExpression; kws...)
    return string_tree(ex.tree, options(ex); ex.variable_names, kws...)
end
for io in ((), (:(io::IO),))
    @eval function print_tree($(io...), ex::AbstractExpression; kws...)
        return print_tree($(io...), ex.tree, options(ex); ex.variable_names, kws...)
    end
end

function Base.show(io::IO, ::MIME"text/plain", ex::AbstractExpression)
    return print(io, string_tree(ex.tree, options(ex); ex.variable_names))
end

import ..EvaluateModule: eval_tree_array, differentiable_eval_tree_array

function max_feature(ex::AbstractExpression)
    return tree_mapreduce(
        leaf -> leaf.constant ? zero(UInt16) : leaf.feature,
        branch -> zero(UInt16),
        max,
        ex.tree,
        UInt16,
    )
end

function _validate_input(ex::AbstractExpression, X::AbstractMatrix)
    @argcheck max_feature(ex) <= size(X, 1)
end

function eval_tree_array(ex::AbstractExpression, cX::AbstractMatrix; kws...)
    _validate_input(ex, cX)
    return eval_tree_array(ex.tree, cX, options(ex); kws...)
end
function differentiable_eval_tree_array(ex::AbstractExpression, cX::AbstractMatrix; kws...)
    _validate_input(ex, cX)
    return differentiable_eval_tree_array(ex.tree, cX, options(ex); kws...)
end

import ..EvaluateDerivativeModule: eval_diff_tree_array, eval_grad_tree_array

function eval_diff_tree_array(ex::AbstractExpression, cX::AbstractMatrix; kws...)
    _validate_input(ex, cX)
    return eval_diff_tree_array(ex.tree, cX, options(ex); kws...)
end
function eval_grad_tree_array(ex::AbstractExpression, cX::AbstractMatrix; kws...)
    _validate_input(ex, cX)
    return eval_grad_tree_array(ex.tree, cX, options(ex); kws...)
end

import ..EvaluationHelpersModule: _grad_evaluator

function _grad_evaluator(ex::AbstractExpression, cX::AbstractMatrix; kws...)
    _validate_input(ex, cX)
    return _grad_evaluator(ex.tree, cX, options(ex); kws...)
end
function (ex::AbstractExpression)(X; kws...)
    _validate_input(ex, X)
    return ex.tree(X, options(ex); kws...)
end

import ..SimplifyModule: combine_operators, simplify_tree!

function combine_operators(ex::AbstractExpression; kws...)
    return combine_operators(ex.tree, options(ex); kws...)
end
function simplify_tree!(ex::AbstractExpression; kws...)
    return simplify_tree!(ex.tree, options(ex); kws...)
end

import Base: copy, hash

function copy(ex::Expression)
    return Expression(copy(ex.tree), copy(ex.operators), copy(ex.variable_names))
end
function hash(ex::Expression, h::UInt)
    return hash((ex.tree, ex.operators, ex.variable_names), h)
end

end
