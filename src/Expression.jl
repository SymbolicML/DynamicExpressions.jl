"""This module defines a user-facing `Expression` type"""
module ExpressionModule

using ArgCheck: @argcheck

using ..NodeModule: AbstractExpressionNode
using ..OperatorEnumModule: AbstractOperatorEnum, OperatorEnum
using ..UtilsModule: Undefined

"""
    AbstractExpression{T,N<:AbstractExpressionNode{T}}

Abstract type for user-facing expression types, which contain
both the raw expression tree, as well as associated metadata
to evaluate and render the expression. You should avoid using
these types for internal operations, as they result in extra allocations.

# Fields

- `tree::N`: the raw expression tree

# Interface

An `AbstractExpression` must declare:

1.

```julia
get_settings(ex::AbstractExpression, cur_settings::Union{Nothing,Any})
```

which will return the settings to be passed to internal functions
such as `eval_tree_array` or `string_tree`, either from the expression itself,
or `cur_settings` if it is not `nothing`. If left as default,
it requires `cur_settings` to not be `nothing`.
`cur_settings` would typically be an `OperatorEnum`.

2.

```julia
variable_names(ex::AbstractExpression, cur_variable_names::Union{Nothing,AbstractVector{<:AbstractString}})
```

The same as `settings`, but for variable names.

3.

```
get_tree(ex::AbstractExpression)
```

A method that extracts the expression tree from `AbstractExpression`
and should return an `AbstractExpressionNode`.
"""
abstract type AbstractExpression{T,N<:AbstractExpressionNode{T}} end

"""
    Expression{T, N, O, V} <: AbstractExpression{T, N}

Defines a high level, user-facing, expression type that encapsulates an
expression tree (like `Node`) along with associated metadata for evaluation and rendering.

# Fields

- `tree::N`: The root node of the raw expression tree.
- `settings::S`: Settings for the expression, such as the operators.
- `variable_names::V`: A vector of variable names used when printing the expression.

# Constructors

- Expression(tree::AbstractExpressionNode, operators::AbstractOperatorEnum, variable_names::AbstractVector): Construct from the fields
- @parse_expression(expr, operators=operators, variable_names=variable_names, node_type=Node): Parse a Julia expression with a given context and create an Expression object.

# Usage

This type is intended for end-users to interact with and manipulate expressions at a high level,
abstracting away the complexities of the underlying expression tree operations.
"""
struct Expression{T,N<:AbstractExpressionNode{T},S,V} <: AbstractExpression{T,N}
    tree::N
    settings::S
    variable_names::V
end

########################################################
# Abstract interface ###################################
########################################################
for f in (:get_settings, :get_variable_names, :get_tree)
    args = f == :get_tree ? () : (:(_),)
    @eval function $f(ex::AbstractExpression, $(args...))
        throw(
            MethodError(
                $f,
                "`$f` function must be implemented for " * string(typeof(ex)) * " types.",
            ),
        )
    end
end
########################################################

function get_settings(ex::Expression, settings)
    return settings === nothing ? ex.settings : settings
end
function get_variable_names(ex::Expression, variable_names)
    return variable_names === nothing ? ex.variable_names : variable_names
end
function get_tree(ex::Expression)
    return ex.tree
end

import ..NodeModule: constructorof
constructorof(::Type{<:Expression}) = Expression

# Overload all methods on AbstractExpressionNode that return an aggregation, or can
# return an entire tree. Methods that only return the nodes are *not* overloaded, so
# that the user must use the low-level interface.

import ..NodeModule: preserve_sharing, copy_node, set_node!, count_nodes, tree_mapreduce

#! format: off
preserve_sharing(::Type{<:AbstractExpression{T,N}}) where {T,N} = preserve_sharing(N)
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
set_constants!(ex::AbstractExpression, constants) = set_constants!(get_tree(ex), constants)
#! format: on

import ..StringsModule: string_tree, print_tree

function string_tree(
    ex::AbstractExpression, settings=nothing; variable_names=nothing, kws...
)
    return string_tree(
        get_tree(ex),
        get_settings(ex, settings);
        variable_names=get_variable_names(ex, settings),
        kws...,
    )
end
for io in ((), (:(io::IO),))
    @eval function print_tree(
        $(io...), ex::AbstractExpression, settings=nothing; variable_names=nothing, kws...
    )
        return print_tree(
            $(io...),
            get_tree(ex),
            get_settings(ex, settings);
            variable_names=get_variable_names(ex, settings),
            kws...,
        )
    end
end

function Base.show(io::IO, ::MIME"text/plain", ex::AbstractExpression)
    return print(
        io,
        string_tree(
            get_tree(ex),
            get_settings(ex, nothing);
            variable_names=get_variable_names(ex, nothing),
        ),
    )
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

function _validate_input(ex::AbstractExpression, X, settings)
    if get_settings(ex, settings) isa OperatorEnum
        @argcheck X isa AbstractMatrix
        @argcheck max_feature(ex) <= size(X, 1)
    end
    return nothing
end

function eval_tree_array(
    ex::AbstractExpression, cX::AbstractMatrix, settings=nothing; kws...
)
    _validate_input(ex, cX, settings)
    return eval_tree_array(get_tree(ex), cX, get_settings(ex, settings); kws...)
end
function differentiable_eval_tree_array(
    ex::AbstractExpression, cX::AbstractMatrix, settings=nothing; kws...
)
    _validate_input(ex, cX, settings)
    return differentiable_eval_tree_array(
        get_tree(ex), cX, get_settings(ex, settings); kws...
    )
end

import ..EvaluateDerivativeModule: eval_diff_tree_array, eval_grad_tree_array

function eval_diff_tree_array(
    ex::AbstractExpression, cX::AbstractMatrix, settings=nothing; kws...
)
    _validate_input(ex, cX, settings)
    return eval_diff_tree_array(get_tree(ex), cX, get_settings(ex, settings); kws...)
end
function eval_grad_tree_array(
    ex::AbstractExpression, cX::AbstractMatrix, settings=nothing; kws...
)
    _validate_input(ex, cX, settings)
    return eval_grad_tree_array(get_tree(ex), cX, get_settings(ex, settings); kws...)
end

import ..EvaluationHelpersModule: _grad_evaluator

function _grad_evaluator(
    ex::AbstractExpression, cX::AbstractMatrix, settings=nothing; kws...
)
    _validate_input(ex, cX, settings)
    return _grad_evaluator(get_tree(ex), cX, get_settings(ex, settings); kws...)
end
function (ex::AbstractExpression)(X, settings=nothing; kws...)
    _validate_input(ex, X, settings)
    return get_tree(ex)(X, get_settings(ex, settings); kws...)
end

import ..SimplifyModule: combine_operators, simplify_tree!
import Base: copy, hash

# Avoid implementing a generic version for these, as it is less likely to generalize
function combine_operators(ex::Expression, settings=nothing; kws...)
    return combine_operators(get_tree(ex), get_settings(ex, settings); kws...)
end
function simplify_tree!(ex::Expression, settings=nothing; kws...)
    return simplify_tree!(get_tree(ex), get_settings(ex, settings); kws...)
end
function copy(ex::Expression)
    return Expression(copy(ex.tree), copy(ex.settings), copy(ex.variable_names))
end

end
