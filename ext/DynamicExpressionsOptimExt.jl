module DynamicExpressionsOptimExt

using DynamicExpressions: AbstractExpressionNode, eval_tree_array
using Compat: @inline

import Optim: Optim, OptimizationResults, NLSolversBase

#! format: off
"""
    ExpressionOptimizationResults{R,N<:AbstractExpressionNode}

Optimization results for an expression, which wraps the base optimization results
on a vector of constants.
"""
struct ExpressionOptimizationResults{R<:OptimizationResults,N<:AbstractExpressionNode} <: OptimizationResults
    _results::R # The raw results from Optim.
    tree::N # The final expression tree
end
#! format: on
function Base.getproperty(r::ExpressionOptimizationResults, s::Symbol)
    if s == :tree || s == :minimizer
        return getfield(r, :tree)
    else
        return getproperty(getfield(r, :_results), s)
    end
end
function Base.propertynames(r::ExpressionOptimizationResults)
    return (:tree, propertynames(getfield(r, :_results))...)
end
function base_results(r::ExpressionOptimizationResults)
    return getfield(r, :_results)
end
function Optim.minimizer(r::ExpressionOptimizationResults)
    return r.tree
end

function set_constant_nodes!(
    constant_nodes::AbstractArray{N}, x
) where {T,N<:AbstractExpressionNode{T}}
    for (ci, xi) in zip(constant_nodes, x)
        ci.val::T = xi::T
    end
    return nothing
end

"""Wrap function or objective with insertion of values of the constant nodes."""
function wrap_func(
    f::F, tree::N, constant_nodes::AbstractArray{N}
) where {F<:Function,T,N<:AbstractExpressionNode{T}}
    function wrapped_f(args::Vararg{Any,M}) where {M}
        first_args = args[1:(end - 1)]
        x = last(args)
        set_constant_nodes!(constant_nodes, x)
        return @inline(f(first_args..., tree))
    end
    return wrapped_f
end
function wrap_func(
    ::Nothing, tree::N, constant_nodes::AbstractArray{N}
) where {N<:AbstractExpressionNode}
    return nothing
end
function wrap_func(
    f::NLSolversBase.InplaceObjective, tree::N, constant_nodes::AbstractArray{N}
) where {N<:AbstractExpressionNode}
    # Some objectives, like `Optim.only_fg!(fg!)`, are not functions but instead
    # `InplaceObjective`. These contain multiple functions, each of which needs to be
    # wrapped. Some functions are `nothing`; those can be left as-is.
    @assert fieldnames(NLSolversBase.InplaceObjective) == (:df, :fdf, :fgh, :hv, :fghv)
    return NLSolversBase.InplaceObjective(
        wrap_func(f.df, tree, constant_nodes),
        wrap_func(f.fdf, tree, constant_nodes),
        wrap_func(f.fgh, tree, constant_nodes),
        wrap_func(f.hv, tree, constant_nodes),
        wrap_func(f.fghv, tree, constant_nodes),
    )
end

"""
    optimize(f, [g!, [h!,]] tree, args...; kwargs...)

Optimize an expression tree with respect to the constants in the tree.
Returns an `ExpressionOptimizationResults` object, which wraps the base
optimization results on a vector of constants. You may use `res.minimizer`
to view the optimized expression tree.
"""
function Optim.optimize(f::F, tree::AbstractExpressionNode, args...; kwargs...) where {F}
    return Optim.optimize(f, nothing, tree, args...; kwargs...)
end
function Optim.optimize(
    f::F, g!::G, tree::AbstractExpressionNode, args...; kwargs...
) where {F,G}
    return Optim.optimize(f, g!, nothing, tree, args...; kwargs...)
end
function Optim.optimize(
    f::F, g!::G, h!::H, tree::AbstractExpressionNode{T}, args...; make_copy=true, kwargs...
) where {F,G,H,T}
    if make_copy
        tree = copy(tree)
    end
    constant_nodes = filter(t -> t.degree == 0 && t.constant, tree)
    x0 = T[t.val::T for t in constant_nodes]
    base_res = if g! === nothing
        @assert h! === nothing
        Optim.optimize(wrap_func(f, tree, constant_nodes), x0, args...; kwargs...)
    elseif h! === nothing
        Optim.optimize(
            wrap_func(f, tree, constant_nodes),
            wrap_func(g!, tree, constant_nodes),
            x0,
            args...;
            kwargs...,
        )
    else
        Optim.optimize(
            wrap_func(f, tree, constant_nodes),
            wrap_func(g!, tree, constant_nodes),
            wrap_func(h!, tree, constant_nodes),
            x0,
            args...;
            kwargs...,
        )
    end
    set_constant_nodes!(constant_nodes, Optim.minimizer(base_res))
    return ExpressionOptimizationResults(base_res, tree)
end

end
