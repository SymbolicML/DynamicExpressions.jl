module DynamicExpressionsOptimExt

using DynamicExpressions:
    AbstractExpressionNode, filter_map, eval_tree_array, get_constants, set_constants!
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
function Optim.minimizer(r::ExpressionOptimizationResults)
    return r.tree
end

"""Wrap function or objective with insertion of values of the constant nodes."""
function wrap_func(
    f::F, tree::N, refs::AbstractArray
) where {F<:Function,T,N<:AbstractExpressionNode{T}}
    function wrapped_f(args::Vararg{Any,M}) where {M}
        (first_args..., x) = args
        set_constants!(tree, x, refs)
        return @inline(f(first_args..., tree))
    end
    return wrapped_f
end
function wrap_func(
    ::Nothing, tree::N, refs::AbstractArray
) where {N<:AbstractExpressionNode}
    return nothing
end
function wrap_func(
    f::NLSolversBase.InplaceObjective, tree::N, refs::AbstractArray
) where {N<:AbstractExpressionNode}
    # Some objectives, like `Optim.only_fg!(fg!)`, are not functions but instead
    # `InplaceObjective`. These contain multiple functions, each of which needs to be
    # wrapped. Some functions are `nothing`; those can be left as-is.
    @assert fieldnames(NLSolversBase.InplaceObjective) == (:df, :fdf, :fgh, :hv, :fghv)
    return NLSolversBase.InplaceObjective(
        wrap_func(f.df, tree, refs),
        wrap_func(f.fdf, tree, refs),
        wrap_func(f.fgh, tree, refs),
        wrap_func(f.hv, tree, refs),
        wrap_func(f.fghv, tree, refs),
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
    x0, refs = get_constants(tree)
    if !isnothing(h!)
        throw(
            ArgumentError(
                "Optim.optimize does not yet support Hessians on `AbstractExpressionNode`. " *
                "Please raise an issue at github.com/SymbolicML/DynamicExpressions.jl.",
            ),
        )
    end
    base_res = if isnothing(g!)
        Optim.optimize(wrap_func(f, tree, refs), x0, args...; kwargs...)
    else
        Optim.optimize(
            wrap_func(f, tree, refs), wrap_func(g!, tree, refs), x0, args...; kwargs...
        )
    end
    minimizer = Optim.minimizer(base_res)
    set_constants!(tree, minimizer, refs)
    return ExpressionOptimizationResults(base_res, tree)
end

end
