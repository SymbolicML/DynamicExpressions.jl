module DynamicExpressionsOptimExt

using DynamicExpressions:
    AbstractExpression,
    AbstractExpressionNode,
    filter_map,
    eval_tree_array,
    get_scalar_constants,
    set_scalar_constants!,
    get_number_type

import Optim: Optim, OptimizationResults, NLSolversBase

#! format: off
"""
    ExpressionOptimizationResults{R,N<:AbstractExpressionNode}

Optimization results for an expression, which wraps the base optimization results
on a vector of constants.
"""
struct ExpressionOptimizationResults{R<:OptimizationResults,N<:Union{AbstractExpressionNode,AbstractExpression}} <: OptimizationResults
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
@inline function _wrap_objective_x_last(
    ::Nothing, tree::N, refs
) where {N<:Union{AbstractExpressionNode,AbstractExpression}}
    return nothing
end
@inline function _wrap_objective_x_last(
    f::F, tree::N, refs
) where {F<:Function,T,N<:Union{AbstractExpressionNode{T},AbstractExpression{T}}}
    function wrapped_f(args::Vararg{Any,M}) where {M}
        x = args[M]
        set_scalar_constants!(tree, x, refs)
        newargs = Base.setindex(args, tree, M)
        return @inline(f(newargs...))
    end
    return wrapped_f
end

@inline function _wrap_objective_xv_tail(
    ::Nothing, tree::N, refs
) where {N<:Union{AbstractExpressionNode,AbstractExpression}}
    return nothing
end
@inline function _wrap_objective_xv_tail(
    f::F, tree::N, refs
) where {F<:Function,T,N<:Union{AbstractExpressionNode{T},AbstractExpression{T}}}
    function wrapped_f(args::Vararg{Any,M}) where {M}
        if M < 2
            throw(
                ArgumentError(
                    "Expected at least 2 arguments for objective functions of the form (..., x, v).",
                ),
            )
        end
        x = args[M - 1]
        set_scalar_constants!(tree, x, refs)
        newargs = Base.setindex(args, tree, M - 1)
        return @inline(f(newargs...))
    end
    return wrapped_f
end

function wrap_func(
    f::F, tree::N, refs
) where {F<:Function,T,N<:Union{AbstractExpressionNode{T},AbstractExpression{T}}}
    return _wrap_objective_x_last(f, tree, refs)
end
function wrap_func(
    ::Nothing, tree::N, refs
) where {N<:Union{AbstractExpressionNode,AbstractExpression}}
    return nothing
end

function wrap_func(
    f::NLSolversBase.InplaceObjective, tree::N, refs
) where {N<:Union{AbstractExpressionNode,AbstractExpression}}
    # Some objectives, like `only_fg!(fg!)`, are not functions but instead
    # `InplaceObjective`. These contain multiple functions, each of which needs to be
    # wrapped. Some functions are `nothing`; those can be left as-is.
    #
    # We use `@static` branching so that only the relevant layout for the *installed*
    # NLSolversBase version is compiled/instrumented.
    @static if fieldnames(NLSolversBase.InplaceObjective) == (:fdf, :fgh, :hvp, :fghvp, :fjvp)
        # NLSolversBase v8 / Optim v2
        return NLSolversBase.InplaceObjective(
            _wrap_objective_x_last(getfield(f, :fdf), tree, refs),
            _wrap_objective_x_last(getfield(f, :fgh), tree, refs),
            _wrap_objective_xv_tail(getfield(f, :hvp), tree, refs),
            _wrap_objective_xv_tail(getfield(f, :fghvp), tree, refs),
            _wrap_objective_xv_tail(getfield(f, :fjvp), tree, refs),
        )
    elseif fieldnames(NLSolversBase.InplaceObjective) == (:df, :fdf, :fgh, :hv, :fghv)
        # NLSolversBase v7 / Optim v1
        return NLSolversBase.InplaceObjective(
            _wrap_objective_x_last(getfield(f, :df), tree, refs),
            _wrap_objective_x_last(getfield(f, :fdf), tree, refs),
            _wrap_objective_x_last(getfield(f, :fgh), tree, refs),
            _wrap_objective_xv_tail(getfield(f, :hv), tree, refs),
            _wrap_objective_xv_tail(getfield(f, :fghv), tree, refs),
        )
    elseif fieldnames(NLSolversBase.InplaceObjective) == (:fdf, :fgh, :hv, :fghv)
        # Older NLSolversBase / Optim
        return NLSolversBase.InplaceObjective(
            _wrap_objective_x_last(getfield(f, :fdf), tree, refs),
            _wrap_objective_x_last(getfield(f, :fgh), tree, refs),
            _wrap_objective_xv_tail(getfield(f, :hv), tree, refs),
            _wrap_objective_xv_tail(getfield(f, :fghv), tree, refs),
        )
    else
        fields = fieldnames(NLSolversBase.InplaceObjective)
        throw(
            ArgumentError(
                "Unsupported NLSolversBase.InplaceObjective field layout: $(fields). " *
                "This extension supports layouts used by Optim v1 and v2. " *
                "Please open an issue at github.com/SymbolicML/DynamicExpressions.jl with your versions.",
            ),
        )
    end
end

"""
    optimize(f, [g!, [h!,]] tree, args...; kwargs...)

Optimize an expression tree with respect to the constants in the tree.
Returns an `ExpressionOptimizationResults` object, which wraps the base
optimization results on a vector of constants. You may use `res.minimizer`
to view the optimized expression tree.
"""
function Optim.optimize(
    f::F, tree::Union{AbstractExpressionNode,AbstractExpression}, args...; kwargs...
) where {F}
    return Optim.optimize(f, nothing, tree, args...; kwargs...)
end
function Optim.optimize(
    f::F, g!::G, tree::Union{AbstractExpressionNode,AbstractExpression}, args...; kwargs...
) where {F,G}
    return Optim.optimize(f, g!, nothing, tree, args...; kwargs...)
end
function Optim.optimize(
    f::F,
    g!::G,
    h!::H,
    tree::Union{AbstractExpressionNode{T},AbstractExpression{T}},
    args...;
    make_copy=true,
    kwargs...,
) where {F,G,H,T}
    if make_copy
        tree = copy(tree)
    end

    x0, refs = get_scalar_constants(tree)
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
    set_scalar_constants!(tree, minimizer, refs)
    return ExpressionOptimizationResults(base_res, tree)
end

end
