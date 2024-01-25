module DynamicExpressionsOptimExt

using DynamicExpressions: AbstractExpressionNode, eval_tree_array
using Compat: @inline

import Optim: optimize

"""Wrap f with insertion of values of the constant nodes."""
function get_wrapped_f(
    f::F, tree::N, constant_nodes::AbstractArray{N}
) where {F,T,N<:AbstractExpressionNode{T}}
    function wrapped_f(x)
        for (ci, xi) in zip(constant_nodes, x)
            ci.val::T = xi::T
        end
        return @inline(f(tree))
    end
    return wrapped_f
end

"""Wrap g! or h! with insertion of values of the constant nodes."""
function get_wrapped_gh!(
    gh!::GH, tree::N, constant_nodes::AbstractArray{N}
) where {GH,T,N<:AbstractExpressionNode{T}}
    function wrapped_gh!(G, x)
        for (ci, xi) in zip(constant_nodes, x)
            ci.val::T = xi::T
        end
        @inline(gh!(G, tree))
        return nothing
    end
    return wrapped_gh!
end

function optimize(f::F, g!::G, h!::H, tree::AbstractExpressionNode{T}, args...; kwargs...) where {F,G,H,T}
    constant_nodes = filter(t -> t.degree == 0 && t.constant, tree)
    x0 = T[t.val::T for t in constant_nodes]
    if g! === nothing
        @assert h! === nothing
        return optimize(get_wrapped_f(f, tree, constant_nodes), x0, args...; kwargs...)
    elseif h! === nothing
        return optimize(
            get_wrapped_f(f, tree, constant_nodes),
            get_wrapped_gh!(g!, tree, constant_nodes),
            x0,
            args...;
            kwargs...,
        )
    else
        return optimize(
            get_wrapped_f(f, tree, constant_nodes),
            get_wrapped_gh!(g!, tree, constant_nodes),
            get_wrapped_gh!(h!, tree, constant_nodes),
            x0,
            args...;
            kwargs...,
        )
    end
end
function optimize(f::F, g!::G, tree::AbstractExpressionNode, args...; kwargs...) where {F,G}
    return optimize(f, g!, nothing, tree, args...; kwargs...)
end
function optimize(f::F, tree::AbstractExpressionNode, args...; kwargs...) where {F}
    return optimize(f, nothing, tree, args...; kwargs...)
end

end
