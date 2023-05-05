module ChainRulesModule

import ChainRulesCore: rrule, frule, ZeroTangent, NoTangent, Tangent, @thunk
import LinearAlgebra: pinv
import ..EquationModule: Node
import ..OperatorEnumModule: OperatorEnum
import ..EvaluateEquationModule: eval_tree_array
import ..EvaluateEquationDerivativeModule: eval_grad_tree_array, eval_diff_tree_array

# e.g.,
# function frule((_, Δx), ::typeof(sin), x)
#     return sin(x), cos(x) * Δx
# end

function frule(
    (Δself, _, ΔX, _),
    ::typeof(eval_tree_array),
    tree::Node{T},
    X::AbstractMatrix{T},
    operators::OperatorEnum;
    turbo::Bool=false,
) where {T}
    # Δself: (nconstants)
    # ΔX: (nfeatures, nrows)
    # X: (nfeatures, nrows)

    y = zeros(T, size(X, 2))
    ∂y = zeros(T, size(X, 2))

    if !(typeof(ΔX) <: ZeroTangent)
        y, ∂y_∂X, completed = eval_grad_tree_array(tree, X, operators; variable=true, turbo)
        # y: (nrows)
        # ∂y_∂X: (nfeatures, nrows)
        if !completed
            y .= T(NaN)
            ∂y_∂X .= T(NaN)
        end
        for i in axes(∂y_∂X, 1), j in axes(∂y_∂X, 2)
            ∂y[j] += ∂y_∂X[i, j] * ΔX[i, j]
        end
    end
    if !(typeof(Δself) <: ZeroTangent)
        y, ∂y_∂self, completed = eval_grad_tree_array(
            tree, X, operators; variable=false, turbo
        )
        # y: (nrows)
        # ∂y_∂self: (nconstants, nrows)
        if !completed
            y .= T(NaN)
            ∂y_∂X .= T(NaN)
        end
        for i in axes(∂y_∂self, 1), j in axes(∂y_∂self, 2)
            ∂y[j] += ∂y_∂self[i, j] * Δself[i]
        end
    end

    return y, ∂y
end

function rrule(
    ::typeof(eval_tree_array),
    tree::Node{T},
    X::AbstractMatrix{T},
    operators::OperatorEnum;
    turbo::Bool=false,
) where {T}
    y, complete = eval_tree_array(tree, X, operators; turbo)
    !complete && (y = T(NaN))
    function eval_tree_array_pullback(Δy, args...)
        _, ∂y_∂X, completed1 = eval_grad_tree_array(
            tree, X, operators; variable=true, turbo=turbo
        )
        _, ∂y_∂self, completed2 = eval_grad_tree_array(
            tree, X, operators; variable=false, turbo=turbo
        )

        if !completed1 || !completed2
            ∂y_∂X = ∂y_∂X .* T(NaN)
            ∂y_∂self = ∂y_∂self .* T(NaN)
        end

        Δoperators = NoTangent()
        Δturbo = NoTangent()

        Δself = @thunk(pinv(∂y_∂self') * Δy)
        ΔX = @thunk(pinv(∂y_∂X') * Δy)
        return (Δself, ΔX, Δoperators, Δturbo)
    end
    # ∂self, ∂args... = pullback(Δy)
    return y, eval_tree_array_pullback
end

end
