module ChainRulesModule

import ChainRulesCore: rrule, frule, ZeroTangent, Tangent
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
        y, ∂y_∂X, completed = eval_grad_tree_array(
            tree, X, operators; variable=true, turbo=turbo
        )
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
            tree, X, operators; variable=false, turbo=turbo
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

end
