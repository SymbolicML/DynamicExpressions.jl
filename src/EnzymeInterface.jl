module EnzymeInterfaceModule

import Enzyme: autodiff, Duplicated, Const, Forward, Reverse
import ..EquationModule: Node, copy_node
import ..EquationUtilsModule: set_constants!, get_constants
import ..EvaluateEquationModule: eval_tree_array
import ..OperatorEnumModule: OperatorEnum

function _eval_tree_array!(
    result::AbstractVector{T},
    tree::Node{T},
    constants::C,
    X::AbstractMatrix{T},
    operators::OperatorEnum,
) where {T,C}
    if !(C <: Nothing)
        set_constants!(tree, constants)
    end
    out, completed = eval_tree_array(tree, X, operators; turbo=false)
    if !completed
        result .= T(NaN)
    else
        result .= out
    end
    return nothing
end

function enzyme_reverse_gradient(
    tree::Node{T}, X::AbstractMatrix{T}, operators::OperatorEnum; variable::Bool=false
) where {T}
    nfeatures, nrows = size(X)
    result = zeros(T, nrows)
    if variable
        dX = zeros(T, nfeatures, nrows)
        dresult = ones(T, nrows)
        autodiff(
            Reverse,
            _eval_tree_array!,
            Duplicated(result, dresult),
            Const(tree),
            Const(nothing),
            Duplicated(X, dX),
            Const(operators),
        )
        return dX
    else
        ctree = copy_node(tree)
        c = get_constants(tree)
        set_constants!(ctree, c .* zero(T))
        output = zeros(T, length(c), nrows)
        for i in 1:nrows
            dresult = zeros(T, nrows)
            dresult[i] = 1
            cc = copy(c) .* zero(T)
            autodiff(
                Reverse,
                _eval_tree_array!,
                Duplicated(result, dresult),
                Duplicated(tree, ctree),
                Duplicated(c, cc),
                Const(X),
                Const(operators),
            )
            output[:, i] .= cc
        end
        return output
    end
end

function enzyme_forward_diff_constants(
    i::Int,
    tree::Node{T},
    constants::AbstractVector{T},
    X::AbstractMatrix{T},
    operators::OperatorEnum,
) where {T}
    nrows = size(X, 2)
    result = zeros(T, nrows)
    dresult = ones(T, nrows)
    dconstants = zeros(T, length(constants))
    dconstants[i] = T(1)
    autodiff(
        Forward,
        _eval_tree_array!,
        Duplicated(result, dresult),
        Duplicated(tree, copy_node(tree)),
        Duplicated(constants, dconstants),
        Const(X),
        Const(operators),
    )
    return dresult
end

function enzyme_forward_diff_variables(
    i::Int, tree::Node{T}, X::AbstractMatrix{T}, operators::OperatorEnum
) where {T}
    nfeatures, nrows = size(X)
    result = zeros(T, nrows)
    dresult = ones(T, nrows)
    dX = zeros(T, nfeatures, nrows)
    dX[i, :] .= T(1)
    autodiff(
        Forward,
        _eval_tree_array!,
        Duplicated(result, dresult),
        Const(tree),
        Const(nothing),
        Duplicated(X, dX),
        Const(operators),
    )
    return dresult
end

function enzyme_forward_gradient(
    tree::Node{T}, X::AbstractMatrix{T}, operators::OperatorEnum; variable::Bool=false
) where {T}
    if variable
        output = similar(X)
        nfeatures = size(X, 1)
        for i in 1:nfeatures
            output[i, :] .= enzyme_forward_diff_variables(i, tree, X, operators)
        end
        return output
    else
        constants = get_constants(tree)
        nrows = size(X, 2)
        n_constants = length(constants)
        output = zeros(T, n_constants, nrows)
        for i in 1:n_constants
            output[i, :] .= enzyme_forward_diff_constants(i, tree, constants, X, operators)
        end
        return output
    end
end

end
