module EvaluateEquationDerivativeModule

import LoopVectorization: indices, @turbo
import ..EquationModule: Node
import ..OperatorEnumModule: OperatorEnum
import ..UtilsModule: @maybe_turbo, is_bad_array, fill_similar
import ..EquationUtilsModule: count_constants, index_constants, NodeIndex
import ..EvaluateEquationModule: deg0_eval

struct ResultOk2{A<:AbstractArray,B<:AbstractArray}
    x::A
    dx::B
    ok::Bool
end

_zygote_gradient(args...) = error("Please load the Zygote.jl package.")

"""
    eval_diff_tree_array(tree::Node{T}, cX::AbstractMatrix{T}, operators::OperatorEnum, direction::Integer; turbo::Bool=false)

Compute the forward derivative of an expression, using a similar
structure and optimization to eval_tree_array. `direction` is the index of a particular
variable in the expression. e.g., `direction=1` would indicate derivative with
respect to `x1`.

# Arguments

- `tree::Node`: The expression tree to evaluate.
- `cX::AbstractMatrix{T}`: The data matrix, with each column being a data point.
- `operators::OperatorEnum`: The operators used to create the `tree`.
- `direction::Integer`: The index of the variable to take the derivative with respect to.
- `turbo::Bool`: Use `LoopVectorization.@turbo` for faster evaluation.

# Returns

- `(evaluation, derivative, complete)::Tuple{AbstractVector{T}, AbstractVector{T}, Bool}`: the normal evaluation,
    the derivative, and whether the evaluation completed as normal (or encountered a nan or inf).
"""
function eval_diff_tree_array(
    tree::Node{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    direction::Integer;
    turbo::Bool=false,
) where {T<:Number}
    # TODO: Implement quick check for whether the variable is actually used
    # in this tree. Otherwise, return zero.
    result = _eval_diff_tree_array(
        tree, cX, operators, direction, (turbo ? Val(true) : Val(false))
    )
    return (result.x, result.dx, result.ok)
end
function eval_diff_tree_array(
    tree::Node{T1},
    cX::AbstractMatrix{T2},
    operators::OperatorEnum,
    direction::Integer;
    turbo::Bool=false,
) where {T1<:Number,T2<:Number}
    T = promote_type(T1, T2)
    @warn "Warning: eval_diff_tree_array received mixed types: tree=$(T1) and data=$(T2)."
    tree = convert(Node{T}, tree)
    cX = Base.Fix1(convert, T).(cX)
    return eval_diff_tree_array(tree, cX, operators, direction; turbo=turbo)
end

function _eval_diff_tree_array(
    tree::Node{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    direction::Integer,
    ::Val{turbo},
)::ResultOk2 where {T<:Number,turbo}
    result = if tree.degree == 0
        diff_deg0_eval(tree, cX, direction)
    elseif tree.degree == 1
        op_idx = tree.op
        nuna = length(operators.unaops)
        Base.Cartesian.@nif(
            16,
            i -> i == op_idx,
            i -> let op = operators.unaops[min(i < 16 ? i : op_idx, nuna)]
                @assert i <= nuna
                diff_deg1_eval(tree, cX, op, operators, direction, Val(turbo))
            end
        )
    else
        op_idx = tree.op
        nbin = length(operators.binops)
        Base.Cartesian.@nif(
            16,
            i -> i == op_idx,
            i -> let op = operators.binops[min(i < 16 ? i : op_idx, nbin)]
                @assert i <= nbin
                diff_deg2_eval(tree, cX, op, operators, direction, Val(turbo))
            end
        )
    end
    !result.ok && return result
    return ResultOk2(
        result.x, result.dx, !(is_bad_array(result.x) || is_bad_array(result.dx))
    )
end

function diff_deg0_eval(
    tree::Node{T}, cX::AbstractMatrix{T}, direction::Integer
) where {T<:Number}
    const_part = deg0_eval(tree, cX).x
    derivative_part = if ((!tree.constant) && tree.feature == direction)
        fill_similar(one(T), cX, axes(cX, 2))
    else
        fill_similar(zero(T), cX, axes(cX, 2))
    end
    return ResultOk2(const_part, derivative_part, true)
end

function diff_deg1_eval(
    tree::Node{T},
    cX::AbstractMatrix{T},
    op::F,
    operators::OperatorEnum,
    direction::Integer,
    ::Val{turbo},
) where {T<:Number,F,turbo}
    result = _eval_diff_tree_array(tree.l, cX, operators, direction, Val(turbo))
    !result.ok && return result

    # TODO - add type assertions to get better speed:
    cumulator = result.x
    dcumulator = result.dx
    diff_op = _zygote_gradient(op, Val(1), Val(turbo))
    @maybe_turbo turbo for j in eachindex(cumulator)
        x = op(cumulator[j])::T
        dx = diff_op(cumulator[j])::T * dcumulator[j]

        cumulator[j] = x
        dcumulator[j] = dx
    end
    return result
end

function diff_deg2_eval(
    tree::Node{T},
    cX::AbstractMatrix{T},
    op::F,
    operators::OperatorEnum,
    direction::Integer,
    ::Val{turbo},
) where {T<:Number,F,turbo}
    result_l = _eval_diff_tree_array(tree.l, cX, operators, direction, Val(turbo))
    !result_l.ok && return result_l
    result_r = _eval_diff_tree_array(tree.r, cX, operators, direction, Val(turbo))
    !result_r.ok && return result_r

    ar_l = result_l.x
    d_ar_l = result_l.dx
    ar_r = result_r.x
    d_ar_r = result_r.dx
    diff_op = _zygote_gradient(op, Val(2), Val(turbo))

    @maybe_turbo turbo for j in eachindex(ar_l)
        x = op(ar_l[j], ar_r[j])::T

        first, second = diff_op(ar_l[j], ar_r[j])::Tuple{T,T}
        dx = first * d_ar_l[j] + second * d_ar_r[j]

        ar_l[j] = x
        d_ar_l[j] = dx
    end
    return result_l
end

"""
    eval_grad_tree_array(tree::Node{T}, cX::AbstractMatrix{T}, operators::OperatorEnum; variable::Bool=false, turbo::Bool=false)

Compute the forward-mode derivative of an expression, using a similar
structure and optimization to eval_tree_array. `variable` specifies whether
we should take derivatives with respect to features (i.e., cX), or with respect
to every constant in the expression.

# Arguments

- `tree::Node{T}`: The expression tree to evaluate.
- `cX::AbstractMatrix{T}`: The data matrix, with each column being a data point.
- `operators::OperatorEnum`: The operators used to create the `tree`.
- `variable::Bool`: Whether to take derivatives with respect to features (i.e., `cX` - with `variable=true`),
    or with respect to every constant in the expression (`variable=false`).
- `turbo::Bool`: Use `LoopVectorization.@turbo` for faster evaluation.

# Returns

- `(evaluation, gradient, complete)::Tuple{AbstractVector{T}, AbstractMatrix{T}, Bool}`: the normal evaluation,
    the gradient, and whether the evaluation completed as normal (or encountered a nan or inf).
"""
function eval_grad_tree_array(
    tree::Node{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum;
    variable::Union{Val,Bool}=Val{false}(),
    turbo::Union{Val,Bool}=Val{false}(),
) where {T<:Number}
    n_gradients = if isa(variable, Val{true}) || (isa(variable, Bool) && variable)
        size(cX, 1)::Int
    else
        count_constants(tree)::Int
    end
    index_tree = index_constants(tree, UInt16(0))
    result = eval_grad_tree_array(
        tree,
        n_gradients,
        index_tree,
        cX,
        operators,
        if isa(variable, Val)
            variable
        else
            variable ? Val(true) : Val(false)
        end,
        if isa(turbo, Val)
            turbo
        else
            turbo ? Val(true) : Val(false)
        end,
    )
    return (result.x, result.dx, result.ok)
end

function eval_grad_tree_array(
    tree::Node{T},
    n_gradients,
    index_tree::NodeIndex,
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    ::Val{variable},
    ::Val{turbo},
)::ResultOk2 where {T<:Number,variable,turbo}
    result = _eval_grad_tree_array(
        tree, n_gradients, index_tree, cX, operators, Val(variable), Val(turbo)
    )
    !result.ok && return result
    return ResultOk2(
        result.x, result.dx, !(is_bad_array(result.x) || is_bad_array(result.dx))
    )
end

function eval_grad_tree_array(
    tree::Node{T1},
    cX::AbstractMatrix{T2},
    operators::OperatorEnum;
    variable::Bool=false,
    turbo::Bool=false,
) where {T1<:Number,T2<:Number}
    T = promote_type(T1, T2)
    return eval_grad_tree_array(
        convert(Node{T}, tree),
        convert(AbstractMatrix{T}, cX),
        operators;
        variable=variable,
        turbo=turbo,
    )
end

function _eval_grad_tree_array(
    tree::Node{T},
    n_gradients,
    index_tree::NodeIndex,
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    ::Val{variable},
    ::Val{turbo},
)::ResultOk2 where {T<:Number,variable,turbo}
    if tree.degree == 0
        grad_deg0_eval(tree, n_gradients, index_tree, cX, Val(variable))
    elseif tree.degree == 1
        op_idx = tree.op
        nuna = length(operators.unaops)
        Base.Cartesian.@nif(
            16,
            i -> i == op_idx,
            i -> let op = operators.unaops[min(i < 16 ? i : op_idx, nuna)]
                @assert i <= nuna
                grad_deg1_eval(
                    tree,
                    n_gradients,
                    index_tree,
                    cX,
                    op,
                    operators,
                    Val(variable),
                    Val(turbo),
                )
            end
        )
    else
        op_idx = tree.op
        nbin = length(operators.binops)
        Base.Cartesian.@nif(
            16,
            i -> i == op_idx,
            i -> let op = operators.binops[min(i < 16 ? i : op_idx, nbin)]
                @assert i <= nbin
                grad_deg2_eval(
                    tree,
                    n_gradients,
                    index_tree,
                    cX,
                    op,
                    operators,
                    Val(variable),
                    Val(turbo),
                )
            end
        )
    end
end

function grad_deg0_eval(
    tree::Node{T},
    n_gradients,
    index_tree::NodeIndex,
    cX::AbstractMatrix{T},
    ::Val{variable},
)::ResultOk2 where {T<:Number,variable}
    const_part = deg0_eval(tree, cX).x

    zero_mat = if isa(cX, Array)
        fill_similar(zero(T), cX, n_gradients, axes(cX, 2))
    else
        hcat([fill_similar(zero(T), cX, axes(cX, 2)) for _ in 1:n_gradients]...)'
    end

    if variable == tree.constant
        return ResultOk2(const_part, zero_mat, true)
    else
        if variable
            zero_mat[tree.feature, :] .= one(T)
        else
            zero_mat[index_tree.constant_index, :] .= one(T)
        end

        return ResultOk2(const_part, zero_mat, true)
    end
end

function grad_deg1_eval(
    tree::Node{T},
    n_gradients,
    index_tree::NodeIndex,
    cX::AbstractMatrix{T},
    op::F,
    operators::OperatorEnum,
    ::Val{variable},
    ::Val{turbo},
)::ResultOk2 where {T<:Number,F,variable,turbo}
    result = eval_grad_tree_array(
        tree.l, n_gradients, index_tree.l, cX, operators, Val(variable), Val(turbo)
    )
    !result.ok && return result

    cumulator = result.x
    dcumulator = result.dx
    diff_op = _zygote_gradient(op, Val(1), Val(turbo))
    @maybe_turbo turbo for j in axes(dcumulator, 2)
        x = op(cumulator[j])::T
        dx = diff_op(cumulator[j])::T

        cumulator[j] = x
        for k in axes(dcumulator, 1)
            dcumulator[k, j] = dx * dcumulator[k, j]
        end
    end
    return result
end

function grad_deg2_eval(
    tree::Node{T},
    n_gradients,
    index_tree::NodeIndex,
    cX::AbstractMatrix{T},
    op::F,
    operators::OperatorEnum,
    ::Val{variable},
    ::Val{turbo},
)::ResultOk2 where {T<:Number,F,variable,turbo}
    result_l = eval_grad_tree_array(
        tree.l, n_gradients, index_tree.l, cX, operators, Val(variable), Val(turbo)
    )
    !result_l.ok && return result_l
    result_r = eval_grad_tree_array(
        tree.r, n_gradients, index_tree.r, cX, operators, Val(variable), Val(turbo)
    )
    !result_r.ok && return result_r

    cumulator_l = result_l.x
    dcumulator_l = result_l.dx
    cumulator_r = result_r.x
    dcumulator_r = result_r.dx
    diff_op = _zygote_gradient(op, Val(2), Val(turbo))
    @maybe_turbo turbo for j in axes(dcumulator_l, 2)
        c1 = cumulator_l[j]
        c2 = cumulator_r[j]
        x = op(c1, c2)::T
        dx1, dx2 = diff_op(c1, c2)::Tuple{T,T}
        cumulator_l[j] = x
        for k in axes(dcumulator_l, 1)
            dcumulator_l[k, j] = dx1 * dcumulator_l[k, j] + dx2 * dcumulator_r[k, j]
        end
    end

    return result_l
end

end
