module EvaluateEquationDerivativeModule

import ..EquationModule: AbstractExpressionNode, constructorof
import ..OperatorEnumModule: OperatorEnum
import ..UtilsModule: is_bad_array, fill_similar
import ..EquationUtilsModule: count_constants, index_constants, NodeIndex
import ..EvaluateEquationModule:
    deg0_eval, get_nuna, get_nbin, OPERATOR_LIMIT_BEFORE_SLOWDOWN
import ..ExtensionInterfaceModule: _zygote_gradient

struct ResultOk2{A<:AbstractArray,B<:AbstractArray}
    x::A
    dx::B
    ok::Bool
end

"""
    eval_diff_tree_array(tree::AbstractExpressionNode{T}, cX::AbstractMatrix{T}, operators::OperatorEnum, direction::Integer; turbo::Bool=false)

Compute the forward derivative of an expression, using a similar
structure and optimization to eval_tree_array. `direction` is the index of a particular
variable in the expression. e.g., `direction=1` would indicate derivative with
respect to `x1`.

# Arguments

- `tree::AbstractExpressionNode`: The expression tree to evaluate.
- `cX::AbstractMatrix{T}`: The data matrix, with each column being a data point.
- `operators::OperatorEnum`: The operators used to create the `tree`.
- `direction::Integer`: The index of the variable to take the derivative with respect to.
- `turbo::Bool`: Use `LoopVectorization.@turbo` for faster evaluation.

# Returns

- `(evaluation, derivative, complete)::Tuple{AbstractVector{T}, AbstractVector{T}, Bool}`: the normal evaluation,
    the derivative, and whether the evaluation completed as normal (or encountered a nan or inf).
"""
function eval_diff_tree_array(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    direction::Integer;
    turbo::Bool=false,
) where {T<:Number}
    # TODO: Implement quick check for whether the variable is actually used
    # in this tree. Otherwise, return zero.

    # TODO: `turbo` slows performance. Need to fix.
    result = _eval_diff_tree_array(tree, cX, operators, direction)
    return (result.x, result.dx, result.ok)
end
function eval_diff_tree_array(
    tree::AbstractExpressionNode{T1},
    cX::AbstractMatrix{T2},
    operators::OperatorEnum,
    direction::Integer;
    turbo::Bool=false,
) where {T1<:Number,T2<:Number}
    T = promote_type(T1, T2)
    @warn "Warning: eval_diff_tree_array received mixed types: tree=$(T1) and data=$(T2)."
    tree = convert(constructorof(typeof(tree)){T}, tree)
    cX = Base.Fix1(convert, T).(cX)
    return eval_diff_tree_array(tree, cX, operators, direction; turbo=turbo)
end

@generated function _eval_diff_tree_array(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    direction::Integer,
)::ResultOk2 where {T<:Number}
    nuna = get_nuna(operators)
    nbin = get_nbin(operators)
    deg1_branch = if nuna > OPERATOR_LIMIT_BEFORE_SLOWDOWN
        quote
            diff_deg1_eval(tree, cX, operators.unaops[op_idx], operators, direction)
        end
    else
        quote
            Base.Cartesian.@nif(
                $nuna,
                i -> i == op_idx,
                i ->
                    diff_deg1_eval(tree, cX, operators.unaops[i], operators, direction)
            )
        end
    end
    deg2_branch = if nbin > OPERATOR_LIMIT_BEFORE_SLOWDOWN
        diff_deg2_eval(tree, cX, operators.binops[op_idx], operators, direction)
    else
        quote
            Base.Cartesian.@nif(
                $nbin,
                i -> i == op_idx,
                i ->
                    diff_deg2_eval(tree, cX, operators.binops[i], operators, direction)
            )
        end
    end
    quote
        result = if tree.degree == 0
            diff_deg0_eval(tree, cX, direction)
        elseif tree.degree == 1
            op_idx = tree.op
            $deg1_branch
        else
            op_idx = tree.op
            $deg2_branch
        end
        !result.ok && return result
        return ResultOk2(
            result.x, result.dx, !(is_bad_array(result.x) || is_bad_array(result.dx))
        )
    end
end

function diff_deg0_eval(
    tree::AbstractExpressionNode{T}, cX::AbstractMatrix{T}, direction::Integer
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
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op::F,
    operators::OperatorEnum,
    direction::Integer,
) where {T<:Number,F}
    result = _eval_diff_tree_array(tree.l, cX, operators, direction)
    !result.ok && return result

    # TODO - add type assertions to get better speed:
    cumulator = result.x
    dcumulator = result.dx
    diff_op = _zygote_gradient(op, Val(1))
    @inbounds @simd for j in eachindex(cumulator)
        x = op(cumulator[j])::T
        dx = diff_op(cumulator[j])::T * dcumulator[j]

        cumulator[j] = x
        dcumulator[j] = dx
    end
    return result
end

function diff_deg2_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op::F,
    operators::OperatorEnum,
    direction::Integer,
) where {T<:Number,F}
    result_l = _eval_diff_tree_array(tree.l, cX, operators, direction)
    !result_l.ok && return result_l
    result_r = _eval_diff_tree_array(tree.r, cX, operators, direction)
    !result_r.ok && return result_r

    ar_l = result_l.x
    d_ar_l = result_l.dx
    ar_r = result_r.x
    d_ar_r = result_r.dx
    diff_op = _zygote_gradient(op, Val(2))

    @inbounds @simd for j in eachindex(ar_l)
        x = op(ar_l[j], ar_r[j])::T

        first, second = diff_op(ar_l[j], ar_r[j])::Tuple{T,T}
        dx = first * d_ar_l[j] + second * d_ar_r[j]

        ar_l[j] = x
        d_ar_l[j] = dx
    end
    return result_l
end

"""
    eval_grad_tree_array(tree::AbstractExpressionNode{T}, cX::AbstractMatrix{T}, operators::OperatorEnum; variable::Bool=false, turbo::Bool=false)

Compute the forward-mode derivative of an expression, using a similar
structure and optimization to eval_tree_array. `variable` specifies whether
we should take derivatives with respect to features (i.e., cX), or with respect
to every constant in the expression.

# Arguments

- `tree::AbstractExpressionNode{T}`: The expression tree to evaluate.
- `cX::AbstractMatrix{T}`: The data matrix, with each column being a data point.
- `operators::OperatorEnum`: The operators used to create the `tree`.
- `variable::Bool`: Whether to take derivatives with respect to features (i.e., `cX` - with `variable=true`),
    or with respect to every constant in the expression (`variable=false`).
- `turbo::Bool`: Use `LoopVectorization.@turbo` for faster evaluation. Currently this does not have
    any effect.

# Returns

- `(evaluation, gradient, complete)::Tuple{AbstractVector{T}, AbstractMatrix{T}, Bool}`: the normal evaluation,
    the gradient, and whether the evaluation completed as normal (or encountered a nan or inf).
"""
function eval_grad_tree_array(
    tree::AbstractExpressionNode{T},
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
    result = if isa(variable, Val{true}) || (variable isa Bool && variable)
        eval_grad_tree_array(tree, n_gradients, nothing, cX, operators, Val(true))
    else
        index_tree = index_constants(tree)
        eval_grad_tree_array(tree, n_gradients, index_tree, cX, operators, Val(false))
    end
    return (result.x, result.dx, result.ok)
end

function eval_grad_tree_array(
    tree::AbstractExpressionNode{T},
    n_gradients,
    index_tree::Union{NodeIndex,Nothing},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    ::Val{variable},
)::ResultOk2 where {T<:Number,variable}
    result = _eval_grad_tree_array(
        tree, n_gradients, index_tree, cX, operators, Val(variable)
    )
    !result.ok && return result
    return ResultOk2(
        result.x, result.dx, !(is_bad_array(result.x) || is_bad_array(result.dx))
    )
end

function eval_grad_tree_array(
    tree::AbstractExpressionNode{T1},
    cX::AbstractMatrix{T2},
    operators::OperatorEnum;
    variable::Union{Val,Bool}=Val{false}(),
    turbo::Union{Val,Bool}=Val{false}(),
) where {T1<:Number,T2<:Number}
    T = promote_type(T1, T2)
    return eval_grad_tree_array(
        convert(constructorof(typeof(tree)){T}, tree),
        convert(AbstractMatrix{T}, cX),
        operators;
        variable=variable,
        turbo=turbo,
    )
end

@generated function _eval_grad_tree_array(
    tree::AbstractExpressionNode{T},
    n_gradients,
    index_tree::Union{NodeIndex,Nothing},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    ::Val{variable},
)::ResultOk2 where {T<:Number,variable}
    nuna = get_nuna(operators)
    nbin = get_nbin(operators)
    deg1_branch_skeleton = quote
        grad_deg1_eval(
            tree,
            n_gradients,
            index_tree,
            cX,
            operators.unaops[i],
            operators,
            Val(variable),
        )
    end
    deg2_branch_skeleton = quote
        grad_deg2_eval(
            tree,
            n_gradients,
            index_tree,
            cX,
            operators.binops[i],
            operators,
            Val(variable),
        )
    end
    deg1_branch = if nuna > OPERATOR_LIMIT_BEFORE_SLOWDOWN
        quote
            i = tree.op
            $deg1_branch_skeleton
        end
    else
        quote
            op_idx = tree.op
            Base.Cartesian.@nif($nuna, i -> i == op_idx, i -> $deg1_branch_skeleton)
        end
    end
    deg2_branch = if nbin > OPERATOR_LIMIT_BEFORE_SLOWDOWN
        quote
            i = tree.op
            $deg2_branch_skeleton
        end
    else
        quote
            op_idx = tree.op
            Base.Cartesian.@nif($nbin, i -> i == op_idx, i -> $deg2_branch_skeleton)
        end
    end
    quote
        if tree.degree == 0
            grad_deg0_eval(tree, n_gradients, index_tree, cX, Val(variable))
        elseif tree.degree == 1
            $deg1_branch
        else
            $deg2_branch
        end
    end
end

function grad_deg0_eval(
    tree::AbstractExpressionNode{T},
    n_gradients,
    index_tree::Union{NodeIndex,Nothing},
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
    end

    index = if variable
        tree.feature
    else
        (index_tree === nothing ? zero(UInt16) : index_tree.val::UInt16)
    end
    derivative_part = zero_mat
    derivative_part[index, :] .= one(T)
    return ResultOk2(const_part, derivative_part, true)
end

function grad_deg1_eval(
    tree::AbstractExpressionNode{T},
    n_gradients,
    index_tree::Union{NodeIndex,Nothing},
    cX::AbstractMatrix{T},
    op::F,
    operators::OperatorEnum,
    ::Val{variable},
)::ResultOk2 where {T<:Number,F,variable}
    result = eval_grad_tree_array(
        tree.l,
        n_gradients,
        index_tree === nothing ? index_tree : index_tree.l,
        cX,
        operators,
        Val(variable),
    )
    !result.ok && return result

    cumulator = result.x
    dcumulator = result.dx
    diff_op = _zygote_gradient(op, Val(1))
    @inbounds @simd for j in axes(dcumulator, 2)
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
    tree::AbstractExpressionNode{T},
    n_gradients,
    index_tree::Union{NodeIndex,Nothing},
    cX::AbstractMatrix{T},
    op::F,
    operators::OperatorEnum,
    ::Val{variable},
)::ResultOk2 where {T<:Number,F,variable}
    result_l = eval_grad_tree_array(
        tree.l,
        n_gradients,
        index_tree === nothing ? index_tree : index_tree.l,
        cX,
        operators,
        Val(variable),
    )
    !result_l.ok && return result_l
    result_r = eval_grad_tree_array(
        tree.r,
        n_gradients,
        index_tree === nothing ? index_tree : index_tree.r,
        cX,
        operators,
        Val(variable),
    )
    !result_r.ok && return result_r

    cumulator_l = result_l.x
    dcumulator_l = result_l.dx
    cumulator_r = result_r.x
    dcumulator_r = result_r.dx
    diff_op = _zygote_gradient(op, Val(2))
    @inbounds @simd for j in axes(dcumulator_l, 2)
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
