module DynamicExpressionsLoopVectorizationExt

using LoopVectorization: @turbo
using DynamicExpressions: AbstractExpressionNode
using DynamicExpressions.UtilsModule: ResultOk
using DynamicExpressions.EvaluateModule:
    @return_on_nonfinite_val, EvalOptions, get_array, get_feature_array, get_filled_array
import DynamicExpressions.EvaluateModule:
    deg1_eval,
    deg2_eval,
    deg1_l2_ll0_lr0_eval,
    deg1_l1_ll0_eval,
    deg2_l0_r0_eval,
    deg2_l0_eval,
    deg2_r0_eval,
    deg2_l2_ll0_lr0_r0_eval,
    deg2_l0_r2_rl0_rr0_eval
import DynamicExpressions.ExtensionInterfaceModule:
    _is_loopvectorization_loaded, bumper_kern1!, bumper_kern2!

_is_loopvectorization_loaded(::Int) = true

function deg2_eval(
    cumulator_l::AbstractVector{T},
    cumulator_r::AbstractVector{T},
    op::F,
    ::EvalOptions{true},
)::ResultOk where {T<:Number,F}
    @turbo for j in eachindex(cumulator_l)
        x = op(cumulator_l[j], cumulator_r[j])
        cumulator_l[j] = x
    end
    return ResultOk(cumulator_l, true)
end

function deg1_eval(
    cumulator::AbstractVector{T}, op::F, ::EvalOptions{true}
)::ResultOk where {T<:Number,F}
    @turbo for j in eachindex(cumulator)
        x = op(cumulator[j])
        cumulator[j] = x
    end
    return ResultOk(cumulator, true)
end

function deg1_l2_ll0_lr0_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op::F,
    op_l::F2,
    eval_options::EvalOptions{true},
) where {T<:Number,F,F2}
    if tree.l.l.constant && tree.l.r.constant
        val_ll = tree.l.l.val
        val_lr = tree.l.r.val
        @return_on_nonfinite_val(eval_options, val_ll, cX)
        @return_on_nonfinite_val(eval_options, val_lr, cX)
        x_l = op_l(val_ll, val_lr)::T
        @return_on_nonfinite_val(eval_options, x_l, cX)
        x = op(x_l)::T
        @return_on_nonfinite_val(eval_options, x, cX)
        return ResultOk(get_filled_array(eval_options.buffer, x, cX, axes(cX, 2)), true)
    elseif tree.l.l.constant
        val_ll = tree.l.l.val
        @return_on_nonfinite_val(eval_options, val_ll, cX)
        feature_lr = tree.l.r.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_l = op_l(val_ll, cX[feature_lr, j])
            x = op(x_l)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.l.r.constant
        feature_ll = tree.l.l.feature
        val_lr = tree.l.r.val
        @return_on_nonfinite_val(eval_options, val_lr, cX)
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_l = op_l(cX[feature_ll, j], val_lr)
            x = op(x_l)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        feature_ll = tree.l.l.feature
        feature_lr = tree.l.r.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_l = op_l(cX[feature_ll, j], cX[feature_lr, j])
            x = op(x_l)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    end
end

function deg1_l1_ll0_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op::F,
    op_l::F2,
    eval_options::EvalOptions{true},
) where {T<:Number,F,F2}
    if tree.l.l.constant
        val_ll = tree.l.l.val
        @return_on_nonfinite_val(eval_options, val_ll, cX)
        x_l = op_l(val_ll)::T
        @return_on_nonfinite_val(eval_options, x_l, cX)
        x = op(x_l)::T
        @return_on_nonfinite_val(eval_options, x, cX)
        return ResultOk(get_filled_array(eval_options.buffer, x, cX, axes(cX, 2)), true)
    else
        feature_ll = tree.l.l.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_l = op_l(cX[feature_ll, j])
            x = op(x_l)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    end
end

function deg2_l0_r0_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op::F,
    eval_options::EvalOptions{true},
) where {T<:Number,F}
    if tree.l.constant && tree.r.constant
        val_l = tree.l.val
        @return_on_nonfinite_val(eval_options, val_l, cX)
        val_r = tree.r.val
        @return_on_nonfinite_val(eval_options, val_r, cX)
        x = op(val_l, val_r)::T
        @return_on_nonfinite_val(eval_options, x, cX)
        return ResultOk(get_filled_array(eval_options.buffer, x, cX, axes(cX, 2)), true)
    elseif tree.l.constant
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        val_l = tree.l.val
        @return_on_nonfinite_val(eval_options, val_l, cX)
        feature_r = tree.r.feature
        @turbo for j in axes(cX, 2)
            x = op(val_l, cX[feature_r, j])
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.r.constant
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        feature_l = tree.l.feature
        val_r = tree.r.val
        @return_on_nonfinite_val(eval_options, val_r, cX)
        @turbo for j in axes(cX, 2)
            x = op(cX[feature_l, j], val_r)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        feature_l = tree.l.feature
        feature_r = tree.r.feature
        @turbo for j in axes(cX, 2)
            x = op(cX[feature_l, j], cX[feature_r, j])
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    end
end

# op(x, y) for x variable/constant, y arbitrary
function deg2_l0_eval(
    tree::AbstractExpressionNode{T},
    cumulator::AbstractVector{T},
    cX::AbstractArray{T},
    op::F,
    eval_options::EvalOptions{true},
) where {T<:Number,F}
    if tree.l.constant
        val = tree.l.val
        @return_on_nonfinite_val(eval_options, val, cX)
        @turbo for j in eachindex(cumulator)
            x = op(val, cumulator[j])
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        feature = tree.l.feature
        @turbo for j in eachindex(cumulator)
            x = op(cX[feature, j], cumulator[j])
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    end
end

function deg2_r0_eval(
    tree::AbstractExpressionNode{T},
    cumulator::AbstractVector{T},
    cX::AbstractArray{T},
    op::F,
    eval_options::EvalOptions{true},
) where {T<:Number,F}
    if tree.r.constant
        val = tree.r.val
        @return_on_nonfinite_val(eval_options, val, cX)
        @turbo for j in eachindex(cumulator)
            x = op(cumulator[j], val)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        feature = tree.r.feature
        @turbo for j in eachindex(cumulator)
            x = op(cumulator[j], cX[feature, j])
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    end
end

## Interface with Bumper.jl
function bumper_kern1!(
    op::F, cumulator, ::EvalOptions{true,true,early_exit}
) where {F,early_exit}
    @turbo @. cumulator = op(cumulator)
    return cumulator
end
function bumper_kern2!(
    op::F, cumulator1, cumulator2, ::EvalOptions{true,true,early_exit}
) where {F,early_exit}
    @turbo @. cumulator1 = op(cumulator1, cumulator2)
    return cumulator1
end

function deg2_l2_ll0_lr0_r0_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op::F,
    op_l::F2,
    eval_options::EvalOptions{true},
) where {T<:Number,F,F2}
    if tree.l.l.constant && tree.l.r.constant && tree.r.constant
        val_ll = tree.l.l.val
        val_lr = tree.l.r.val
        val_r = tree.r.val
        @return_on_nonfinite_val(eval_options, val_ll, cX)
        @return_on_nonfinite_val(eval_options, val_lr, cX)
        @return_on_nonfinite_val(eval_options, val_r, cX)
        x_l = op_l(val_ll, val_lr)::T
        @return_on_nonfinite_val(eval_options, x_l, cX)
        x = op(x_l, val_r)::T
        @return_on_nonfinite_val(eval_options, x, cX)
        return ResultOk(get_filled_array(eval_options.buffer, x, cX, axes(cX, 2)), true)
    elseif tree.l.l.constant && tree.l.r.constant
        val_ll = tree.l.l.val
        val_lr = tree.l.r.val
        @return_on_nonfinite_val(eval_options, val_ll, cX)
        @return_on_nonfinite_val(eval_options, val_lr, cX)
        feature_r = tree.r.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        x_l = op_l(val_ll, val_lr)::T
        @turbo for j in axes(cX, 2)
            x = op(x_l, cX[feature_r, j])
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.l.l.constant && tree.r.constant
        val_ll = tree.l.l.val
        val_r = tree.r.val
        @return_on_nonfinite_val(eval_options, val_ll, cX)
        @return_on_nonfinite_val(eval_options, val_r, cX)
        feature_lr = tree.l.r.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_l = op_l(val_ll, cX[feature_lr, j])
            x = op(x_l, val_r)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.l.r.constant && tree.r.constant
        val_lr = tree.l.r.val
        val_r = tree.r.val
        @return_on_nonfinite_val(eval_options, val_lr, cX)
        @return_on_nonfinite_val(eval_options, val_r, cX)
        feature_ll = tree.l.l.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_l = op_l(cX[feature_ll, j], val_lr)
            x = op(x_l, val_r)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.l.l.constant
        val_ll = tree.l.l.val
        @return_on_nonfinite_val(eval_options, val_ll, cX)
        feature_lr = tree.l.r.feature
        feature_r = tree.r.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_l = op_l(val_ll, cX[feature_lr, j])
            x = op(x_l, cX[feature_r, j])
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.l.r.constant
        val_lr = tree.l.r.val
        @return_on_nonfinite_val(eval_options, val_lr, cX)
        feature_ll = tree.l.l.feature
        feature_r = tree.r.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_l = op_l(cX[feature_ll, j], val_lr)
            x = op(x_l, cX[feature_r, j])
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.r.constant
        val_r = tree.r.val
        @return_on_nonfinite_val(eval_options, val_r, cX)
        feature_ll = tree.l.l.feature
        feature_lr = tree.l.r.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_l = op_l(cX[feature_ll, j], cX[feature_lr, j])
            x = op(x_l, val_r)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        feature_ll = tree.l.l.feature
        feature_lr = tree.l.r.feature
        feature_r = tree.r.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_l = op_l(cX[feature_ll, j], cX[feature_lr, j])
            x = op(x_l, cX[feature_r, j])
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    end
end

function deg2_l0_r2_rl0_rr0_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op::F,
    op_r::F2,
    eval_options::EvalOptions{true},
) where {T<:Number,F,F2}
    if tree.l.constant && tree.r.l.constant && tree.r.r.constant
        val_l = tree.l.val
        val_rl = tree.r.l.val
        val_rr = tree.r.r.val
        @return_on_nonfinite_val(eval_options, val_l, cX)
        @return_on_nonfinite_val(eval_options, val_rl, cX)
        @return_on_nonfinite_val(eval_options, val_rr, cX)
        x_r = op_r(val_rl, val_rr)::T
        @return_on_nonfinite_val(eval_options, x_r, cX)
        x = op(val_l, x_r)::T
        @return_on_nonfinite_val(eval_options, x, cX)
        return ResultOk(get_filled_array(eval_options.buffer, x, cX, axes(cX, 2)), true)
    elseif tree.r.l.constant && tree.r.r.constant
        val_rl = tree.r.l.val
        val_rr = tree.r.r.val
        @return_on_nonfinite_val(eval_options, val_rl, cX)
        @return_on_nonfinite_val(eval_options, val_rr, cX)
        feature_l = tree.l.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        x_r = op_r(val_rl, val_rr)::T
        @turbo for j in axes(cX, 2)
            x = op(cX[feature_l, j], x_r)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.l.constant && tree.r.r.constant
        val_l = tree.l.val
        val_rr = tree.r.r.val
        @return_on_nonfinite_val(eval_options, val_l, cX)
        @return_on_nonfinite_val(eval_options, val_rr, cX)
        feature_rl = tree.r.l.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_r = op_r(cX[feature_rl, j], val_rr)
            x = op(val_l, x_r)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.l.constant && tree.r.l.constant
        val_l = tree.l.val
        val_rl = tree.r.l.val
        @return_on_nonfinite_val(eval_options, val_l, cX)
        @return_on_nonfinite_val(eval_options, val_rl, cX)
        feature_rr = tree.r.r.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_r = op_r(val_rl, cX[feature_rr, j])
            x = op(val_l, x_r)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.l.constant
        val_l = tree.l.val
        @return_on_nonfinite_val(eval_options, val_l, cX)
        feature_rl = tree.r.l.feature
        feature_rr = tree.r.r.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_r = op_r(cX[feature_rl, j], cX[feature_rr, j])
            x = op(val_l, x_r)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.r.l.constant
        val_rl = tree.r.l.val
        @return_on_nonfinite_val(eval_options, val_rl, cX)
        feature_l = tree.l.feature
        feature_rr = tree.r.r.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_r = op_r(val_rl, cX[feature_rr, j])
            x = op(cX[feature_l, j], x_r)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.r.r.constant
        val_rr = tree.r.r.val
        @return_on_nonfinite_val(eval_options, val_rr, cX)
        feature_l = tree.l.feature
        feature_rl = tree.r.l.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_r = op_r(cX[feature_rl, j], val_rr)
            x = op(cX[feature_l, j], x_r)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        feature_l = tree.l.feature
        feature_rl = tree.r.l.feature
        feature_rr = tree.r.r.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_r = op_r(cX[feature_rl, j], cX[feature_rr, j])
            x = op(cX[feature_l, j], x_r)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    end
end

end
