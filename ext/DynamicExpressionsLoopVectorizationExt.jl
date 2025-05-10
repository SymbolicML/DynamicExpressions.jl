module DynamicExpressionsLoopVectorizationExt

using LoopVectorization: @turbo
using DynamicExpressions: AbstractExpressionNode
using DynamicExpressions.UtilsModule: ResultOk
using DynamicExpressions.EvaluateModule:
    @return_on_nonfinite_val, EvalOptions, get_array, get_feature_array, get_filled_array
import DynamicExpressions.EvaluateModule:
    deg1_eval,
    deg2_eval,
    degn_eval,
    deg1_l2_ll0_lr0_eval,
    deg1_l1_ll0_eval,
    deg2_l0_r0_eval,
    deg2_l0_eval,
    deg2_r0_eval
import DynamicExpressions.ExtensionInterfaceModule:
    _is_loopvectorization_loaded, bumper_kern1!, bumper_kern2!

_is_loopvectorization_loaded(::Int) = true

@generated function degn_eval(
    cumulators::NTuple{N,<:AbstractVector{T}}, op::F, ::EvalOptions{true}
)::ResultOk where {N,T,F}
    # Fast general implementation of `cumulators[1] .= op.(cumulators[1], cumulators[2], ...)`
    quote
        Base.Cartesian.@nexprs($N, i -> cumulator_i = cumulators[i])
        @turbo for j in eachindex(cumulator_1)
            cumulator_1[j] = Base.Cartesian.@ncall($N, op, i -> cumulator_i[j])
        end
        return ResultOk(cumulator_1, true)
    end
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

end
