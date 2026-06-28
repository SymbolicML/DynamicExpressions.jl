module DynamicExpressionsLoopVectorizationExt

using LoopVectorization: @turbo
using DynamicExpressions: AbstractExpressionNode
using DynamicExpressions.NodeModule: get_child
using DynamicExpressions.UtilsModule: ResultOk
using DynamicExpressions.ValueInterfaceModule: turbo_can_eval_nonfinite
using DynamicExpressions.EvaluateModule:
    @return_on_nonfinite_val, EvalContext, get_array, get_feature_array, get_filled_array
import DynamicExpressions.EvaluateModule:
    deg1_eval,
    deg2_eval,
    degn_eval,
    deg1_l2_ll0_lr0_eval,
    deg1_l1_ll0_eval,
    deg2_branch0_eval,
    deg2_l0_r0_eval,
    deg2_l0_eval,
    deg2_r0_eval
import DynamicExpressions.ExtensionInterfaceModule:
    _is_loopvectorization_loaded, bumper_kern!

_is_loopvectorization_loaded(::Int) = true

@generated function degn_eval(
    cumulators::NTuple{N,<:AbstractVector{T}}, op::F, ::EvalContext{true}
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
    eval_context::EvalContext{true},
) where {T<:Number,F,F2}
    if get_child(get_child(tree, 1), 1).constant &&
        get_child(get_child(tree, 1), 2).constant
        val_ll = get_child(get_child(tree, 1), 1).val
        val_lr = get_child(get_child(tree, 1), 2).val
        @return_on_nonfinite_val(eval_context, val_ll, cX)
        @return_on_nonfinite_val(eval_context, val_lr, cX)
        x_l = op_l(val_ll, val_lr)::T
        @return_on_nonfinite_val(eval_context, x_l, cX)
        x = op(x_l)::T
        @return_on_nonfinite_val(eval_context, x, cX)
        return ResultOk(get_filled_array(eval_context.buffer, x, cX, axes(cX, 2)), true)
    elseif get_child(get_child(tree, 1), 1).constant
        val_ll = get_child(get_child(tree, 1), 1).val
        @return_on_nonfinite_val(eval_context, val_ll, cX)
        feature_lr = get_child(get_child(tree, 1), 2).feature
        cumulator = get_array(eval_context.buffer, cX, axes(cX, 2))
        if turbo_can_eval_nonfinite(op)
            @turbo for j in axes(cX, 2)
                x_l = op_l(val_ll, cX[feature_lr, j])
                cumulator[j] = op(x_l)
            end
        else
            @turbo for j in axes(cX, 2)
                x_l = op_l(val_ll, cX[feature_lr, j])
                cumulator[j] = ifelse(isfinite(x_l), op(x_l), T(NaN))
            end
        end
        return ResultOk(cumulator, true)
    elseif get_child(get_child(tree, 1), 2).constant
        feature_ll = get_child(get_child(tree, 1), 1).feature
        val_lr = get_child(get_child(tree, 1), 2).val
        @return_on_nonfinite_val(eval_context, val_lr, cX)
        cumulator = get_array(eval_context.buffer, cX, axes(cX, 2))
        if turbo_can_eval_nonfinite(op)
            @turbo for j in axes(cX, 2)
                x_l = op_l(cX[feature_ll, j], val_lr)
                cumulator[j] = op(x_l)
            end
        else
            @turbo for j in axes(cX, 2)
                x_l = op_l(cX[feature_ll, j], val_lr)
                cumulator[j] = ifelse(isfinite(x_l), op(x_l), T(NaN))
            end
        end
        return ResultOk(cumulator, true)
    else
        feature_ll = get_child(get_child(tree, 1), 1).feature
        feature_lr = get_child(get_child(tree, 1), 2).feature
        cumulator = get_array(eval_context.buffer, cX, axes(cX, 2))
        if turbo_can_eval_nonfinite(op)
            @turbo for j in axes(cX, 2)
                x_l = op_l(cX[feature_ll, j], cX[feature_lr, j])
                cumulator[j] = op(x_l)
            end
        else
            @turbo for j in axes(cX, 2)
                x_l = op_l(cX[feature_ll, j], cX[feature_lr, j])
                cumulator[j] = ifelse(isfinite(x_l), op(x_l), T(NaN))
            end
        end
        return ResultOk(cumulator, true)
    end
end

function deg1_l1_ll0_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op::F,
    op_l::F2,
    eval_context::EvalContext{true},
) where {T<:Number,F,F2}
    if get_child(get_child(tree, 1), 1).constant
        val_ll = get_child(get_child(tree, 1), 1).val
        @return_on_nonfinite_val(eval_context, val_ll, cX)
        x_l = op_l(val_ll)::T
        @return_on_nonfinite_val(eval_context, x_l, cX)
        x = op(x_l)::T
        @return_on_nonfinite_val(eval_context, x, cX)
        return ResultOk(get_filled_array(eval_context.buffer, x, cX, axes(cX, 2)), true)
    else
        feature_ll = get_child(get_child(tree, 1), 1).feature
        cumulator = get_array(eval_context.buffer, cX, axes(cX, 2))
        if turbo_can_eval_nonfinite(op)
            @turbo for j in axes(cX, 2)
                x_l = op_l(cX[feature_ll, j])
                cumulator[j] = op(x_l)
            end
        else
            @turbo for j in axes(cX, 2)
                x_l = op_l(cX[feature_ll, j])
                cumulator[j] = ifelse(isfinite(x_l), op(x_l), T(NaN))
            end
        end
        return ResultOk(cumulator, true)
    end
end

function deg2_branch0_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op::F,
    branch_op::F2,
    ::Val{side},
    eval_context::EvalContext{true,false},
) where {T<:Number,F,F2,side}
    branch = side === :left ? get_child(tree, 1) : get_child(tree, 2)
    leaf1 = side === :left ? get_child(branch, 1) : get_child(tree, 1)
    leaf2 = side === :left ? get_child(branch, 2) : get_child(branch, 1)
    leaf3 = side === :left ? get_child(tree, 2) : get_child(branch, 2)

    if eval_context.early_exit isa Val{true} && leaf1.constant && !isfinite(leaf1.val)
        return ResultOk(get_array(eval_context.buffer, cX, axes(cX, 2)), false)
    elseif eval_context.early_exit isa Val{true} && leaf2.constant && !isfinite(leaf2.val)
        return ResultOk(get_array(eval_context.buffer, cX, axes(cX, 2)), false)
    elseif eval_context.early_exit isa Val{true} && leaf3.constant && !isfinite(leaf3.val)
        return ResultOk(get_array(eval_context.buffer, cX, axes(cX, 2)), false)
    end

    constant1 = leaf1.constant
    constant2 = leaf2.constant
    constant3 = leaf3.constant
    feature1 = constant1 ? 1 : Int(leaf1.feature)
    feature2 = constant2 ? 1 : Int(leaf2.feature)
    feature3 = constant3 ? 1 : Int(leaf3.feature)
    value1 = constant1 ? leaf1.val : zero(T)
    value2 = constant2 ? leaf2.val : zero(T)
    value3 = constant3 ? leaf3.val : zero(T)
    cumulator = get_array(eval_context.buffer, cX, axes(cX, 2))

    if side === :left && eval_context.early_exit isa Val{true}
        @inbounds @simd for j in axes(cX, 2)
            x1 = ifelse(constant1, value1, cX[feature1, j])
            x2 = ifelse(constant2, value2, cX[feature2, j])
            x3 = ifelse(constant3, value3, cX[feature3, j])
            branch_x = branch_op(x1, x2)
            cumulator[j] = isfinite(branch_x) & isfinite(x3) ? op(branch_x, x3) : T(Inf)
        end  # COV_EXCL_LINE
    elseif side === :right && eval_context.early_exit isa Val{true}
        @inbounds @simd for j in axes(cX, 2)
            x1 = ifelse(constant1, value1, cX[feature1, j])
            x2 = ifelse(constant2, value2, cX[feature2, j])
            x3 = ifelse(constant3, value3, cX[feature3, j])
            branch_x = branch_op(x2, x3)
            cumulator[j] = isfinite(x1) & isfinite(branch_x) ? op(x1, branch_x) : T(Inf)
        end  # COV_EXCL_LINE
    elseif side === :left
        @turbo for j in axes(cX, 2)
            x1 = ifelse(constant1, value1, cX[feature1, j])
            x2 = ifelse(constant2, value2, cX[feature2, j])
            x3 = ifelse(constant3, value3, cX[feature3, j])
            cumulator[j] = op(branch_op(x1, x2), x3)
        end
    else
        @turbo for j in axes(cX, 2)
            x1 = ifelse(constant1, value1, cX[feature1, j])
            x2 = ifelse(constant2, value2, cX[feature2, j])
            x3 = ifelse(constant3, value3, cX[feature3, j])
            cumulator[j] = op(x1, branch_op(x2, x3))
        end
    end
    return ResultOk(cumulator, true)
end

function deg2_l0_r0_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op::F,
    eval_context::EvalContext{true},
) where {T<:Number,F}
    if get_child(tree, 1).constant && get_child(tree, 2).constant
        val_l = get_child(tree, 1).val
        @return_on_nonfinite_val(eval_context, val_l, cX)
        val_r = get_child(tree, 2).val
        @return_on_nonfinite_val(eval_context, val_r, cX)
        x = op(val_l, val_r)::T
        @return_on_nonfinite_val(eval_context, x, cX)
        return ResultOk(get_filled_array(eval_context.buffer, x, cX, axes(cX, 2)), true)
    elseif get_child(tree, 1).constant
        cumulator = get_array(eval_context.buffer, cX, axes(cX, 2))
        val_l = get_child(tree, 1).val
        @return_on_nonfinite_val(eval_context, val_l, cX)
        feature_r = get_child(tree, 2).feature
        @turbo for j in axes(cX, 2)
            x = op(val_l, cX[feature_r, j])
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif get_child(tree, 2).constant
        cumulator = get_array(eval_context.buffer, cX, axes(cX, 2))
        feature_l = get_child(tree, 1).feature
        val_r = get_child(tree, 2).val
        @return_on_nonfinite_val(eval_context, val_r, cX)
        @turbo for j in axes(cX, 2)
            x = op(cX[feature_l, j], val_r)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        cumulator = get_array(eval_context.buffer, cX, axes(cX, 2))
        feature_l = get_child(tree, 1).feature
        feature_r = get_child(tree, 2).feature
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
    eval_context::EvalContext{true},
) where {T<:Number,F}
    if get_child(tree, 1).constant
        val = get_child(tree, 1).val
        @return_on_nonfinite_val(eval_context, val, cX)
        @turbo for j in eachindex(cumulator)
            x = op(val, cumulator[j])
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        feature = get_child(tree, 1).feature
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
    eval_context::EvalContext{true},
) where {T<:Number,F}
    if get_child(tree, 2).constant
        val = get_child(tree, 2).val
        @return_on_nonfinite_val(eval_context, val, cX)
        @turbo for j in eachindex(cumulator)
            x = op(cumulator[j], val)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        feature = get_child(tree, 2).feature
        @turbo for j in eachindex(cumulator)
            x = op(cumulator[j], cX[feature, j])
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    end
end

# Interface with Bumper.jl
function bumper_kern!(
    op::F, cumulators::Tuple{Vararg{Any,degree}}, ::EvalContext{true,true,early_exit}
) where {F,degree,early_exit}
    cumulator_1 = first(cumulators)
    @turbo @. cumulator_1 = op(cumulators...)
    return cumulator_1
end

end
