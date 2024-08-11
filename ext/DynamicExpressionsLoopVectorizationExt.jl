module DynamicExpressionsLoopVectorizationExt

using DynamicExpressions

using LoopVectorization: @turbo, vmapnt
using DynamicExpressions: AbstractExpressionNode, GraphNode, OperatorEnum
using DynamicExpressions.UtilsModule: ResultOk, fill_similar
using DynamicExpressions.EvaluateModule: @return_on_check
import DynamicExpressions.EvaluateModule:
    deg1_eval,
    deg2_eval,
    deg1_l2_ll0_lr0_eval,
    deg1_l1_ll0_eval,
    deg2_l0_r0_eval,
    deg2_l0_eval,
    deg2_r0_eval
import DynamicExpressions.ExtensionInterfaceModule:
    _is_loopvectorization_loaded, bumper_kern1!, bumper_kern2!
import DynamicExpressions.ValueInterfaceModule: is_valid, is_valid_array

_is_loopvectorization_loaded(::Int) = true

function deg2_eval(
    cumulator_l::AbstractVector{T}, cumulator_r::AbstractVector{T}, op::F, ::Val{true}
)::ResultOk where {T<:Number,F}
    @turbo for j in eachindex(cumulator_l)
        x = op(cumulator_l[j], cumulator_r[j])
        cumulator_l[j] = x
    end
    return ResultOk(cumulator_l, true)
end

function deg1_eval(
    cumulator::AbstractVector{T}, op::F, ::Val{true}
)::ResultOk where {T<:Number,F}
    @turbo for j in eachindex(cumulator)
        x = op(cumulator[j])
        cumulator[j] = x
    end
    return ResultOk(cumulator, true)
end

function deg1_l2_ll0_lr0_eval(
    tree::AbstractExpressionNode{T}, cX::AbstractMatrix{T}, op::F, op_l::F2, ::Val{true}
) where {T<:Number,F,F2}
    if tree.l.l.constant && tree.l.r.constant
        val_ll = tree.l.l.val
        val_lr = tree.l.r.val
        @return_on_check val_ll cX
        @return_on_check val_lr cX
        x_l = op_l(val_ll, val_lr)::T
        @return_on_check x_l cX
        x = op(x_l)::T
        @return_on_check x cX
        return ResultOk(fill_similar(x, cX, axes(cX, 2)), true)
    elseif tree.l.l.constant
        val_ll = tree.l.l.val
        @return_on_check val_ll cX
        feature_lr = tree.l.r.feature
        cumulator = similar(cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_l = op_l(val_ll, cX[feature_lr, j])
            x = op(x_l)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.l.r.constant
        feature_ll = tree.l.l.feature
        val_lr = tree.l.r.val
        @return_on_check val_lr cX
        cumulator = similar(cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_l = op_l(cX[feature_ll, j], val_lr)
            x = op(x_l)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        feature_ll = tree.l.l.feature
        feature_lr = tree.l.r.feature
        cumulator = similar(cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_l = op_l(cX[feature_ll, j], cX[feature_lr, j])
            x = op(x_l)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    end
end

function deg1_l1_ll0_eval(
    tree::AbstractExpressionNode{T}, cX::AbstractMatrix{T}, op::F, op_l::F2, ::Val{true}
) where {T<:Number,F,F2}
    if tree.l.l.constant
        val_ll = tree.l.l.val
        @return_on_check val_ll cX
        x_l = op_l(val_ll)::T
        @return_on_check x_l cX
        x = op(x_l)::T
        @return_on_check x cX
        return ResultOk(fill_similar(x, cX, axes(cX, 2)), true)
    else
        feature_ll = tree.l.l.feature
        cumulator = similar(cX, axes(cX, 2))
        @turbo for j in axes(cX, 2)
            x_l = op_l(cX[feature_ll, j])
            x = op(x_l)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    end
end

function deg2_l0_r0_eval(
    tree::AbstractExpressionNode{T}, cX::AbstractMatrix{T}, op::F, ::Val{true}
) where {T<:Number,F}
    if tree.l.constant && tree.r.constant
        val_l = tree.l.val
        @return_on_check val_l cX
        val_r = tree.r.val
        @return_on_check val_r cX
        x = op(val_l, val_r)::T
        @return_on_check x cX
        return ResultOk(fill_similar(x, cX, axes(cX, 2)), true)
    elseif tree.l.constant
        cumulator = similar(cX, axes(cX, 2))
        val_l = tree.l.val
        @return_on_check val_l cX
        feature_r = tree.r.feature
        @turbo for j in axes(cX, 2)
            x = op(val_l, cX[feature_r, j])
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.r.constant
        cumulator = similar(cX, axes(cX, 2))
        feature_l = tree.l.feature
        val_r = tree.r.val
        @return_on_check val_r cX
        @turbo for j in axes(cX, 2)
            x = op(cX[feature_l, j], val_r)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        cumulator = similar(cX, axes(cX, 2))
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
    ::Val{true},
) where {T<:Number,F}
    if tree.l.constant
        val = tree.l.val
        @return_on_check val cX
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
    ::Val{true},
) where {T<:Number,F}
    if tree.r.constant
        val = tree.r.val
        @return_on_check val cX
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
function bumper_kern1!(op::F, cumulator, ::Val{true}) where {F}
    @turbo @. cumulator = op(cumulator)
    return cumulator
end
function bumper_kern2!(op::F, cumulator1, cumulator2, ::Val{true}) where {F}
    @turbo @. cumulator1 = op(cumulator1, cumulator2)
    return cumulator1
end



# graph eval

function DynamicExpressions.EvaluateModule._eval_graph_array(
    root::GraphNode{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    loopVectorization::Val{true}
) where {T}

    # vmap is faster with small cX sizes
    # vmapnt (non-temporal) is faster with larger cX sizes (too big so not worth caching?)

    order = topological_sort(root)
    for node in order
        if node.degree == 0 && !node.constant
            node.cache = view(cX, node.feature, :)
        elseif node.degree == 1
            if node.l.constant
                node.constant = true
                node.val = operators.unaops[node.op](node.l.val)
                if !is_valid(node.val) return ResultOk(Vector{T}(undef, size(cX, 2)), false) end
            else
                node.constant = false
                node.cache = vmapnt(operators.unaops[node.op], node.l.cache)
                if !is_valid_array(node.cache) return ResultOk(node.cache, false) end
            end
        elseif node.degree == 2
            if node.l.constant
                if node.r.constant
                    node.constant = true
                    node.val = operators.binops[node.op](node.l.val, node.r.val)
                    if !is_valid(node.val) return ResultOk(Vector{T}(undef, size(cX, 2)), false) end
                else
                    node.constant = false
                    node.cache = vmapnt(Base.Fix1(operators.binops[node.op], node.l.val), node.r.cache)
                    if !is_valid_array(node.cache) return ResultOk(node.cache, false) end
                end
            else
                if node.r.constant
                    node.constant = false
                    node.cache = vmapnt(Base.Fix2(operators.binops[node.op], node.r.val), node.l.cache)
                    if !is_valid_array(node.cache) return ResultOk(node.cache, false) end
                else
                    node.constant = false
                    node.cache = vmapnt(operators.binops[node.op], node.l.cache, node.r.cache)
                    if !is_valid_array(node.cache) return ResultOk(node.cache, false) end
                end
            end
        end
    end
    if root.constant
        return ResultOk(fill(root.val, size(cX, 2)), true)
    else
        return ResultOk(root.cache, true)
    end
end

end
