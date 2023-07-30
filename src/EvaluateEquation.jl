module EvaluateEquationModule

import LoopVectorization: @turbo, indices
import ..EquationModule: Node, string_tree
import ..TypedEquationsModule: _eval_tree_array_typed
import ..OperatorEnumModule: OperatorEnum, GenericOperatorEnum
import ..UtilsModule: @return_on_false, @maybe_turbo, is_bad_array, fill_similar
import ..EquationUtilsModule: is_constant, count_nodes_under_limit

macro return_on_check(val, X)
    :(
        if !isfinite($(esc(val)))
            return (similar($(esc(X)), axes($(esc(X)), 2)), false)
        end
    )
end

macro return_on_nonfinite_array(array)
    :(
        if is_bad_array($(esc(array)))
            return ($(esc(array)), false)
        end
    )
end

"""
    eval_tree_array(tree::Node, cX::AbstractMatrix{T}, operators::OperatorEnum; turbo::Bool=false)

Evaluate a binary tree (equation) over a given input data matrix. The
operators contain all of the operators used. This function fuses doublets
and triplets of operations for lower memory usage.

This function can be represented by the following pseudocode:

```
function eval(current_node)
    if current_node is leaf
        return current_node.value
    elif current_node is degree 1
        return current_node.operator(eval(current_node.left_child))
    else
        return current_node.operator(eval(current_node.left_child), eval(current_node.right_child))
```
The bulk of the code is for optimizations and pre-emptive NaN/Inf checks,
which speed up evaluation significantly.

# Arguments
- `tree::Node`: The root node of the tree to evaluate.
- `cX::AbstractMatrix{T}`: The input data to evaluate the tree on.
- `operators::OperatorEnum`: The operators used in the tree.
- `turbo::Bool`: Use `LoopVectorization.@turbo` for faster evaluation.

# Returns
- `(output, complete)::Tuple{AbstractVector{T}, Bool}`: the result,
    which is a 1D array, as well as if the evaluation completed
    successfully (true/false). A `false` complete means an infinity
    or nan was encountered, and a large loss should be assigned
    to the equation.
"""
function eval_tree_array(
    tree::Node{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum;
    turbo::Bool=false,
    specialize_kernels::Bool=false,
    specialization_depth::Int=7,
)::Tuple{AbstractVector{T},Bool} where {T<:Number}
    if turbo
        @assert T in (Float32, Float64)
    end
    result, finished = _eval_tree_array(
        tree,
        cX,
        operators,
        (turbo ? Val(true) : Val(false)),
        (specialize_kernels ? Val(true) : Val(false)),
        (specialize_kernels ? Val(specialization_depth) : Val(0)),
    )
    @return_on_false finished result
    @return_on_nonfinite_array result
    return result, finished
end
function eval_tree_array(
    tree::Node{T1},
    cX::AbstractMatrix{T2},
    operators::OperatorEnum;
    turbo::Bool=false,
    specialize_kernels::Bool=false,
    specialization_depth::Int=7,
) where {T1<:Number,T2<:Number}
    T = promote_type(T1, T2)
    @warn "Warning: eval_tree_array received mixed types: tree=$(T1) and data=$(T2)."
    tree = convert(Node{T}, tree)
    cX = T.(cX)
    return eval_tree_array(
        tree, cX, operators; turbo, specialize_kernels, specialization_depth
    )
end

function _eval_tree_array(
    tree::Node{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    ::Val{turbo},
    ::Val{specialize_kernels},
    ::Val{specialization_depth},
) where {T<:Number,turbo,specialize_kernels,specialization_depth}
    # First, we see if there are only constants in the tree - meaning
    # we can just return the constant result.
    if tree.degree == 0
        return deg0_eval(tree, cX)
    elseif is_constant(tree)
        # Speed hack for constant trees.
        result, flag = _eval_constant_tree(tree, operators)
        !flag && return similar(cX, axes(cX, 2)), false
        return fill_similar(result, cX, axes(cX, 2)), true
    elseif specialize_kernels && count_nodes_under_limit(tree, specialization_depth)
        # Speed hack with fully-specialized kernels
        return _eval_tree_array_typed(tree, cX, operators; turbo), true
    elseif tree.degree == 1
        op = operators.unaops[tree.op]
        # op(x), for any x.
        (cumulator, complete) = _eval_tree_array(
            tree.l,
            cX,
            operators,
            Val(turbo),
            Val(specialize_kernels),
            Val(specialization_depth),
        )
        @return_on_false complete cumulator
        @return_on_nonfinite_array cumulator
        return deg1_eval(cumulator, op, Val(turbo))

    elseif tree.degree == 2
        op = operators.binops[tree.op]
        # TODO - add op(op2(x, y), z) and op(x, op2(y, z))
        # op(x, y), where x, y are constants or variables.
        (cumulator_l, complete) = _eval_tree_array(
            tree.l,
            cX,
            operators,
            Val(turbo),
            Val(specialize_kernels),
            Val(specialization_depth),
        )
        @return_on_false complete cumulator_l
        @return_on_nonfinite_array cumulator_l
        (cumulator_r, complete) = _eval_tree_array(
            tree.r,
            cX,
            operators,
            Val(turbo),
            Val(specialize_kernels),
            Val(specialization_depth),
        )
        @return_on_false complete cumulator_r
        @return_on_nonfinite_array cumulator_r
        # op(x, y), for any x or y
        return deg2_eval(cumulator_l, cumulator_r, op, Val(turbo))
    end
end

function deg2_eval(
    cumulator_l::AbstractVector{T}, cumulator_r::AbstractVector{T}, op::F, ::Val{turbo}
)::Tuple{AbstractVector{T},Bool} where {T<:Number,F,turbo}
    @maybe_turbo turbo for j in indices(cumulator_l)
        x = op(cumulator_l[j], cumulator_r[j])::T
        cumulator_l[j] = x
    end
    return (cumulator_l, true)
end

function deg1_eval(
    cumulator::AbstractVector{T}, op::F, ::Val{turbo}
)::Tuple{AbstractVector{T},Bool} where {T<:Number,F,turbo}
    @maybe_turbo turbo for j in indices(cumulator)
        x = op(cumulator[j])::T
        cumulator[j] = x
    end
    return (cumulator, true)
end

function deg0_eval(
    tree::Node{T}, cX::AbstractMatrix{T}
)::Tuple{AbstractVector{T},Bool} where {T<:Number}
    if tree.constant
        return (fill_similar(tree.val::T, cX, axes(cX, 2)), true)
    else
        return (cX[tree.feature, :], true)
    end
end

function deg1_l2_ll0_lr0_eval(
    tree::Node{T}, cX::AbstractMatrix{T}, op::F, op_l::F2, ::Val{turbo}
)::Tuple{AbstractVector{T},Bool} where {T<:Number,F,F2,turbo}
    if tree.l.l.constant && tree.l.r.constant
        val_ll = tree.l.l.val::T
        val_lr = tree.l.r.val::T
        @return_on_check val_ll cX
        @return_on_check val_lr cX
        x_l = op_l(val_ll, val_lr)::T
        @return_on_check x_l cX
        x = op(x_l)::T
        @return_on_check x cX
        return (fill_similar(x, cX, axes(cX, 2)), true)
    elseif tree.l.l.constant
        val_ll = tree.l.l.val::T
        @return_on_check val_ll cX
        feature_lr = tree.l.r.feature
        cumulator = similar(cX, axes(cX, 2))
        @maybe_turbo turbo for j in indices((cX, cumulator), (2, 1))
            x_l = op_l(val_ll, cX[feature_lr, j])::T
            x = isfinite(x_l) ? op(x_l)::T : T(Inf)
            cumulator[j] = x
        end
        return (cumulator, true)
    elseif tree.l.r.constant
        feature_ll = tree.l.l.feature
        val_lr = tree.l.r.val::T
        @return_on_check val_lr cX
        cumulator = similar(cX, axes(cX, 2))
        @maybe_turbo turbo for j in indices((cX, cumulator), (2, 1))
            x_l = op_l(cX[feature_ll, j], val_lr)::T
            x = isfinite(x_l) ? op(x_l)::T : T(Inf)
            cumulator[j] = x
        end
        return (cumulator, true)
    else
        feature_ll = tree.l.l.feature
        feature_lr = tree.l.r.feature
        cumulator = similar(cX, axes(cX, 2))
        @maybe_turbo turbo for j in indices((cX, cumulator), (2, 1))
            x_l = op_l(cX[feature_ll, j], cX[feature_lr, j])::T
            x = isfinite(x_l) ? op(x_l)::T : T(Inf)
            cumulator[j] = x
        end
        return (cumulator, true)
    end
end

# op(op2(x)) for x variable or constant
function deg1_l1_ll0_eval(
    tree::Node{T}, cX::AbstractMatrix{T}, op::F, op_l::F2, ::Val{turbo}
)::Tuple{AbstractVector{T},Bool} where {T<:Number,F,F2,turbo}
    if tree.l.l.constant
        val_ll = tree.l.l.val::T
        @return_on_check val_ll cX
        x_l = op_l(val_ll)::T
        @return_on_check x_l cX
        x = op(x_l)::T
        @return_on_check x cX
        return (fill_similar(x, cX, axes(cX, 2)), true)
    else
        feature_ll = tree.l.l.feature
        cumulator = similar(cX, axes(cX, 2))
        @maybe_turbo turbo for j in indices((cX, cumulator), (2, 1))
            x_l = op_l(cX[feature_ll, j])::T
            x = isfinite(x_l) ? op(x_l)::T : T(Inf)
            cumulator[j] = x
        end
        return (cumulator, true)
    end
end

# op(x, y) for x and y variable/constant
function deg2_l0_r0_eval(
    tree::Node{T}, cX::AbstractMatrix{T}, op::F, ::Val{turbo}
)::Tuple{AbstractVector{T},Bool} where {T<:Number,F,turbo}
    if tree.l.constant && tree.r.constant
        val_l = tree.l.val::T
        @return_on_check val_l cX
        val_r = tree.r.val::T
        @return_on_check val_r cX
        x = op(val_l, val_r)::T
        @return_on_check x cX
        return (fill_similar(x, cX, axes(cX, 2)), true)
    elseif tree.l.constant
        cumulator = similar(cX, axes(cX, 2))
        val_l = tree.l.val::T
        @return_on_check val_l cX
        feature_r = tree.r.feature
        @maybe_turbo turbo for j in indices((cX, cumulator), (2, 1))
            x = op(val_l, cX[feature_r, j])::T
            cumulator[j] = x
        end
    elseif tree.r.constant
        cumulator = similar(cX, axes(cX, 2))
        feature_l = tree.l.feature
        val_r = tree.r.val::T
        @return_on_check val_r cX
        @maybe_turbo turbo for j in indices((cX, cumulator), (2, 1))
            x = op(cX[feature_l, j], val_r)::T
            cumulator[j] = x
        end
    else
        cumulator = similar(cX, axes(cX, 2))
        feature_l = tree.l.feature
        feature_r = tree.r.feature
        @maybe_turbo turbo for j in indices((cX, cumulator), (2, 1))
            x = op(cX[feature_l, j], cX[feature_r, j])::T
            cumulator[j] = x
        end
    end
    return (cumulator, true)
end

# op(x, y) for x variable/constant, y arbitrary
function deg2_l0_eval(
    tree::Node{T}, cumulator::AbstractVector{T}, cX::AbstractArray{T}, op::F, ::Val{turbo}
)::Tuple{AbstractVector{T},Bool} where {T<:Number,F,turbo}
    if tree.l.constant
        val = tree.l.val::T
        @return_on_check val cX
        @maybe_turbo turbo for j in indices(cumulator)
            x = op(val, cumulator[j])::T
            cumulator[j] = x
        end
    else
        feature = tree.l.feature
        @maybe_turbo turbo for j in indices((cX, cumulator), (2, 1))
            x = op(cX[feature, j], cumulator[j])::T
            cumulator[j] = x
        end
    end
    return (cumulator, true)
end

# op(x, y) for x arbitrary, y variable/constant
function deg2_r0_eval(
    tree::Node{T}, cumulator::AbstractVector{T}, cX::AbstractArray{T}, op::F, ::Val{turbo}
)::Tuple{AbstractVector{T},Bool} where {T<:Number,F,turbo}
    if tree.r.constant
        val = tree.r.val::T
        @return_on_check val cX
        @maybe_turbo turbo for j in indices(cumulator)
            x = op(cumulator[j], val)::T
            cumulator[j] = x
        end
    else
        feature = tree.r.feature
        @maybe_turbo turbo for j in indices((cX, cumulator), (2, 1))
            x = op(cumulator[j], cX[feature, j])::T
            cumulator[j] = x
        end
    end
    return (cumulator, true)
end

"""
    _eval_constant_tree(tree::Node{T}, operators::OperatorEnum)::Tuple{T,Bool} where {T<:Number}

Evaluate a tree which is assumed to not contain any variable nodes. This
gives better performance, as we do not need to perform computation
over an entire array when the values are all the same.
"""
function _eval_constant_tree(
    tree::Node{T}, operators::OperatorEnum
)::Tuple{T,Bool} where {T<:Number}
    if tree.degree == 0
        return deg0_eval_constant(tree)
    elseif tree.degree == 1
        return deg1_eval_constant(tree, operators.unaops[tree.op], operators)
    else
        return deg2_eval_constant(tree, operators.binops[tree.op], operators)
    end
end

@inline function deg0_eval_constant(tree::Node{T})::Tuple{T,Bool} where {T<:Number}
    return tree.val::T, true
end

function deg1_eval_constant(
    tree::Node{T}, op::F, operators::OperatorEnum
)::Tuple{T,Bool} where {T<:Number,F}
    (cumulator, complete) = _eval_constant_tree(tree.l, operators)
    !complete && return zero(T), false
    output = op(cumulator)::T
    return output, isfinite(output)
end

function deg2_eval_constant(
    tree::Node{T}, op::F, operators::OperatorEnum
)::Tuple{T,Bool} where {T<:Number,F}
    (cumulator, complete) = _eval_constant_tree(tree.l, operators)
    !complete && return zero(T), false
    (cumulator2, complete2) = _eval_constant_tree(tree.r, operators)
    !complete2 && return zero(T), false
    output = op(cumulator, cumulator2)::T
    return output, isfinite(output)
end

"""
    differentiable_eval_tree_array(tree::Node, cX::AbstractMatrix, operators::OperatorEnum)

Evaluate an expression tree in a way that can be auto-differentiated.
"""
function differentiable_eval_tree_array(
    tree::Node{T1}, cX::AbstractMatrix{T}, operators::OperatorEnum
)::Tuple{AbstractVector{T},Bool} where {T<:Number,T1}
    if tree.degree == 0
        if tree.constant
            return (fill_similar(one(T), cX, axes(cX, 2)) .* tree.val, true)
        else
            return (cX[tree.feature, :], true)
        end
    elseif tree.degree == 1
        return deg1_diff_eval(tree, cX, operators.unaops[tree.op], operators)
    else
        return deg2_diff_eval(tree, cX, operators.binops[tree.op], operators)
    end
end

function deg1_diff_eval(
    tree::Node{T1}, cX::AbstractMatrix{T}, op::F, operators::OperatorEnum
)::Tuple{AbstractVector{T},Bool} where {T<:Number,F,T1}
    (left, complete) = differentiable_eval_tree_array(tree.l, cX, operators)
    @return_on_false complete left
    out = op.(left)
    no_nans = !any(x -> (!isfinite(x)), out)
    return (out, no_nans)
end

function deg2_diff_eval(
    tree::Node{T1}, cX::AbstractMatrix{T}, op::F, operators::OperatorEnum
)::Tuple{AbstractVector{T},Bool} where {T<:Number,F,T1}
    (left, complete) = differentiable_eval_tree_array(tree.l, cX, operators)
    @return_on_false complete left
    (right, complete2) = differentiable_eval_tree_array(tree.r, cX, operators)
    @return_on_false complete2 left
    out = op.(left, right)
    no_nans = !any(x -> (!isfinite(x)), out)
    return (out, no_nans)
end

"""
    eval_tree_array(tree::Node, cX::AbstractMatrix, operators::GenericOperatorEnum; throw_errors::Bool=true)

Evaluate a generic binary tree (equation) over a given input data,
whatever that input data may be. The `operators` enum contains all
of the operators used. Unlike `eval_tree_array` with the normal
`OperatorEnum`, the array `cX` is sliced only along the first dimension.
i.e., if `cX` is a vector, then the output of a feature node
will be a scalar. If `cX` is a 3D tensor, then the output
of a feature node will be a 2D tensor.
Note also that `tree.feature` will index along the first axis of `cX`.

However, there is no requirement about input and output types in general.
You may set up your tree such that some operator nodes work on tensors, while
other operator nodes work on scalars. `eval_tree_array` will simply
return `nothing` if a given operator is not defined for the given input type.

This function can be represented by the following pseudocode:

```
function eval(current_node)
    if current_node is leaf
        return current_node.value
    elif current_node is degree 1
        return current_node.operator(eval(current_node.left_child))
    else
        return current_node.operator(eval(current_node.left_child), eval(current_node.right_child))
```

# Arguments
- `tree::Node`: The root node of the tree to evaluate.
- `cX::AbstractArray`: The input data to evaluate the tree on.
- `operators::GenericOperatorEnum`: The operators used in the tree.
- `throw_errors::Bool=true`: Whether to throw errors
    if they occur during evaluation. Otherwise,
    MethodErrors will be caught before they happen and 
    evaluation will return `nothing`,
    rather than throwing an error. This is useful in cases
    where you are unsure if a particular tree is valid or not,
    and would prefer to work with `nothing` as an output.

# Returns
- `(output, complete)::Tuple{Any, Bool}`: the result,
    as well as if the evaluation completed successfully (true/false).
    If evaluation failed, `nothing` will be returned for the first argument.
    A `false` complete means an operator was called on input types
    that it was not defined for.
"""
function eval_tree_array(
    tree::Node, cX::AbstractArray, operators::GenericOperatorEnum; throw_errors::Bool=true
)
    !throw_errors && return _eval_tree_array_generic(tree, cX, operators, Val(false))
    try
        return _eval_tree_array_generic(tree, cX, operators, Val(true))
    catch e
        tree_s = string_tree(tree, operators)
        error_msg = "Failed to evaluate tree $(tree_s)."
        if isa(e, MethodError)
            error_msg *= (
                " Note that you can efficiently skip MethodErrors" *
                " beforehand by passing `throw_errors=false` to " *
                " `eval_tree_array`."
            )
        end
        throw(ErrorException(error_msg))
    end
end

function _eval_tree_array_generic(
    tree::Node{T1},
    cX::AbstractArray{T2,N},
    operators::GenericOperatorEnum,
    ::Val{throw_errors},
) where {T1,T2,N,throw_errors}
    if tree.degree == 0
        if tree.constant
            return (tree.val::T1), true
        else
            if N == 1
                return cX[tree.feature], true
            else
                return selectdim(cX, 1, tree.feature), true
            end
        end
    elseif tree.degree == 1
        return deg1_eval_generic(
            tree, cX, operators.unaops[tree.op], operators, Val(throw_errors)
        )
    else
        return deg2_eval_generic(
            tree, cX, operators.binops[tree.op], operators, Val(throw_errors)
        )
    end
end

function deg1_eval_generic(
    tree, cX, op::F, operators::GenericOperatorEnum, ::Val{throw_errors}
) where {F,throw_errors}
    left, complete = eval_tree_array(tree.l, cX, operators)
    !throw_errors && !complete && return nothing, false
    !throw_errors && !hasmethod(op, Tuple{typeof(left)}) && return nothing, false
    return op(left), true
end

function deg2_eval_generic(
    tree, cX, op::F, operators::GenericOperatorEnum, ::Val{throw_errors}
) where {F,throw_errors}
    left, complete = eval_tree_array(tree.l, cX, operators)
    !throw_errors && !complete && return nothing, false
    right, complete = eval_tree_array(tree.r, cX, operators)
    !throw_errors && !complete && return nothing, false
    !throw_errors &&
        !hasmethod(op, Tuple{typeof(left),typeof(right)}) &&
        return nothing, false
    return op(left, right), true
end

end
