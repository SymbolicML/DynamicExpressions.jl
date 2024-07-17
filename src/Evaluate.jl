module EvaluateModule

using DispatchDoctor: @unstable

import ..NodeModule: AbstractExpressionNode, constructorof, with_type_parameters
import ..StringsModule: string_tree
import ..OperatorEnumModule: OperatorEnum, GenericOperatorEnum
import ..UtilsModule: fill_similar, counttuple, ResultOk
import ..NodeUtilsModule: is_constant
import ..ExtensionInterfaceModule: bumper_eval_tree_array, _is_loopvectorization_loaded
import ..ValueInterfaceModule: is_valid, is_valid_array

const OPERATOR_LIMIT_BEFORE_SLOWDOWN = 15

macro return_on_check(val, X)
    :(
        if !is_valid($(esc(val)))
            return $(ResultOk)(similar($(esc(X)), axes($(esc(X)), 2)), false)
        end
    )
end

macro return_on_nonfinite_array(array)
    :(
        if !is_valid_array($(esc(array)))
            return $(ResultOk)($(esc(array)), false)
        end
    )
end

"""
    eval_tree_array(tree::AbstractExpressionNode, cX::AbstractMatrix{T}, operators::OperatorEnum; turbo::Union{Bool,Val}=Val(false), bumper::Union{Bool,Val}=Val(false))

Evaluate a binary tree (equation) over a given input data matrix. The
operators contain all of the operators used. This function fuses doublets
and triplets of operations for lower memory usage.

# Arguments
- `tree::AbstractExpressionNode`: The root node of the tree to evaluate.
- `cX::AbstractMatrix{T}`: The input data to evaluate the tree on.
- `operators::OperatorEnum`: The operators used in the tree.
- `turbo::Union{Bool,Val}`: Use LoopVectorization.jl for faster evaluation.
- `bumper::Union{Bool,Val}`: Use Bumper.jl for faster evaluation.

# Returns
- `(output, complete)::Tuple{AbstractVector{T}, Bool}`: the result,
    which is a 1D array, as well as if the evaluation completed
    successfully (true/false). A `false` complete means an infinity
    or nan was encountered, and a large loss should be assigned
    to the equation.

# Notes
This function can be represented by the following pseudocode:

```
def eval(current_node)
    if current_node is leaf
        return current_node.value
    elif current_node is degree 1
        return current_node.operator(eval(current_node.left_child))
    else
        return current_node.operator(eval(current_node.left_child), eval(current_node.right_child))
```
The bulk of the code is for optimizations and pre-emptive NaN/Inf checks,
which speed up evaluation significantly.
"""
function eval_tree_array(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum;
    turbo::Union{Bool,Val}=Val(false),
    bumper::Union{Bool,Val}=Val(false),
) where {T}
    v_turbo = isa(turbo, Val) ? turbo : (turbo ? Val(true) : Val(false))
    v_bumper = isa(bumper, Val) ? bumper : (bumper ? Val(true) : Val(false))
    if v_turbo isa Val{true} || v_bumper isa Val{true}
        @assert T in (Float32, Float64)
    end
    if v_turbo isa Val{true}
        _is_loopvectorization_loaded(0) ||
            error("Please load the LoopVectorization.jl package to use this feature.")
    end
    if (v_turbo isa Val{true} || v_bumper isa Val{true}) && !(T <: Number)
        error(
            "Bumper and LoopVectorization features are only compatible with numeric element types",
        )
    end
    if v_bumper isa Val{true}
        return bumper_eval_tree_array(tree, cX, operators, v_turbo)
    end

    result = _eval_tree_array(tree, cX, operators, v_turbo)
    return (result.x, result.ok && is_valid_array(result.x))
end

function eval_tree_array(
    tree::AbstractExpressionNode{T}, cX::AbstractVector{T}, operators::OperatorEnum; kws...
) where {T}
    return eval_tree_array(tree, reshape(cX, (size(cX, 1), 1)), operators; kws...)
end

function eval_tree_array(
    tree::AbstractExpressionNode{T1},
    cX::AbstractMatrix{T2},
    operators::OperatorEnum;
    turbo::Union{Bool,Val}=Val(false),
    bumper::Union{Bool,Val}=Val(false),
) where {T1,T2}
    T = promote_type(T1, T2)
    @warn "Warning: eval_tree_array received mixed types: tree=$(T1) and data=$(T2)."
    tree = convert(with_type_parameters(typeof(tree), T), tree)
    cX = T.(cX)
    return eval_tree_array(tree, cX, operators; turbo, bumper)
end
function eval_tree_array(
    trees::Union{Tuple{N,Vararg{N}},AbstractVector{N}},
    cX::AbstractMatrix{T},
    operators::OperatorEnum;
    kws...,
) where {T<:Number,N<:AbstractExpressionNode{T}}
    outs = (t -> eval_tree_array(t, cX, operators; kws...)).(trees)
    return first.(outs), last.(outs)
end

get_nuna(::Type{<:OperatorEnum{B,U}}) where {B,U} = counttuple(U)
get_nbin(::Type{<:OperatorEnum{B}}) where {B} = counttuple(B)

function _eval_tree_array(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    ::Val{turbo},
)::ResultOk where {T,turbo}
    # First, we see if there are only constants in the tree - meaning
    # we can just return the constant result.
    if tree.degree == 0
        return deg0_eval(tree, cX)
    elseif is_constant(tree)
        # Speed hack for constant trees.
        const_result = dispatch_constant_tree(tree, operators)::ResultOk{Vector{T}}
        !const_result.ok && return ResultOk(similar(cX, axes(cX, 2)), false)
        return ResultOk(fill_similar(const_result.x[], cX, axes(cX, 2)), true)
    elseif tree.degree == 1
        op_idx = tree.op
        return dispatch_deg1_eval(tree, cX, op_idx, operators, Val(turbo))
    else
        # TODO - add op(op2(x, y), z) and op(x, op2(y, z))
        # op(x, y), where x, y are constants or variables.
        op_idx = tree.op
        return dispatch_deg2_eval(tree, cX, op_idx, operators, Val(turbo))
    end
end

function deg2_eval(
    cumulator_l::AbstractVector{T}, cumulator_r::AbstractVector{T}, op::F, ::Val{false}
)::ResultOk where {T,F}
    @inbounds @simd for j in eachindex(cumulator_l)
        x = op(cumulator_l[j], cumulator_r[j])::T
        cumulator_l[j] = x
    end
    return ResultOk(cumulator_l, true)
end

function deg1_eval(cumulator::AbstractVector{T}, op::F, ::Val{false})::ResultOk where {T,F}
    @inbounds @simd for j in eachindex(cumulator)
        x = op(cumulator[j])::T
        cumulator[j] = x
    end
    return ResultOk(cumulator, true)
end

function deg0_eval(
    tree::AbstractExpressionNode{T}, cX::AbstractMatrix{T}
)::ResultOk where {T}
    if tree.constant
        return ResultOk(fill_similar(tree.val, cX, axes(cX, 2)), true)
    else
        return ResultOk(cX[tree.feature, :], true)
    end
end

@generated function dispatch_deg2_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op_idx::Integer,
    operators::OperatorEnum,
    ::Val{turbo},
) where {T,turbo}
    nbin = get_nbin(operators)
    long_compilation_time = nbin > OPERATOR_LIMIT_BEFORE_SLOWDOWN
    if long_compilation_time
        return quote
            result_l = _eval_tree_array(tree.l, cX, operators, Val(turbo))
            !result_l.ok && return result_l
            @return_on_nonfinite_array result_l.x
            result_r = _eval_tree_array(tree.r, cX, operators, Val(turbo))
            !result_r.ok && return result_r
            @return_on_nonfinite_array result_r.x
            # op(x, y), for any x or y
            deg2_eval(result_l.x, result_r.x, operators.binops[op_idx], Val(turbo))
        end
    end
    return quote
        return Base.Cartesian.@nif(
            $nbin,
            i -> i == op_idx,
            i -> let op = operators.binops[i]
                if tree.l.degree == 0 && tree.r.degree == 0
                    deg2_l0_r0_eval(tree, cX, op, Val(turbo))
                elseif tree.r.degree == 0
                    result_l = _eval_tree_array(tree.l, cX, operators, Val(turbo))
                    !result_l.ok && return result_l
                    @return_on_nonfinite_array result_l.x
                    # op(x, y), where y is a constant or variable but x is not.
                    deg2_r0_eval(tree, result_l.x, cX, op, Val(turbo))
                elseif tree.l.degree == 0
                    result_r = _eval_tree_array(tree.r, cX, operators, Val(turbo))
                    !result_r.ok && return result_r
                    @return_on_nonfinite_array result_r.x
                    # op(x, y), where x is a constant or variable but y is not.
                    deg2_l0_eval(tree, result_r.x, cX, op, Val(turbo))
                else
                    result_l = _eval_tree_array(tree.l, cX, operators, Val(turbo))
                    !result_l.ok && return result_l
                    @return_on_nonfinite_array result_l.x
                    result_r = _eval_tree_array(tree.r, cX, operators, Val(turbo))
                    !result_r.ok && return result_r
                    @return_on_nonfinite_array result_r.x
                    # op(x, y), for any x or y
                    deg2_eval(result_l.x, result_r.x, op, Val(turbo))
                end
            end
        )
    end
end
@generated function dispatch_deg1_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op_idx::Integer,
    operators::OperatorEnum,
    ::Val{turbo},
) where {T,turbo}
    nuna = get_nuna(operators)
    long_compilation_time = nuna > OPERATOR_LIMIT_BEFORE_SLOWDOWN
    if long_compilation_time
        return quote
            result = _eval_tree_array(tree.l, cX, operators, Val(turbo))
            !result.ok && return result
            @return_on_nonfinite_array result.x
            deg1_eval(result.x, operators.unaops[op_idx], Val(turbo))
        end
    end
    # This @nif lets us generate an if statement over choice of operator,
    # which means the compiler will be able to completely avoid type inference on operators.
    return quote
        Base.Cartesian.@nif(
            $nuna,
            i -> i == op_idx,
            i -> let op = operators.unaops[i]
                if tree.l.degree == 2 && tree.l.l.degree == 0 && tree.l.r.degree == 0
                    # op(op2(x, y)), where x, y, z are constants or variables.
                    l_op_idx = tree.l.op
                    dispatch_deg1_l2_ll0_lr0_eval(
                        tree, cX, op, l_op_idx, operators.binops, Val(turbo)
                    )
                elseif tree.l.degree == 1 && tree.l.l.degree == 0
                    # op(op2(x)), where x is a constant or variable.
                    l_op_idx = tree.l.op
                    dispatch_deg1_l1_ll0_eval(
                        tree, cX, op, l_op_idx, operators.unaops, Val(turbo)
                    )
                else
                    # op(x), for any x.
                    result = _eval_tree_array(tree.l, cX, operators, Val(turbo))
                    !result.ok && return result
                    @return_on_nonfinite_array result.x
                    deg1_eval(result.x, op, Val(turbo))
                end
            end
        )
    end
end
@generated function dispatch_deg1_l2_ll0_lr0_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op::F,
    l_op_idx::Integer,
    binops,
    ::Val{turbo},
) where {T,F,turbo}
    nbin = counttuple(binops)
    # (Note this is only called from dispatch_deg1_eval, which has already
    # checked for long compilation times, so we don't need to check here)
    quote
        Base.Cartesian.@nif(
            $nbin,
            j -> j == l_op_idx,
            j -> let op_l = binops[j]
                deg1_l2_ll0_lr0_eval(tree, cX, op, op_l, Val(turbo))
            end,
        )
    end
end
@generated function dispatch_deg1_l1_ll0_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op::F,
    l_op_idx::Integer,
    unaops,
    ::Val{turbo},
)::ResultOk where {T,F,turbo}
    nuna = counttuple(unaops)
    quote
        Base.Cartesian.@nif(
            $nuna,
            j -> j == l_op_idx,
            j -> let op_l = unaops[j]
                deg1_l1_ll0_eval(tree, cX, op, op_l, Val(turbo))
            end,
        )
    end
end

function deg1_l2_ll0_lr0_eval(
    tree::AbstractExpressionNode{T}, cX::AbstractMatrix{T}, op::F, op_l::F2, ::Val{false}
) where {T,F,F2}
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
        @inbounds @simd for j in axes(cX, 2)
            x_l = op_l(val_ll, cX[feature_lr, j])::T
            x = is_valid(x_l) ? op(x_l)::T : T(Inf)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.l.r.constant
        feature_ll = tree.l.l.feature
        val_lr = tree.l.r.val
        @return_on_check val_lr cX
        cumulator = similar(cX, axes(cX, 2))
        @inbounds @simd for j in axes(cX, 2)
            x_l = op_l(cX[feature_ll, j], val_lr)::T
            x = is_valid(x_l) ? op(x_l)::T : T(Inf)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        feature_ll = tree.l.l.feature
        feature_lr = tree.l.r.feature
        cumulator = similar(cX, axes(cX, 2))
        @inbounds @simd for j in axes(cX, 2)
            x_l = op_l(cX[feature_ll, j], cX[feature_lr, j])::T
            x = is_valid(x_l) ? op(x_l)::T : T(Inf)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    end
end

# op(op2(x)) for x variable or constant
function deg1_l1_ll0_eval(
    tree::AbstractExpressionNode{T}, cX::AbstractMatrix{T}, op::F, op_l::F2, ::Val{false}
) where {T,F,F2}
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
        @inbounds @simd for j in axes(cX, 2)
            x_l = op_l(cX[feature_ll, j])::T
            x = is_valid(x_l) ? op(x_l)::T : T(Inf)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    end
end

# op(x, y) for x and y variable/constant
function deg2_l0_r0_eval(
    tree::AbstractExpressionNode{T}, cX::AbstractMatrix{T}, op::F, ::Val{false}
) where {T,F}
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
        @inbounds @simd for j in axes(cX, 2)
            x = op(val_l, cX[feature_r, j])::T
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.r.constant
        cumulator = similar(cX, axes(cX, 2))
        feature_l = tree.l.feature
        val_r = tree.r.val
        @return_on_check val_r cX
        @inbounds @simd for j in axes(cX, 2)
            x = op(cX[feature_l, j], val_r)::T
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        cumulator = similar(cX, axes(cX, 2))
        feature_l = tree.l.feature
        feature_r = tree.r.feature
        @inbounds @simd for j in axes(cX, 2)
            x = op(cX[feature_l, j], cX[feature_r, j])::T
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
    ::Val{false},
) where {T,F}
    if tree.l.constant
        val = tree.l.val
        @return_on_check val cX
        @inbounds @simd for j in eachindex(cumulator)
            x = op(val, cumulator[j])::T
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        feature = tree.l.feature
        @inbounds @simd for j in eachindex(cumulator)
            x = op(cX[feature, j], cumulator[j])::T
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    end
end

# op(x, y) for x arbitrary, y variable/constant
function deg2_r0_eval(
    tree::AbstractExpressionNode{T},
    cumulator::AbstractVector{T},
    cX::AbstractArray{T},
    op::F,
    ::Val{false},
) where {T,F}
    if tree.r.constant
        val = tree.r.val
        @return_on_check val cX
        @inbounds @simd for j in eachindex(cumulator)
            x = op(cumulator[j], val)::T
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        feature = tree.r.feature
        @inbounds @simd for j in eachindex(cumulator)
            x = op(cumulator[j], cX[feature, j])::T
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    end
end

"""
    dispatch_constant_tree(tree::AbstractExpressionNode{T}, operators::OperatorEnum) where {T}

Evaluate a tree which is assumed to not contain any variable nodes. This
gives better performance, as we do not need to perform computation
over an entire array when the values are all the same.
"""
@generated function dispatch_constant_tree(
    tree::AbstractExpressionNode{T}, operators::OperatorEnum
) where {T}
    nuna = get_nuna(operators)
    nbin = get_nbin(operators)
    deg1_branch = if nuna > OPERATOR_LIMIT_BEFORE_SLOWDOWN
        quote
            deg1_eval_constant(tree, operators.unaops[op_idx], operators)::ResultOk{Vector{T}}
        end
    else
        quote
            Base.Cartesian.@nif(
                $nuna,
                i -> i == op_idx,
                i -> deg1_eval_constant(
                    tree, operators.unaops[i], operators
                )::ResultOk{Vector{T}}
            )
        end
    end
    deg2_branch = if nbin > OPERATOR_LIMIT_BEFORE_SLOWDOWN
        quote
            deg2_eval_constant(tree, operators.binops[op_idx], operators)::ResultOk{Vector{T}}
        end
    else
        quote
            Base.Cartesian.@nif(
                $nbin,
                i -> i == op_idx,
                i -> deg2_eval_constant(
                    tree, operators.binops[i], operators
                )::ResultOk{Vector{T}}
            )
        end
    end
    return quote
        if tree.degree == 0
            return deg0_eval_constant(tree)::ResultOk{Vector{T}}
        elseif tree.degree == 1
            op_idx = tree.op
            return $deg1_branch
        else
            op_idx = tree.op
            return $deg2_branch
        end
    end
end

@inline function deg0_eval_constant(tree::AbstractExpressionNode{T}) where {T}
    output = tree.val
    return ResultOk([output], true)::ResultOk{Vector{T}}
end

function deg1_eval_constant(
    tree::AbstractExpressionNode{T}, op::F, operators::OperatorEnum
) where {T,F}
    result = dispatch_constant_tree(tree.l, operators)
    !result.ok && return result
    output = op(result.x[])::T
    return ResultOk([output], is_valid(output))::ResultOk{Vector{T}}
end

function deg2_eval_constant(
    tree::AbstractExpressionNode{T}, op::F, operators::OperatorEnum
) where {T,F}
    cumulator = dispatch_constant_tree(tree.l, operators)
    !cumulator.ok && return cumulator
    result_r = dispatch_constant_tree(tree.r, operators)
    !result_r.ok && return result_r
    output = op(cumulator.x[], result_r.x[])::T
    return ResultOk([output], is_valid(output))::ResultOk{Vector{T}}
end

"""
    differentiable_eval_tree_array(tree::AbstractExpressionNode, cX::AbstractMatrix, operators::OperatorEnum)

Evaluate an expression tree in a way that can be auto-differentiated.
"""
function differentiable_eval_tree_array(
    tree::AbstractExpressionNode{T1}, cX::AbstractMatrix{T}, operators::OperatorEnum
) where {T<:Number,T1}
    result = _differentiable_eval_tree_array(tree, cX, operators)
    return (result.x, result.ok)
end

@generated function _differentiable_eval_tree_array(
    tree::AbstractExpressionNode{T1}, cX::AbstractMatrix{T}, operators::OperatorEnum
)::ResultOk where {T<:Number,T1}
    nuna = get_nuna(operators)
    nbin = get_nbin(operators)
    quote
        if tree.degree == 0
            if tree.constant
                ResultOk(fill_similar(one(T), cX, axes(cX, 2)) .* tree.val, true)
            else
                ResultOk(cX[tree.feature, :], true)
            end
        elseif tree.degree == 1
            op_idx = tree.op
            Base.Cartesian.@nif(
                $nuna,
                i -> i == op_idx,
                i -> deg1_diff_eval(tree, cX, operators.unaops[i], operators)
            )
        else
            op_idx = tree.op
            Base.Cartesian.@nif(
                $nbin,
                i -> i == op_idx,
                i -> deg2_diff_eval(tree, cX, operators.binops[i], operators)
            )
        end
    end
end

function deg1_diff_eval(
    tree::AbstractExpressionNode{T1}, cX::AbstractMatrix{T}, op::F, operators::OperatorEnum
)::ResultOk where {T<:Number,F,T1}
    left = _differentiable_eval_tree_array(tree.l, cX, operators)
    !left.ok && return left
    out = op.(left.x)
    return ResultOk(out, all(isfinite, out))
end

function deg2_diff_eval(
    tree::AbstractExpressionNode{T1}, cX::AbstractMatrix{T}, op::F, operators::OperatorEnum
)::ResultOk where {T<:Number,F,T1}
    left = _differentiable_eval_tree_array(tree.l, cX, operators)
    !left.ok && return left
    right = _differentiable_eval_tree_array(tree.r, cX, operators)
    !right.ok && return right
    out = op.(left.x, right.x)
    return ResultOk(out, all(isfinite, out))
end

"""
    eval_tree_array(tree::AbstractExpressionNode, cX::AbstractMatrix, operators::GenericOperatorEnum; throw_errors::Bool=true)

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
- `tree::AbstractExpressionNode`: The root node of the tree to evaluate.
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
@unstable function eval_tree_array(
    tree::AbstractExpressionNode{T1},
    cX::AbstractArray{T2,N},
    operators::GenericOperatorEnum;
    throw_errors::Bool=true,
) where {T1,T2,N}
    try
        return _eval_tree_array_generic(tree, cX, operators, Val(true))
    catch e
        if !throw_errors
            return nothing, false
        end
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

@unstable function _eval_tree_array_generic(
    tree::AbstractExpressionNode{T1},
    cX::AbstractArray{T2,N},
    operators::GenericOperatorEnum,
    ::Val{throw_errors},
) where {T1,T2,N,throw_errors}
    if tree.degree == 0
        if tree.constant
            if N == 1
                return (tree.val::T1), true
            else
                return fill(tree.val::T1, size(cX)[2:N]), true
            end
        else
            if N == 1
                return (cX[tree.feature]), true
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

@unstable function deg1_eval_generic(
    tree::AbstractExpressionNode{T1},
    cX::AbstractArray{T2,N},
    op::F,
    operators::GenericOperatorEnum,
    ::Val{throw_errors},
) where {F,T1,T2,N,throw_errors}
    left, complete = _eval_tree_array_generic(tree.l, cX, operators, Val(throw_errors))
    !throw_errors && !complete && return nothing, false
    !throw_errors &&
        !hasmethod(op, N == 1 ? Tuple{typeof(left)} : Tuple{eltype(left)}) &&
        return nothing, false
    if N == 1
        return op(left), true
    else
        return op.(left), true
    end
end

@unstable function deg2_eval_generic(
    tree::AbstractExpressionNode{T1},
    cX::AbstractArray{T2,N},
    op::F,
    operators::GenericOperatorEnum,
    ::Val{throw_errors},
) where {F,T1,T2,N,throw_errors}
    left, complete = _eval_tree_array_generic(tree.l, cX, operators, Val(throw_errors))
    !throw_errors && !complete && return nothing, false
    right, complete = _eval_tree_array_generic(tree.r, cX, operators, Val(throw_errors))
    !throw_errors && !complete && return nothing, false
    !throw_errors &&
        !hasmethod(
            op,
            N == 1 ? Tuple{typeof(left),typeof(right)} : Tuple{eltype(left),eltype(right)},
        ) &&
        return nothing, false
    if N == 1
        return op(left, right), true
    else
        return op.(left, right), true
    end
end

end
