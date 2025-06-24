module EvaluateDerivativeModule

import ..NodeModule: AbstractExpressionNode, constructorof, get_children
import ..OperatorEnumModule: OperatorEnum
import ..UtilsModule: fill_similar, ResultOk2, @finite
import ..ValueInterfaceModule: is_valid_array
import ..NodeUtilsModule: count_constant_nodes, index_constant_nodes, NodeIndex
import ..EvaluateModule:
    deg0_eval, get_op, get_nops, OPERATOR_LIMIT_BEFORE_SLOWDOWN, EvalOptions
import ..ExtensionInterfaceModule: _zygote_gradient

"""
    eval_diff_tree_array(
        tree::AbstractExpressionNode{T},
        cX::AbstractMatrix{T},
        operators::OperatorEnum,
        direction::Integer;
        turbo::Union{Bool,Val}=Val(false)
    ) where {T<:Number}

Compute the forward derivative of an expression, using a similar
structure and optimization to eval_tree_array. `direction` is the index of a particular
variable in the expression. e.g., `direction=1` would indicate derivative with
respect to `x1`.

# Arguments

- `tree::AbstractExpressionNode`: The expression tree to evaluate.
- `cX::AbstractMatrix{T}`: The data matrix, with shape `[num_features, num_rows]`.
- `operators::OperatorEnum`: The operators used to create the `tree`.
- `direction::Integer`: The index of the variable to take the derivative with respect to.
- `turbo::Union{Bool,Val}`: Use LoopVectorization.jl for faster evaluation. Currently this does not have
    any effect.

# Returns

- `(evaluation, derivative, complete)::Tuple{AbstractVector{T}, AbstractVector{T}, Bool}`: the normal evaluation,
    the derivative, and whether the evaluation completed as normal (or encountered a nan or inf).
"""
function eval_diff_tree_array(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    direction::Integer;
    turbo::Union{Bool,Val}=Val(false),
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
    turbo::Union{Bool,Val}=Val(false),
) where {T1<:Number,T2<:Number}
    T = promote_type(T1, T2)
    @warn "Warning: eval_diff_tree_array received mixed types: tree=$(T1) and data=$(T2)."
    tree = convert(constructorof(typeof(tree)){T}, tree)
    cX = Base.Fix1(convert, T).(cX)
    return eval_diff_tree_array(tree, cX, operators, direction; turbo=turbo)
end

@generated function _eval_diff_tree_array(
    tree::AbstractExpressionNode{T,D},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    direction::Integer,
)::ResultOk2 where {T<:Number,D}
    quote
        deg = tree.degree
        deg == 0 && return diff_deg0_eval(tree, cX, direction)
        Base.Cartesian.@nif(
            $D,
            i -> i == deg,  # COV_EXCL_LINE
            i -> begin  # COV_EXCL_LINE
                dispatch_diff_degn_eval(tree, cX, Val(i), operators, direction)
            end,
        )
    end
end

function diff_deg0_eval(
    tree::AbstractExpressionNode{T}, cX::AbstractMatrix{T}, direction::Integer
) where {T<:Number}
    const_part = deg0_eval(tree, cX, EvalOptions()).x
    derivative_part = if ((!tree.constant) && tree.feature == direction)
        fill_similar(one(T), cX, axes(cX, 2))
    else
        fill_similar(zero(T), cX, axes(cX, 2))
    end
    return ResultOk2(const_part, derivative_part, true)
end

@generated function diff_degn_eval(
    x_cumulators::NTuple{N}, dx_cumulators::NTuple{N}, op::F, direction::Integer
) where {N,F}
    quote
        Base.Cartesian.@nexprs($N, i -> begin  # COV_EXCL_LINE
            x_cumulator_i = x_cumulators[i]
            dx_cumulator_i = dx_cumulators[i]
        end)
        diff_op = _zygote_gradient(op, Val(N))
        @finite @inbounds @simd ivdep for j in eachindex(x_cumulator_1)
            x = Base.Cartesian.@ncall($N, op, i -> x_cumulator_i[j])
            Base.Cartesian.@ntuple($N, i -> grad_i) = Base.Cartesian.@ncall(
                $N, diff_op, i -> x_cumulator_i[j]
            )
            dx = Base.Cartesian.@ncall($N, +, i -> grad_i * dx_cumulator_i[j])
            x_cumulator_1[j] = x
            dx_cumulator_1[j] = dx
        end  # COV_EXCL_LINE
        return ResultOk2(x_cumulator_1, dx_cumulator_1, true)
    end
end

@generated function dispatch_diff_degn_eval(
    tree::AbstractExpressionNode{T,D},
    cX::AbstractMatrix{T},
    ::Val{degree},
    operators::OperatorEnum,
    direction::Integer,
) where {T<:Number,D,degree}
    nops = get_nops(operators, Val(degree))

    setup = quote
        cs = get_children(tree, Val($degree))
        Base.Cartesian.@nexprs(  # COV_EXCL_LINE
            $degree,
            i -> begin  # COV_EXCL_LINE
                result_i = _eval_diff_tree_array(cs[i], cX, operators, direction)
                !result_i.ok && return result_i
            end
        )
        x_cumulators = Base.Cartesian.@ntuple($degree, i -> result_i.x)
        dx_cumulators = Base.Cartesian.@ntuple($degree, i -> result_i.dx)
        op_idx = tree.op
    end

    if nops > OPERATOR_LIMIT_BEFORE_SLOWDOWN
        quote
            $setup  # COV_EXCL_LINE
            diff_degn_eval(
                x_cumulators, dx_cumulators, operators[$degree][op_idx], direction
            )
        end
    else
        quote
            $setup  # COV_EXCL_LINE
            Base.Cartesian.@nif(
                $nops,
                i -> i == op_idx,  # COV_EXCL_LINE
                i -> begin  # COV_EXCL_LINE
                    diff_degn_eval(
                        x_cumulators,
                        dx_cumulators,
                        get_op(operators, Val($degree), Val(i)),
                        direction,
                    )
                end,
            )
        end
    end
end

"""
    eval_grad_tree_array(tree::AbstractExpressionNode{T}, cX::AbstractMatrix{T}, operators::OperatorEnum; variable::Union{Bool,Val}=Val(false), turbo::Union{Bool,Val}=Val(false))

Compute the forward-mode derivative of an expression, using a similar
structure and optimization to eval_tree_array. `variable` specifies whether
we should take derivatives with respect to features (i.e., cX), or with respect
to every constant in the expression.

# Arguments

- `tree::AbstractExpressionNode{T}`: The expression tree to evaluate.
- `cX::AbstractMatrix{T}`: The data matrix, with each column being a data point.
- `operators::OperatorEnum`: The operators used to create the `tree`.
- `variable::Union{Bool,Val}`: Whether to take derivatives with respect to features (i.e., `cX` - with `variable=true`),
    or with respect to every constant in the expression (`variable=false`).
- `turbo::Union{Bool,Val}`: Use LoopVectorization.jl for faster evaluation. Currently this does not have
    any effect.

# Returns

- `(evaluation, gradient, complete)::Tuple{AbstractVector{T}, AbstractMatrix{T}, Bool}`: the normal evaluation,
    the gradient, and whether the evaluation completed as normal (or encountered a nan or inf).
"""
function eval_grad_tree_array(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum;
    variable::Union{Bool,Val}=Val(false),
    turbo::Union{Bool,Val}=Val(false),
) where {T<:Number}
    variable_mode = isa(variable, Val{true}) || (isa(variable, Bool) && variable)
    constant_mode = isa(variable, Val{false}) || (isa(variable, Bool) && !variable)
    both_mode = isa(variable, Val{:both})

    n_gradients = if variable_mode
        size(cX, 1)::Int
    elseif constant_mode
        count_constant_nodes(tree)::Int
    elseif both_mode
        size(cX, 1) + count_constant_nodes(tree)
    end

    result = if variable_mode
        eval_grad_tree_array(tree, n_gradients, nothing, cX, operators, Val(true))
    elseif constant_mode
        index_tree = index_constant_nodes(tree)
        eval_grad_tree_array(
            tree, n_gradients, index_tree, cX, operators, Val(false)
        )
    elseif both_mode
        # features come first because we can use size(cX, 1) to skip them
        index_tree = index_constant_nodes(tree)
        eval_grad_tree_array(
            tree, n_gradients, index_tree, cX, operators, Val(:both)
        )
    end::ResultOk2

    return (result.x, result.dx, result.ok)
end

function eval_grad_tree_array(
    tree::AbstractExpressionNode{T},
    n_gradients,
    index_tree::Union{NodeIndex,Nothing},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    ::Val{mode},
)::ResultOk2 where {T<:Number,mode}
    result = _eval_grad_tree_array(tree, n_gradients, index_tree, cX, operators, Val(mode))
    !result.ok && return result
    return ResultOk2(
        result.x, result.dx, is_valid_array(result.x) && is_valid_array(result.dx)
    )
end

function eval_grad_tree_array(
    tree::AbstractExpressionNode{T1},
    cX::AbstractMatrix{T2},
    operators::OperatorEnum;
    variable::Union{Bool,Val}=Val(false),
    turbo::Union{Bool,Val}=Val(false),
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
    tree::AbstractExpressionNode{T,D},
    n_gradients,
    index_tree::Union{NodeIndex{<:Any,D},Nothing},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    ::Val{mode},
)::ResultOk2 where {T<:Number,D,mode}
    quote
        deg = tree.degree
        deg == 0 && return grad_deg0_eval(tree, n_gradients, index_tree, cX, Val(mode))
        Base.Cartesian.@nif(
            $D,
            i -> i == deg,  # COV_EXCL_LINE
            i -> begin  # COV_EXCL_LINE
                dispatch_grad_degn_eval(
                    tree, n_gradients, index_tree, cX, Val(i), operators, Val(mode)
                )
            end,
        )
    end
end

@generated function dispatch_grad_degn_eval(
    tree::AbstractExpressionNode{T},
    n_gradients,
    index_tree::Union{NodeIndex,Nothing},
    cX::AbstractMatrix{T},
    ::Val{degree},
    operators::OperatorEnum,
    ::Val{mode},
) where {T<:Number,degree,mode}
    setup = quote
        cs = get_children(tree, Val($degree))
        index_cs =
            isnothing(index_tree) ? index_tree : get_children(index_tree, Val($degree))
        Base.Cartesian.@nexprs(  # COV_EXCL_LINE
            $degree,
            i -> begin  # COV_EXCL_LINE
                result_i = eval_grad_tree_array(
                    cs[i],
                    n_gradients,
                    isnothing(index_cs) ? index_cs : index_cs[i],
                    cX,
                    operators,
                    Val(mode),
                )
                !result_i.ok && return result_i
            end
        )
        x_cumulators = Base.Cartesian.@ntuple($degree, i -> result_i.x)
        d_cumulators = Base.Cartesian.@ntuple($degree, i -> result_i.dx)
        op_idx = tree.op
    end
    nops = get_nops(operators, Val(degree))
    if nops > OPERATOR_LIMIT_BEFORE_SLOWDOWN
        quote
            $setup  # COV_EXCL_LINE
            grad_degn_eval(x_cumulators, d_cumulators, operators[$degree][op_idx])
        end
    else
        quote
            $setup  # COV_EXCL_LINE
            Base.Cartesian.@nif(
                $nops,
                i -> i == op_idx,  # COV_EXCL_LINE
                i -> begin  # COV_EXCL_LINE
                    grad_degn_eval(
                        x_cumulators,
                        d_cumulators,
                        get_op(operators, Val($degree), Val(i)),
                    )
                end,
            )
        end
    end
end

@generated function grad_degn_eval(
    x_cumulators::NTuple{N}, d_cumulators::NTuple{N}, op::F
) where {N,F}
    quote
        Base.Cartesian.@nexprs($N, i -> begin  # COV_EXCL_LINE
            x_cumulator_i = x_cumulators[i]
            d_cumulator_i = d_cumulators[i]
        end)
        diff_op = _zygote_gradient(op, Val($N))
        @finite @inbounds for j in eachindex(x_cumulator_1)
            x = Base.Cartesian.@ncall($N, op, i -> x_cumulator_i[j])
            Base.Cartesian.@ntuple($N, i -> grad_i) = Base.Cartesian.@ncall(
                $N, diff_op, i -> x_cumulator_i[j]
            )
            x_cumulator_1[j] = x
            @simd ivdep for k in axes(d_cumulator_1, 1)
                d_cumulator_1[k, j] = Base.Cartesian.@ncall(
                    $N,
                    +,
                    i -> grad_i * d_cumulator_i[k, j]  # COV_EXCL_LINE
                )
            end
        end  # COV_EXCL_LINE
        return ResultOk2(x_cumulator_1, d_cumulator_1, true)
    end
end

function grad_deg0_eval(
    tree::AbstractExpressionNode{T},
    n_gradients,
    index_tree::Union{NodeIndex,Nothing},
    cX::AbstractMatrix{T},
    ::Val{mode},
)::ResultOk2 where {T<:Number,mode}
    const_part = deg0_eval(tree, cX, EvalOptions()).x

    zero_mat = if isa(cX, Array)
        fill_similar(zero(T), cX, n_gradients, axes(cX, 2))
    else
        hcat([fill_similar(zero(T), cX, axes(cX, 2)) for _ in 1:n_gradients]...)'
    end

    if (mode isa Bool && mode == tree.constant)
        # No gradients at this leaf node
        return ResultOk2(const_part, zero_mat, true)
    end

    index = if (mode isa Bool && mode)
        tree.feature::UInt16
    elseif (mode isa Bool && !mode)
        isnothing(index_tree) && error("unexpected input. Please submit a bug report.")
        index_tree.val::UInt16
    elseif mode == :both
        index_tree::NodeIndex
        if tree.constant
            index_tree.val::UInt16 + UInt16(size(cX, 1))
        else
            tree.feature::UInt16
        end
    end

    derivative_part = zero_mat
    fill!(@view(derivative_part[index, :]), one(T))
    return ResultOk2(const_part, derivative_part, true)
end

end
