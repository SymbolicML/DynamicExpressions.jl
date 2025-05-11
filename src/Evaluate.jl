module EvaluateModule

using DispatchDoctor: @stable, @unstable

import ..NodeModule:
    AbstractExpressionNode, constructorof, max_degree, get_children, with_type_parameters
import ..StringsModule: string_tree
import ..OperatorEnumModule: OperatorEnum, GenericOperatorEnum
import ..UtilsModule: fill_similar, counttuple, ResultOk
import ..NodeUtilsModule: is_constant
import ..ExtensionInterfaceModule: bumper_eval_tree_array, _is_loopvectorization_loaded
import ..ValueInterfaceModule: is_valid, is_valid_array

const OPERATOR_LIMIT_BEFORE_SLOWDOWN = 15

macro return_on_nonfinite_val(eval_options, val, X)
    :(
        if $(esc(eval_options)).early_exit isa Val{true} && !is_valid($(esc(val)))
            return $(ResultOk)(
                get_array($(esc(eval_options)).buffer, $(esc(X)), axes($(esc(X)), 2)), false
            )
        end
    )
end

macro return_on_nonfinite_array(eval_options, array)
    :(
        if $(esc(eval_options)).early_exit isa Val{true} && !is_valid_array($(esc(array)))
            return $(ResultOk)($(esc(array)), false)
        end
    )
end

"""Buffer management for array allocations during evaluation."""
struct ArrayBuffer{A<:AbstractMatrix,R<:Base.RefValue{<:Integer}}
    array::A
    index::R
end

function Base.copy(buffer::ArrayBuffer)
    return ArrayBuffer(copy(buffer.array), Ref(buffer.index[]))
end

reset_index!(buffer::ArrayBuffer) = buffer.index[] = 0
reset_index!(::Nothing) = nothing

next_index!(buffer::ArrayBuffer) = buffer.index[] += 1

function get_array(::Nothing, template::AbstractArray, axes...)
    return similar(template, axes...)
end

function get_array(buffer::ArrayBuffer, template::AbstractArray, axes...)
    i = next_index!(buffer)
    out = @view(buffer.array[i, :])
    return out
end

function get_filled_array(::Nothing, value, template::AbstractArray, axes...)
    return fill_similar(value, template, axes...)
end
function get_filled_array(buffer::ArrayBuffer, value, template::AbstractArray, axes...)
    i = next_index!(buffer)
    @inbounds buffer.array[i, :] .= value
    return @view(buffer.array[i, :])
end

function get_feature_array(::Nothing, X::AbstractMatrix, feature::Integer)
    return @inbounds(X[feature, :])
end
function get_feature_array(buffer::ArrayBuffer, X::AbstractMatrix, feature::Integer)
    i = next_index!(buffer)
    @inbounds buffer.array[i, :] .= X[feature, :]
    return @view(buffer.array[i, :])
end

"""
    EvalOptions

This holds options for expression evaluation, such as evaluation backend.

# Fields

- `turbo::Val{T}=Val(false)`: If `Val{true}`, use LoopVectorization.jl for faster
    evaluation.
- `bumper::Val{B}=Val(false)`: If `Val{true}`, use Bumper.jl for faster evaluation.
- `early_exit::Val{E}=Val(true)`: If `Val{true}`, any element of any step becoming
    `NaN` or `Inf` will terminate the computation. For `eval_tree_array`, this will
    result in the second return value, the completion flag, being `false`. For 
    calling an expression using `tree(X)`, this will result in `NaN`s filling
    the entire buffer. This early exit is performed to avoid wasting compute cycles.
    Setting `Val{false}` will continue the computation as usual and thus result in
    `NaN`s only in the elements that actually have `NaN`s.
- `buffer::Union{ArrayBuffer,Nothing}`: If not `nothing`, use this buffer for evaluation.
    This should be an instance of `ArrayBuffer` which has an `array` field and an
    `index` field used to iterate which buffer slot to use.
"""
struct EvalOptions{T,B,E,BUF<:Union{ArrayBuffer,Nothing}}
    turbo::Val{T}
    bumper::Val{B}
    early_exit::Val{E}
    buffer::BUF
end

@unstable function EvalOptions(;
    turbo::Union{Bool,Val}=Val(false),
    bumper::Union{Bool,Val}=Val(false),
    early_exit::Union{Bool,Val}=Val(true),
    buffer::Union{ArrayBuffer,Nothing}=nothing,
)
    v_turbo = _to_bool_val(turbo)
    v_bumper = _to_bool_val(bumper)
    v_early_exit = _to_bool_val(early_exit)

    if v_bumper isa Val{true}
        @assert buffer === nothing
    end

    return EvalOptions(v_turbo, v_bumper, v_early_exit, buffer)
end

@unstable @inline _to_bool_val(x::Bool) = x ? Val(true) : Val(false)
@inline _to_bool_val(::Val{T}) where {T} = Val(T::Bool)

_copy(x) = copy(x)
_copy(::Nothing) = nothing
function Base.copy(eval_options::EvalOptions)
    return EvalOptions(;
        turbo=eval_options.turbo,
        bumper=eval_options.bumper,
        early_exit=eval_options.early_exit,
        buffer=_copy(eval_options.buffer),
    )
end

@unstable function _process_deprecated_kws(eval_options, deprecated_kws)
    turbo = get(deprecated_kws, :turbo, nothing)
    bumper = get(deprecated_kws, :bumper, nothing)
    if any(Base.Fix2(∉, (:turbo, :bumper)), keys(deprecated_kws))
        throw(ArgumentError("Invalid keyword argument(s): $(keys(deprecated_kws))"))
    end
    if !isempty(deprecated_kws)
        @assert(
            eval_options === nothing,
            "Cannot use both `eval_options` and deprecated flags `turbo` and `bumper`."
        )
        # TODO: We don't do a depwarn as it can GREATLY bottleneck the search speed.
    end
    if eval_options !== nothing
        return eval_options
    else
        return EvalOptions(;
            turbo=turbo === nothing ? Val(false) : turbo,
            bumper=bumper === nothing ? Val(false) : bumper,
        )
    end
end

"""
    eval_tree_array(
        tree::AbstractExpressionNode{T},
        cX::AbstractMatrix{T},
        operators::OperatorEnum;
        eval_options::Union{EvalOptions,Nothing}=nothing,
    ) where {T}

Evaluate a binary tree (equation) over a given input data matrix. The
operators contain all of the operators used. This function fuses doublets
and triplets of operations for lower memory usage.

# Arguments
- `tree::AbstractExpressionNode`: The root node of the tree to evaluate.
- `cX::AbstractMatrix{T}`: The input data to evaluate the tree on, with shape `[num_features, num_rows]`.
- `operators::OperatorEnum`: The operators used in the tree.
- `eval_options::Union{EvalOptions,Nothing}`: See [`EvalOptions`](@ref) for documentation
    on the different evaluation modes.


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
    eval_options::Union{EvalOptions,Nothing}=nothing,
    _deprecated_kws...,
) where {T}
    _eval_options = _process_deprecated_kws(eval_options, _deprecated_kws)
    if _eval_options.turbo isa Val{true} || _eval_options.bumper isa Val{true}
        @assert T in (Float32, Float64)
    end
    if _eval_options.turbo isa Val{true}
        _is_loopvectorization_loaded(0) ||
            error("Please load the LoopVectorization.jl package to use this feature.")
    end
    if (_eval_options.turbo isa Val{true} || _eval_options.bumper isa Val{true}) &&
        !(T <: Number)
        error(
            "Bumper and LoopVectorization features are only compatible with numeric element types",
        )
    end
    if _eval_options.bumper isa Val{true}
        return bumper_eval_tree_array(tree, cX, operators, _eval_options)
    end

    reset_index!(_eval_options.buffer)

    result = _eval_tree_array(tree, cX, operators, _eval_options)
    return (
        result.x,
        result.ok && (_eval_options.early_exit isa Val{false} || is_valid_array(result.x)),
    )
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
    kws...,
) where {T1,T2}
    T = promote_type(T1, T2)
    tree = convert(with_type_parameters(typeof(tree), T), tree)
    cX = Base.Fix1(convert, T).(cX)
    return eval_tree_array(tree, cX, operators; kws...)
end

# These are marked unstable due to issues discussed on
# https://github.com/JuliaLang/julia/issues/55147
@unstable function get_nuna(::Type{<:OperatorEnum{OPS}}) where {OPS}
    ts = OPS.types
    return isempty(ts) ? 0 : counttuple(ts[1])
end
@unstable function get_nbin(::Type{<:OperatorEnum{OPS}}) where {OPS}
    ts = OPS.types
    return length(ts) == 1 ? 0 : counttuple(ts[2])
end

function _eval_tree_array(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    eval_options::EvalOptions,
)::ResultOk where {T}
    # First, we see if there are only constants in the tree - meaning
    # we can just return the constant result.
    if tree.degree == 0
        return deg0_eval(tree, cX, eval_options)
    elseif is_constant(tree)
        # Speed hack for constant trees.
        const_result = dispatch_constant_tree(tree, operators)::ResultOk{T}
        !const_result.ok &&
            return ResultOk(get_array(eval_options.buffer, cX, axes(cX, 2)), false)
        return ResultOk(
            get_filled_array(eval_options.buffer, const_result.x, cX, axes(cX, 2)), true
        )
    elseif tree.degree == 1
        op_idx = tree.op
        return dispatch_deg1_eval(tree, cX, op_idx, operators, eval_options)
    elseif max_degree(tree) == 2 || tree.degree == 2
        # TODO - add op(op2(x, y), z) and op(x, op2(y, z))
        # op(x, y), where x, y are constants or variables.
        op_idx = tree.op
        return dispatch_deg2_eval(tree, cX, op_idx, operators, eval_options)
    else
        return dispatch_degn_eval(tree, cX, operators, eval_options)
    end
end

@generated function degn_eval(
    cumulators::NTuple{N,<:AbstractVector{T}}, op::F, ::EvalOptions{false}
)::ResultOk where {N,T,F}
    # Fast general implementation of `cumulators[1] .= op.(cumulators[1], cumulators[2], ...)`
    quote
        Base.Cartesian.@nexprs($N, i -> cumulator_i = cumulators[i])
        @inbounds @simd for j in eachindex(cumulator_1)
            cumulator_1[j] = Base.Cartesian.@ncall($N, op, i -> cumulator_i[j])::T
        end
        return ResultOk(cumulator_1, true)
    end
end

function deg2_eval(
    cumulator_l::AbstractVector{T},
    cumulator_r::AbstractVector{T},
    op::F,
    eval_options::EvalOptions,
)::ResultOk where {T,F}
    return degn_eval((cumulator_l, cumulator_r), op, eval_options)
end

function deg1_eval(
    cumulator::AbstractVector{T}, op::F, eval_options::EvalOptions
)::ResultOk where {T,F}
    return degn_eval((cumulator,), op, eval_options)
end

function deg0_eval(
    tree::AbstractExpressionNode{T}, cX::AbstractMatrix{T}, eval_options::EvalOptions
)::ResultOk where {T}
    if tree.constant
        return ResultOk(
            get_filled_array(eval_options.buffer, tree.val, cX, axes(cX, 2)), true
        )
    else
        return ResultOk(get_feature_array(eval_options.buffer, cX, tree.feature), true)
    end
end

@generated function inner_dispatch_degn_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    ::Val{degree},
    operators::OperatorEnum{OPS},
    eval_options::EvalOptions,
) where {T,degree,OPS}
    nops = length(OPS.types[degree].types)
    return quote
        cs = get_children(tree, Val($degree))
        Base.Cartesian.@nexprs(
            $degree,
            i -> begin
                result_i = _eval_tree_array(cs[i], cX, operators, eval_options)
                !result_i.ok && return result_i
                @return_on_nonfinite_array(eval_options, result_i.x)
            end
        )
        op_idx = tree.op
        cumulators = Base.Cartesian.@ntuple($degree, i -> result_i.x)
        Base.Cartesian.@nif(
            $nops,
            i -> i == op_idx,
            i -> degn_eval(cumulators, operators[$degree][i], eval_options),
        )
    end
end
@generated function dispatch_degn_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    eval_options::EvalOptions,
) where {T}
    D = max_degree(tree)
    return quote
        # If statement over degrees
        degree = tree.degree
        return Base.Cartesian.@nif(
            $D,
            d -> d == degree,
            d -> inner_dispatch_degn_eval(tree, cX, Val(d), operators, eval_options)
        )
    end
end
@generated function dispatch_deg2_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op_idx::Integer,
    operators::OperatorEnum,
    eval_options::EvalOptions,
) where {T}
    nbin = get_nbin(operators)
    long_compilation_time = nbin > OPERATOR_LIMIT_BEFORE_SLOWDOWN
    if long_compilation_time
        return quote
            result_l = _eval_tree_array(tree.l, cX, operators, eval_options)
            !result_l.ok && return result_l
            @return_on_nonfinite_array(eval_options, result_l.x)
            result_r = _eval_tree_array(tree.r, cX, operators, eval_options)
            !result_r.ok && return result_r
            @return_on_nonfinite_array(eval_options, result_r.x)
            # op(x, y), for any x or y
            deg2_eval(result_l.x, result_r.x, operators.binops[op_idx], eval_options)
        end
    end
    return quote
        return Base.Cartesian.@nif(
            $nbin,
            i -> i == op_idx,
            i -> let op = operators.binops[i]
                if tree.l.degree == 0 && tree.r.degree == 0
                    deg2_l0_r0_eval(tree, cX, op, eval_options)
                elseif tree.r.degree == 0
                    result_l = _eval_tree_array(tree.l, cX, operators, eval_options)
                    !result_l.ok && return result_l
                    @return_on_nonfinite_array(eval_options, result_l.x)
                    # op(x, y), where y is a constant or variable but x is not.
                    deg2_r0_eval(tree, result_l.x, cX, op, eval_options)
                elseif tree.l.degree == 0
                    result_r = _eval_tree_array(tree.r, cX, operators, eval_options)
                    !result_r.ok && return result_r
                    @return_on_nonfinite_array(eval_options, result_r.x)
                    # op(x, y), where x is a constant or variable but y is not.
                    deg2_l0_eval(tree, result_r.x, cX, op, eval_options)
                else
                    result_l = _eval_tree_array(tree.l, cX, operators, eval_options)
                    !result_l.ok && return result_l
                    @return_on_nonfinite_array(eval_options, result_l.x)
                    result_r = _eval_tree_array(tree.r, cX, operators, eval_options)
                    !result_r.ok && return result_r
                    @return_on_nonfinite_array(eval_options, result_r.x)
                    # op(x, y), for any x or y
                    deg2_eval(result_l.x, result_r.x, op, eval_options)
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
    eval_options::EvalOptions,
) where {T}
    nuna = get_nuna(operators)
    long_compilation_time = nuna > OPERATOR_LIMIT_BEFORE_SLOWDOWN
    if long_compilation_time
        return quote
            result = _eval_tree_array(tree.l, cX, operators, eval_options)
            !result.ok && return result
            @return_on_nonfinite_array(eval_options, result.x)
            deg1_eval(result.x, operators.unaops[op_idx], eval_options)
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
                        tree, cX, op, l_op_idx, operators.binops, eval_options
                    )
                elseif tree.l.degree == 1 && tree.l.l.degree == 0
                    # op(op2(x)), where x is a constant or variable.
                    l_op_idx = tree.l.op
                    dispatch_deg1_l1_ll0_eval(
                        tree, cX, op, l_op_idx, operators.unaops, eval_options
                    )
                else
                    # op(x), for any x.
                    result = _eval_tree_array(tree.l, cX, operators, eval_options)
                    !result.ok && return result
                    @return_on_nonfinite_array(eval_options, result.x)
                    deg1_eval(result.x, op, eval_options)
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
    eval_options::EvalOptions,
) where {T,F}
    nbin = counttuple(binops)
    # (Note this is only called from dispatch_deg1_eval, which has already
    # checked for long compilation times, so we don't need to check here)
    quote
        Base.Cartesian.@nif(
            $nbin,
            j -> j == l_op_idx,
            j -> let op_l = binops[j]
                deg1_l2_ll0_lr0_eval(tree, cX, op, op_l, eval_options)
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
    eval_options::EvalOptions,
)::ResultOk where {T,F}
    nuna = counttuple(unaops)
    quote
        Base.Cartesian.@nif(
            $nuna,
            j -> j == l_op_idx,
            j -> let op_l = unaops[j]
                deg1_l1_ll0_eval(tree, cX, op, op_l, eval_options)
            end,
        )
    end
end

function deg1_l2_ll0_lr0_eval(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op::F,
    op_l::F2,
    eval_options::EvalOptions{false,false},
) where {T,F,F2}
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
        @inbounds @simd for j in axes(cX, 2)
            x_l = op_l(val_ll, cX[feature_lr, j])::T
            x = is_valid(x_l) ? op(x_l)::T : T(Inf)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.l.r.constant
        feature_ll = tree.l.l.feature
        val_lr = tree.l.r.val
        @return_on_nonfinite_val(eval_options, val_lr, cX)
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        @inbounds @simd for j in axes(cX, 2)
            x_l = op_l(cX[feature_ll, j], val_lr)::T
            x = is_valid(x_l) ? op(x_l)::T : T(Inf)
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        feature_ll = tree.l.l.feature
        feature_lr = tree.l.r.feature
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
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
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op::F,
    op_l::F2,
    eval_options::EvalOptions{false,false},
) where {T,F,F2}
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
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    op::F,
    eval_options::EvalOptions{false,false},
) where {T,F}
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
        @inbounds @simd for j in axes(cX, 2)
            x = op(val_l, cX[feature_r, j])::T
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    elseif tree.r.constant
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
        feature_l = tree.l.feature
        val_r = tree.r.val
        @return_on_nonfinite_val(eval_options, val_r, cX)
        @inbounds @simd for j in axes(cX, 2)
            x = op(cX[feature_l, j], val_r)::T
            cumulator[j] = x
        end
        return ResultOk(cumulator, true)
    else
        cumulator = get_array(eval_options.buffer, cX, axes(cX, 2))
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
    eval_options::EvalOptions{false,false},
) where {T,F}
    if tree.l.constant
        val = tree.l.val
        @return_on_nonfinite_val(eval_options, val, cX)
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
    eval_options::EvalOptions{false,false},
) where {T,F}
    if tree.r.constant
        val = tree.r.val
        @return_on_nonfinite_val(eval_options, val, cX)
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
    tree::AbstractExpressionNode{T,D}, operators::OperatorEnum
) where {T,D}
    quote
        deg = tree.degree
        deg == 0 && return deg0_eval_constant(tree)
        Base.Cartesian.@nif(
            $D,
            i -> i == deg,
            i -> inner_dispatch_degn_eval_constant(tree, Val(i), operators)
        )
    end
end

# Now that we have the degree, we can get the operator
@generated function inner_dispatch_degn_eval_constant(
    tree::AbstractExpressionNode{T}, ::Val{degree}, operators::OperatorEnum{OPS}
) where {T,degree,OPS}
    nops = length(OPS.types[degree].types)
    get_inputs = quote
        cs = get_children(tree, Val($degree))
        Base.Cartesian.@nexprs(
            $degree,
            i -> begin
                input_i = let result = dispatch_constant_tree(cs[i], operators)
                    !result.ok && return result
                    result.x
                end
            end
        )
        inputs = Base.Cartesian.@ntuple($degree, i -> input_i)
    end
    if nops > OPERATOR_LIMIT_BEFORE_SLOWDOWN
        return quote
            $get_inputs
            op_idx = tree.op
            degn_eval_constant(inputs, operators[$degree][op_idx])::ResultOk{T}
        end
    else
        return quote
            $get_inputs
            op_idx = tree.op
            Base.Cartesian.@nif(
                $nops,
                i -> i == op_idx,
                i -> degn_eval_constant(inputs, operators[$degree][i])::ResultOk{T}
            )
        end
    end
end

@inline function deg0_eval_constant(tree::AbstractExpressionNode{T}) where {T}
    output = tree.val
    return ResultOk(output, is_valid(output))::ResultOk{T}
end

function degn_eval_constant(inputs::Tuple{T,Vararg{T}}, op::F) where {T,F}
    output = op(inputs...)::T
    return ResultOk(output, is_valid(output))::ResultOk{T}
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
    tree::AbstractExpressionNode{T1,D}, cX::AbstractMatrix{T}, operators::OperatorEnum
)::ResultOk where {T<:Number,T1,D}
    quote
        tree.degree == 0 && return deg0_diff_eval(tree, cX, operators)
        op_idx = tree.op
        deg = tree.degree
        Base.Cartesian.@nif(
            $D,
            i -> i == deg,
            i -> dispatch_degn_diff_eval(tree, cX, op_idx, Val(i), operators)
        )
    end
end

function deg0_diff_eval(
    tree::AbstractExpressionNode{T1}, cX::AbstractMatrix{T}, operators::OperatorEnum
)::ResultOk where {T<:Number,T1}
    if tree.constant
        ResultOk(fill_similar(one(T), cX, axes(cX, 2)) .* tree.val, true)
    else
        ResultOk(cX[tree.feature, :], true)
    end
end

function degn_diff_eval(cumulators::C, op::F) where {C<:Tuple,F}
    out = op.(cumulators...)
    return ResultOk(out, all(isfinite, out))
end

@generated function dispatch_degn_diff_eval(
    tree::AbstractExpressionNode{T1,D},
    cX::AbstractMatrix{T},
    op_idx::Integer,
    ::Val{degree},
    operators::OperatorEnum{OPS},
) where {T<:Number,T1,D,degree,OPS}
    nops = length(OPS.types[degree].types)
    quote
        cs = get_children(tree, Val($degree))
        Base.Cartesian.@nexprs(
            $degree,
            i -> begin
                cumulator_i =
                    let result = _differentiable_eval_tree_array(cs[i], cX, operators)
                        !result.ok && return result
                        result.x
                    end
            end
        )
        cumulators = Base.Cartesian.@ntuple($degree, i -> cumulator_i)
        Base.Cartesian.@nif(
            $nops, i -> i == op_idx, i -> degn_diff_eval(cumulators, operators[$degree][i])
        )
    end
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
    throw_errors::Union{Val,Bool}=Val(true),
) where {T1,T2,N}
    v_throw_errors = _to_bool_val(throw_errors)
    try
        return _eval_tree_array_generic(tree, cX, operators, v_throw_errors)
    catch e
        if v_throw_errors isa Val{false}
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

@generated function _eval_tree_array_generic(
    tree::AbstractExpressionNode{T1,D},
    cX::AbstractArray{T2,N},
    operators::GenericOperatorEnum,
    ::Val{throw_errors},
) where {T1,D,T2,N,throw_errors}
    quote
        tree.degree == 0 && return deg0_eval_generic(tree, cX)
        op_idx = tree.op
        deg = tree.degree
        Base.Cartesian.@nif(
            $D,
            i -> i == deg,
            i -> dispatch_degn_eval_generic(
                tree, cX, op_idx, Val(i), operators, Val(throw_errors)
            )
        )
    end
end

@unstable function deg0_eval_generic(
    tree::AbstractExpressionNode{T1}, cX::AbstractArray{T2,N}
) where {T1,T2,N}
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
            return copy(selectdim(cX, 1, tree.feature)), true
        end
    end
end

@unstable function degn_eval_generic(
    cumulators::C, op::F, ::Val{N}, ::Val{throw_errors}
) where {C<:Tuple,F,N,throw_errors}
    if !throw_errors
        input_type = N == 1 ? C : Tuple{map(eltype, cumulators)...}
        !hasmethod(op, input_type) && return nothing, false
    end
    if N == 1
        return op(cumulators...), true
    else
        return op.(cumulators...), true
    end
end

@generated function dispatch_degn_eval_generic(
    tree::AbstractExpressionNode{T1},
    cX::AbstractArray{T2,N},
    op_idx::Integer,
    ::Val{degree},
    operators::GenericOperatorEnum{OPS},
    ::Val{throw_errors},
) where {T1,T2,N,degree,throw_errors,OPS}
    nops = length(OPS.types[degree].types)
    quote
        cs = get_children(tree, Val($degree))
        Base.Cartesian.@nexprs(
            $degree,
            i -> begin
                cumulator_i =
                    let (x, complete) = _eval_tree_array_generic(
                            cs[i], cX, operators, Val(throw_errors)
                        )
                        !throw_errors && !complete && return nothing, false
                        x
                    end
            end
        )
        cumulators = Base.Cartesian.@ntuple($degree, i -> cumulator_i)
        Base.Cartesian.@nif(
            $nops,
            i -> i == op_idx,
            i -> degn_eval_generic(
                cumulators, operators[$degree][i], Val(N), Val(throw_errors)
            )
        )
    end
end

end
