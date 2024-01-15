module DynamicExpressionsCUDAExt
#! format: off

using CUDA
using DynamicExpressions: OperatorEnum, AbstractExpressionNode
using DynamicExpressions.EvaluateEquationModule: get_nbin, get_nuna
using DynamicExpressions.AsArrayModule: as_array

import DynamicExpressions.EvaluateEquationModule: eval_tree_array

function eval_tree_array(
    tree::AbstractExpressionNode{T}, gcX::CuArray{T,2}, operators::OperatorEnum; _...
) where {T<:Number}
    (outs, is_good) = eval_tree_array((tree,), gcX, operators)
    return only(outs), only(is_good)
end

function eval_tree_array(
    trees::NTuple{M,N}, gcX::CuArray{T,2}, operators::OperatorEnum; _...
) where {T<:Number,N<:AbstractExpressionNode{T},M}
    (; degree, constant, val, feature, op, execution_order, idx_self, idx_l, idx_r, roots) = as_array(trees...; index_type=Int32)
    num_launches = maximum(execution_order)
    num_elem = size(gcX, 2)
    num_nodes = length(degree)

    # Convert everything to GPU:
    gbuffer = CuArray{T}(undef, num_elem, num_nodes)
    gexecution_order = cu(execution_order)
    gdegree = cu(degree)
    gconstant = cu(constant)
    gval = CuArray(val)
    gfeature = cu(feature)
    gop = cu(op)
    gidx_self = cu(idx_self)
    gidx_l = cu(idx_l)
    gidx_r = cu(idx_r)

    groots = cu(roots)  # Just for indexing output

    num_threads = 256
    num_blocks = nextpow(2, ceil(Int, num_elem * num_nodes / num_threads))

    _launch_gpu_kernel!(
        num_blocks, num_launches, gbuffer,
        # Thread info:
        num_elem, num_nodes, gexecution_order,
        # Input data and tree
        operators, gcX, gidx_self, gidx_l, gidx_r,
        gdegree, gconstant, gval, gfeature, gop,
    )

    out = ntuple(i -> gbuffer[:, groots[i]], Val(M))
    is_good = ntuple(i -> isfinite(sum(out[i] .* zero(T))), Val(M))
    return out, is_good
end

function _launch_gpu_kernel!(
    num_blocks, num_launches::Integer, buffer::CuArray{T,2},
    # Thread info:
    num_elem::Integer, num_nodes::Integer, execution_order::CuArray{I},
    # Input data and tree
    operators::OperatorEnum, cX::CuArray{T,2}, idx_self::CuArray, idx_l::CuArray, idx_r::CuArray,
    degree::CuArray, constant::CuArray, val::CuArray{T,1}, feature::CuArray, op::CuArray,
) where {I,T}
    nuna = get_nuna(typeof(operators))
    nbin = get_nbin(typeof(operators))
    (nuna > 10 || nbin > 10) && error("Too many operators. Kernels are only compiled up to 10.")
    gpu_kernel! = create_gpu_kernel(operators, Val(nuna), Val(nbin))
    for launch in I.(1:num_launches)
        CUDA.@sync begin
            execute = execution_order .== launch
            @cuda threads=256 blocks=num_blocks gpu_kernel!(
                buffer,
                num_elem, num_nodes, execute,
                cX, idx_self, idx_l, idx_r,
                degree, constant, val, feature, op
            )
        end
    end
    return nothing
end

# Need to pre-compute the GPU kernels with an `@eval` for each number of operators
#   1. We need to use an `@nif` over operators, as GPU kernels
#      can't index into arrays of operators.
#   2. `@nif` is evaluated at parse time and needs to know the number of
#      ifs to generate at that time, so we can't simply use specialization.
#   3. We can't use `@generated` because we can't create closures in those.
for nuna in 0:10, nbin in 0:10
    @eval function create_gpu_kernel(operators::OperatorEnum, ::Val{$nuna}, ::Val{$nbin})
        function (
            # Storage:
            buffer,
            # Thread info:
            num_elem::Integer, num_nodes::Integer, execute::AbstractArray,
            # Input data and tree
            cX::AbstractArray, idx_self::AbstractArray, idx_l::AbstractArray, idx_r::AbstractArray,
            degree::AbstractArray, constant::AbstractArray, val::AbstractArray, feature::AbstractArray, op::AbstractArray,
        )
            i = (blockIdx().x - 1) * blockDim().x + threadIdx().x
            if i > num_elem * num_nodes
                return nothing
            end

            node = (i - 1) % num_nodes + 1
            elem = (i - node) ÷ num_nodes + 1

            if !execute[node]
                return nothing
            end

            cur_degree = degree[node]
            cur_idx = idx_self[node]
            if cur_degree == 0
                if constant[node]
                    cur_val = val[node]
                    buffer[elem, cur_idx] = cur_val
                else
                    cur_feature = feature[node]
                    buffer[elem, cur_idx] = cX[cur_feature, elem]
                end
            else
                if cur_degree == 1
                    cur_op = op[node]
                    l_idx = idx_l[node]
                    Base.Cartesian.@nif(
                        $nuna,
                        i -> i == cur_op,
                        i -> let op = operators.unaops[i]
                            buffer[elem, cur_idx] = op(buffer[elem, l_idx])
                        end
                    )
                else
                    cur_op = op[node]
                    l_idx = idx_l[node]
                    r_idx = idx_r[node]
                    Base.Cartesian.@nif(
                        $nbin,
                        i -> i == cur_op,
                        i -> let op = operators.binops[i]
                            buffer[elem, cur_idx] = op(buffer[elem, l_idx], buffer[elem, r_idx])
                        end
                    )
                end
            end
            return nothing
        end
    end
end

#! format: on
end
