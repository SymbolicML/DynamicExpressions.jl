module DynamicExpressionsCUDAExt
#! format: off

using CUDA: threadIdx, blockIdx, @cuda, CuArray, cu
using DynamicExpressions: OperatorEnum, AbstractExpressionNode
using DynamicExpressions.EvaluateEquationModule: get_nbin, get_nuna
using DynamicExpressions.AsArrayModule: as_array


function eval_tree_gpu(
    tree::AbstractExpressionNode{T}, gcX::CuArray{T,2}, operators::OperatorEnum
)
    (; degree, constant, val, feature, op, execution_order, idx_self, idx_l, idx_r) = as_array(tree)
    num_launches = maximum(execution_order)
    num_elem = size(gcX, 2)
    num_nodes = length(degree)

    # Convert everything to GPU:
    gbuffer = CuArray{T}(undef, num_elem, num_nodes)
    gexecution_order = cu(execution_order)
    gdegree = cu(degree)
    gconstant = cu(constant)
    gval = cu(val)
    gfeature = cu(feature)
    gop = cu(op)
    gidx_self = cu(idx_self)
    gidx_l = cu(idx_l)
    gidx_r = cu(idx_r)

    _launch_gpu_kernel!(
        Val(nextpow(2, num_elem * num_nodes)), num_launches, gbuffer,
        # Thread info:
        num_elem, num_nodes, gexecution_order,
        # Input data and tree
        operators, gcX, gidx_self, gidx_l, gidx_r,
        gdegree, gconstant, gval, gfeature, gop,
    )

    out = gbuffer[:, 1]
    return (out, isfinite(sum(out .* zero(T))))
end

function _launch_gpu_kernel!(
    ::Val{num_threads}, num_launches::Integer, buffer::CuArray{T,2},
    # Thread info:
    num_elem::Integer, num_nodes::Integer, execution_order::CuArray{I},
    # Input data and tree
    operators::OperatorEnum, cX::CuArray{T,2}, idx_self::CuArray, idx_l::CuArray, idx_r::CuArray,
    degree::CuArray, constant::CuArray, val::CuArray{T,1}, feature::CuArray, op::CuArray,
) where {num_threads,I,T}
    for launch in I(1):I(num_launches)
        execute = execution_order .== launch
        @cuda(
            threads = num_threads,
            _gpu_kernel!(
                buffer,
                num_elem, num_nodes, execute,
                operators, cX, idx_self, idx_l, idx_r,
                degree, constant, val, feature, op
            )
        )
    end
end

@generated function _gpu_kernel!(
    # Storage:
    buffer::CuArray{T,2},
    # Thread info:
    num_elem::Integer, num_nodes::Integer, execute::CuArray{Bool},
    # Input data and tree
    operators::OperatorEnum, cX::CuArray{T,2}, idx_self::CuArray, idx_l::CuArray, idx_r::CuArray,
    degree::CuArray, constant::CuArray, val::CuArray{T,1}, feature::CuArray, op::CuArray,
) where {T}
    nbin = get_nbin(operators)
    nuna = get_nuna(operators)
    quote
        i = threadIdx().x
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

#! format: on
end
