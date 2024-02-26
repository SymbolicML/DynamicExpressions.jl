module DynamicExpressionsCUDAExt

# TODO: Switch to KernelAbstractions.jl (once they hit v1.0)
using CUDA: @cuda, CuArray, blockDim, blockIdx, threadIdx
using DynamicExpressions: OperatorEnum, AbstractExpressionNode
using DynamicExpressions.EvaluateEquationModule: get_nbin, get_nuna
using DynamicExpressions.AsArrayModule: as_array

import DynamicExpressions.EvaluateEquationModule: eval_tree_array

# array type for exclusively testing purposes
struct FakeCuArray{T,N,A<:AbstractArray{T,N}} <: AbstractArray{T,N}
    a::A
end
Base.similar(x::FakeCuArray, dims::Integer...) = FakeCuArray(similar(x.a, dims...))
Base.getindex(x::FakeCuArray, i::Int...) = getindex(x.a, i...)
Base.setindex!(x::FakeCuArray, v, i::Int...) = setindex!(x.a, v, i...)
Base.size(x::FakeCuArray) = size(x.a)

const MaybeCuArray{T,N} = Union{CuArray{T,N},FakeCuArray{T,N}}

to_device(a, ::CuArray) = CuArray(a)
to_device(a, ::FakeCuArray) = FakeCuArray(a)

function eval_tree_array(
    tree::AbstractExpressionNode{T}, gcX::MaybeCuArray{T,2}, operators::OperatorEnum; kws...
) where {T<:Number}
    (outs, is_good) = eval_tree_array((tree,), gcX, operators; kws...)
    return (only(outs), only(is_good))
end

function eval_tree_array(
    trees::Union{Tuple{N,Vararg{N}},AbstractVector{N}},
    gcX::MaybeCuArray{T,2},
    operators::OperatorEnum;
    buffer=nothing,
    gpu_workspace=nothing,
    gpu_buffer=nothing,
    roots=nothing,
    num_nodes=nothing,
    num_launches=nothing,
    update_buffers::Val{_update_buffers}=Val(true),
    kws...,
) where {T<:Number,N<:AbstractExpressionNode{T},_update_buffers}
    if _update_buffers
        (; val, roots, buffer, num_nodes, num_launches) = as_array(Int32, trees; buffer)
    end
    num_elem = size(gcX, 2)

    ## The following array is our "workspace" for
    ## the GPU kernel, with size equal to the number of rows
    ## in the input data by the number of nodes in the tree.
    ## It has one extra row to store the constant values.
    gworkspace = if gpu_workspace === nothing
        similar(gcX, num_elem + 1, num_nodes)
    else
        gpu_workspace
    end
    gval = @view gworkspace[end, :]
    if _update_buffers
        copyto!(gval, val)
    end

    ## Index arrays (much faster to have `@view` here)
    gbuffer = if !_update_buffers
        gpu_buffer
    elseif gpu_buffer === nothing
        to_device(buffer, gcX)
    else
        copyto!(gpu_buffer, buffer)
    end
    gdegree = @view gbuffer[1, :]
    gfeature = @view gbuffer[2, :]
    gop = @view gbuffer[3, :]
    gexecution_order = @view gbuffer[4, :]
    gidx_self = @view gbuffer[5, :]
    gidx_l = @view gbuffer[6, :]
    gidx_r = @view gbuffer[7, :]
    gconstant = @view gbuffer[8, :]

    num_threads = 256
    num_blocks = nextpow(2, ceil(Int, num_elem * num_nodes / num_threads))

    #! format: off
    _launch_gpu_kernel!(
        num_threads, num_blocks, num_launches, gworkspace,
        # Thread info:
        num_elem, num_nodes, gexecution_order,
        # Input data and tree
        operators, gcX, gidx_self, gidx_l, gidx_r,
        gdegree, gconstant, gval, gfeature, gop,
    )
    #! format: on

    out = (r -> @view(gworkspace[begin:(end - 1), r])).(roots)
    is_good = (_ -> true).(trees)

    return (out, is_good)
end

#! format: off
function _launch_gpu_kernel!(
    num_threads, num_blocks, num_launches::Integer, buffer::AbstractArray{T,2},
    # Thread info:
    num_elem::Integer, num_nodes::Integer, execution_order::AbstractArray{I},
    # Input data and tree
    operators::OperatorEnum, cX::AbstractArray{T,2}, idx_self::AbstractArray, idx_l::AbstractArray, idx_r::AbstractArray,
    degree::AbstractArray, constant::AbstractArray, val::AbstractArray{T,1}, feature::AbstractArray, op::AbstractArray,
) where {I,T}
    #! format: on
    nuna = get_nuna(typeof(operators))
    nbin = get_nbin(typeof(operators))
    (nuna > 10 || nbin > 10) &&
        error("Too many operators. Kernels are only compiled up to 10.")
    gpu_kernel! = create_gpu_kernel(operators, Val(nuna), Val(nbin))
    for launch in one(I):I(num_launches)
        #! format: off
        if buffer isa CuArray
            @cuda threads=num_threads blocks=num_blocks gpu_kernel!(
                buffer,
                launch, num_elem, num_nodes, execution_order,
                cX, idx_self, idx_l, idx_r,
                degree, constant, val, feature, op
            )
        else
            Threads.@threads for i in 1:(num_threads * num_blocks)
                gpu_kernel!(
                    buffer,
                    launch, num_elem, num_nodes, execution_order,
                    cX, idx_self, idx_l, idx_r,
                    degree, constant, val, feature, op,
                    i
                )
            end
        end
        #! format: on
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
        #! format: off
        function (
            # Storage:
            buffer,
            # Thread info:
            launch::Integer, num_elem::Integer, num_nodes::Integer, execution_order::AbstractArray,
            # Input data and tree
            cX::AbstractArray, idx_self::AbstractArray, idx_l::AbstractArray, idx_r::AbstractArray,
            degree::AbstractArray, constant::AbstractArray, val::AbstractArray, feature::AbstractArray, op::AbstractArray,
            # Override for unittesting:
            i=nothing,
        )
            #! format: on
            i = i === nothing ? (blockIdx().x - 1) * blockDim().x + threadIdx().x : i
            if i > num_elem * num_nodes
                return nothing
            end

            node = (i - 1) % num_nodes + 1
            elem = (i - node) ÷ num_nodes + 1

            if execution_order[node] != launch
                return nothing
            end

            cur_degree = degree[node]
            cur_idx = idx_self[node]
            if cur_degree == 0
                if constant[node] == 1
                    cur_val = val[node]
                    buffer[elem, cur_idx] = cur_val
                else
                    cur_feature = feature[node]
                    buffer[elem, cur_idx] = cX[cur_feature, elem]
                end
            else
                if cur_degree == 1 && $nuna > 0
                    cur_op = op[node]
                    l_idx = idx_l[node]
                    Base.Cartesian.@nif(
                        $nuna,
                        i -> i == cur_op,
                        i -> let op = operators.unaops[i]
                            buffer[elem, cur_idx] = op(buffer[elem, l_idx])
                        end
                    )
                elseif $nbin > 0  # Note this check is to avoid type inference issues when binops is empty
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

end
