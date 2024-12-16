module DynamicExpressionsCUDAExt

# TODO: Switch to KernelAbstractions.jl (once they hit v1.0)
using CUDA: @cuda, CuArray, blockDim, blockIdx, threadIdx
using DynamicExpressions: OperatorEnum, AbstractExpressionNode
using DynamicExpressions.EvaluateModule: get_nbin, get_nuna
using DynamicExpressions.AsArrayModule:
    as_array,
    IDX_DEGREE,
    IDX_FEATURE,
    IDX_OP,
    IDX_EXECUTION_ORDER,
    IDX_SELF,
    IDX_L,
    IDX_R,
    IDX_CONSTANT
using DispatchDoctor: @stable

import DynamicExpressions.EvaluateModule: eval_tree_array

# array type for exclusively testing purposes
struct FakeCuArray{T,N,A<:AbstractArray{T,N}} <: AbstractArray{T,N}
    a::A
end
Base.similar(x::FakeCuArray, dims::Integer...) = FakeCuArray(similar(x.a, dims...))
Base.getindex(x::FakeCuArray, i::Int...) = getindex(x.a, i...)
Base.setindex!(x::FakeCuArray, v, i::Int...) = setindex!(x.a, v, i...)
Base.size(x::FakeCuArray) = size(x.a)

const MaybeCuArray{T,N} = Union{CuArray{T,N},FakeCuArray{T,N}}

@stable default_mode = "disable" begin
    to_device(a, ::CuArray) = CuArray(a)
    to_device(a, ::FakeCuArray) = FakeCuArray(a)
end

@stable default_mode = "disable" function eval_tree_array(
    tree::AbstractExpressionNode{T}, gcX::MaybeCuArray{T,2}, operators::OperatorEnum; kws...
) where {T<:Number}
    (outs, is_good) = eval_tree_array((tree,), gcX, operators; kws...)
    return (only(outs), only(is_good))
end

@stable default_mode = "disable" function eval_tree_array(
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
    local val
    if _update_buffers
        (; val, roots, buffer, num_nodes, num_launches) = as_array(Int32, trees; buffer)
    end
    # TODO: Fix this type instability?
    num_elem = size(gcX, 2)

    num_launches = num_launches isa Integer ? num_launches : num_launches[]

    ## The following array is our "workspace" for
    ## the GPU kernel, with size equal to the number of rows
    ## in the input data by the number of nodes in the tree.
    ## It has one extra row to store the constant values.
    gworkspace = @something(gpu_workspace, similar(gcX, num_elem + 1, num_nodes))
    if _update_buffers
        copyto!(@view(gworkspace[end, :]), val)
    end
    val_idx = size(gworkspace, 1)

    gbuffer = if !_update_buffers
        gpu_buffer
    elseif gpu_buffer === nothing
        to_device(buffer, gcX)
    else
        copyto!(gpu_buffer, buffer)
    end

    # Removed @view definitions of gdegree, gfeature, etc.
    # We'll index directly into gbuffer using the constants above.

    num_threads = 256
    num_blocks = nextpow(2, ceil(Int, num_elem * num_nodes / num_threads))

    #! format: off
    _launch_gpu_kernel!(
        num_threads, num_blocks, num_launches, gworkspace,
        # Thread info:
        num_elem, num_nodes,
        # We'll pass gbuffer directly to the kernel now:
        operators, gcX, gbuffer, val_idx,
    )
    #! format: on

    out = map(r -> @view(gworkspace[begin:(end - 1), r]), roots)
    is_good = map(Returns(true), trees)

    return (out, is_good)
end

#! format: off
@stable default_mode = "disable" function _launch_gpu_kernel!(
    num_threads, num_blocks, num_launches::Integer, buffer::AbstractArray{T,2},
    # Thread info:
    num_elem::Integer, num_nodes::Integer,
    operators::OperatorEnum, cX::AbstractArray{T,2}, gbuffer::AbstractArray{Int32,2},
    val_idx::Integer
) where {T}
    #! format: on
    nuna = get_nuna(typeof(operators))
    nbin = get_nbin(typeof(operators))
    (nuna > 10 || nbin > 10) &&
        error("Too many operators. Kernels are only compiled up to 10.")
    gpu_kernel! = create_gpu_kernel(operators, Val(nuna), Val(nbin))
    for launch in one(Int32):Int32(num_launches)
        #! format: off
        if buffer isa CuArray
            @cuda threads=num_threads blocks=num_blocks gpu_kernel!(
                buffer,
                launch, num_elem, num_nodes,
                cX, gbuffer, val_idx
            )
        else
            Threads.@threads for i in 1:(num_threads * num_blocks)
                gpu_kernel!(
                    buffer,
                    launch, num_elem, num_nodes,
                    cX, gbuffer, val_idx, i
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
            buffer,
            launch::Integer, num_elem::Integer, num_nodes::Integer,
            cX::AbstractArray, gbuffer::AbstractArray{Int32,2},
            val_idx::Integer,
            i=nothing,
        )
            i = @something(i, (blockIdx().x - 1) * blockDim().x + threadIdx().x)
            if i > num_elem * num_nodes
                return nothing
            end

            node = (i - 1) % num_nodes + 1
            elem = (i - node) ÷ num_nodes + 1


            @inbounds begin
            if gbuffer[IDX_EXECUTION_ORDER, node] != launch
                return nothing
            end

            # Use constants to index gbuffer:
            cur_degree = gbuffer[IDX_DEGREE, node]
            cur_idx = gbuffer[IDX_SELF, node]

            if cur_degree == 0
                if gbuffer[IDX_CONSTANT, node] == 1
                    cur_val = buffer[val_idx, node]
                    buffer[elem, cur_idx] = cur_val
                else
                    cur_feature = gbuffer[IDX_FEATURE, node]
                    buffer[elem, cur_idx] = cX[cur_feature, elem]
                end
            else
                if cur_degree == 1 && $nuna > 0
                    cur_op = gbuffer[IDX_OP, node]
                    l_idx = gbuffer[IDX_L, node]
                    Base.Cartesian.@nif(
                        $nuna,
                        i -> i == cur_op,
                        i -> let op = operators.unaops[i]
                            buffer[elem, cur_idx] = op(buffer[elem, l_idx])
                        end
                    )
                elseif $nbin > 0
                    cur_op = gbuffer[IDX_OP, node]
                    l_idx = gbuffer[IDX_L, node]
                    r_idx = gbuffer[IDX_R, node]
                    Base.Cartesian.@nif(
                        $nbin,
                        i -> i == cur_op,
                        i -> let op = operators.binops[i]
                            buffer[elem, cur_idx] = op(buffer[elem, l_idx], buffer[elem, r_idx])
                        end
                    )
                end
            end
            end
            #! format: on
            return nothing
        end
    end
end

end
