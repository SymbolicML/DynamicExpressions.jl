module DynamicExpressionsCUDAExt

# TODO: Switch to KernelAbstractions.jl (once they hit v1.0)
using CUDA: @cuda, CuArray, blockDim, blockIdx, threadIdx
using DynamicExpressions: OperatorEnum, AbstractExpressionNode
using DynamicExpressions.EvaluateModule: get_nops
using DynamicExpressions.AsArrayModule:
    as_array, IDX_DEGREE, IDX_FEATURE, IDX_OP, IDX_L, IDX_R, IDX_CONSTANT
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
    gpu_tree_starts=nothing,
    roots=nothing,
    num_nodes=nothing,
    update_buffers::Val{_update_buffers}=Val(true),
    kws...,
) where {T<:Number,N<:AbstractExpressionNode{T},_update_buffers}
    local val
    if _update_buffers
        (; val, roots, buffer, num_nodes) = as_array(Int32, trees; buffer)
    end
    # TODO: Fix this type instability?
    num_elem = size(gcX, 2)

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

    num_trees = length(roots)
    gtree_starts = if !_update_buffers
        gpu_tree_starts
    else
        tree_starts = Int32[roots..., num_nodes + 1]
        if gpu_tree_starts === nothing
            to_device(tree_starts, gcX)
        else
            copyto!(gpu_tree_starts, tree_starts)
        end
    end

    num_threads = 256
    num_blocks = cld(num_elem * num_trees, num_threads)

    #! format: off
    _launch_gpu_kernel!(
        num_threads, num_blocks, gworkspace,
        # Thread info:
        num_elem, num_trees,
        operators, gcX, gbuffer, val_idx, gtree_starts,
    )
    #! format: on

    out = map(r -> @view(gworkspace[begin:(end - 1), r]), roots)
    is_good = map(Returns(true), trees)

    return (out, is_good)
end

#! format: off
@stable default_mode = "disable" function _launch_gpu_kernel!(
    num_threads, num_blocks, buffer::AbstractArray{T,2},
    # Thread info:
    num_elem::Integer, num_trees::Integer,
    operators::OperatorEnum, cX::AbstractArray{T,2}, gbuffer::AbstractArray{Int32,2},
    val_idx::Integer, tree_starts::AbstractArray{Int32,1}
) where {T}
    #! format: on
    nuna = get_nops(typeof(operators), Val(1))
    nbin = get_nops(typeof(operators), Val(2))
    (nuna > 10 || nbin > 10) &&
        error("Too many operators. Kernels are only compiled up to 10.")
    gpu_kernel!, worker! = create_gpu_kernel(operators, Val(nuna), Val(nbin))
    #! format: off
    if buffer isa CuArray
        @cuda threads=num_threads blocks=num_blocks gpu_kernel!(
            buffer,
            num_elem, num_trees,
            cX, gbuffer, val_idx, tree_starts
        )
    else
        Threads.@threads for i in 1:(num_elem * num_trees)
            worker!(
                buffer,
                num_elem, num_trees,
                cX, gbuffer, val_idx, tree_starts, i
            )
        end
    end
    #! format: on
    return nothing
end

# Need to pre-compute the GPU kernels with an `@eval` for each number of operators
#   1. We need to use an `@nif` over operators, as GPU kernels
#      can't index into arrays of operators.
#   2. `@nif` is evaluated at parse time and needs to know the number of
#      ifs to generate at that time, so we can't simply use specialization.
#   3. We can't use `@generated` because we can't create closures in those.
for nuna in 0:10, nbin in 0:10
    kernel_body = quote
        i > num_elem * num_trees && return nothing
        elem = (i - 1) % num_elem + 1
        tree = (i - elem) ÷ num_elem + 1

        @inbounds begin
            # `as_array` assigns indices in preorder with sharing broken, so every
            # child sits at a higher index than its parent and each tree fills the
            # contiguous block [tree_starts[tree], tree_starts[tree + 1]). Walking
            # the block downward therefore evaluates children before parents.
            for node in (tree_starts[tree + 1] - one(Int32)):-1:tree_starts[tree]
                cur_degree = gbuffer[IDX_DEGREE, node]

                if cur_degree == 0
                    if gbuffer[IDX_CONSTANT, node] == 1
                        cur_val = buffer[val_idx, node]
                        buffer[elem, node] = cur_val
                    else
                        cur_feature = gbuffer[IDX_FEATURE, node]
                        buffer[elem, node] = cX[cur_feature, elem]
                    end
                else
                    if cur_degree == 1 && $nuna > 0
                        cur_op = gbuffer[IDX_OP, node]
                        l_idx = gbuffer[IDX_L, node]
                        Base.Cartesian.@nif(
                            $nuna,
                            i -> i == cur_op,
                            i -> let op = operators.unaops[i]
                                buffer[elem, node] = op(buffer[elem, l_idx])
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
                                buffer[elem, node] = op(
                                    buffer[elem, l_idx], buffer[elem, r_idx]
                                )
                            end
                        )
                    end
                end
            end
        end
        return nothing
    end
    @eval function create_gpu_kernel(operators::OperatorEnum, ::Val{$nuna}, ::Val{$nbin})
        #! format: off
        worker! = function (
            buffer,
            num_elem::Integer, num_trees::Integer,
            cX::AbstractArray, gbuffer::AbstractArray{Int32,2},
            val_idx::Integer, tree_starts::AbstractArray{Int32,1},
            i::Integer,
        )
            $(kernel_body)
        end
        gpu_kernel! = function (
            buffer,
            num_elem::Integer, num_trees::Integer,
            cX::AbstractArray, gbuffer::AbstractArray{Int32,2},
            val_idx::Integer, tree_starts::AbstractArray{Int32,1},
        )
            i = (blockIdx().x - 1) * blockDim().x + threadIdx().x
            $(kernel_body)
        end
        #! format: on
        return gpu_kernel!, worker!
    end
end

end
