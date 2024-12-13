module NodePreallocationModule

using ..NodeModule:
    AbstractExpressionNode,
    with_type_parameters,
    tree_mapreduce,
    leaf_copy,
    branch_copy,
    set_node!

"""
    copy_into!(dest::AbstractArray{N}, src::N) where {BS,N<:AbstractExpressionNode}

Copy a node, recursively copying all children nodes, in-place to an
array of pre-allocated nodes. This should result in no extra allocations.
"""
function copy_into!(
    dest::AbstractArray{N}, src::N; ref::Union{Nothing,Base.RefValue{<:Integer}}=nothing
) where {N<:AbstractExpressionNode}
    _ref = if ref === nothing
        Ref(0)
    else
        ref.x = 0
        ref
    end
    return tree_mapreduce(
        leaf -> leaf_copy_into!(@inbounds(dest[_ref.x += 1]), leaf),
        identity,
        ((p, c::Vararg{Any,M}) where {M}) ->
            branch_copy_into!(@inbounds(dest[_ref.x += 1]), p, c...),
        src,
        N;
        break_sharing=Val(BS),
    )
end
function leaf_copy_into!(dest::N, src::N) where {N<:AbstractExpressionNode}
    set_node!(dest, src)
    return dest
end
function branch_copy_into!(
    dest::N, src::N, children::Vararg{N,M}
) where {N<:AbstractExpressionNode,M}
    dest.degree = M
    dest.op = src.op
    dest.l = children[1]
    if M == 2
        dest.r = children[2]
    end
    return dest
end

"""
    allocate_container(prototype::AbstractExpressionNode, n=nothing)

Preallocate an array of `n` empty nodes matching the type of `prototype`.
If `n` is not provided, it will be computed from `length(prototype)`.

A given return value of this will be passed to `copy_into!` as the first argument,
so it should be compatible.
"""
function allocate_container(
    prototype::N, n::Union{Nothing,Integer}=nothing
) where {T,N<:AbstractExpressionNode{T}}
    num_nodes = @something(n, length(prototype))
    return N[with_type_parameters(N, T)() for _ in 1:num_nodes]
end

end
