module NodePreallocationModule

using ..NodeModule:
    AbstractExpressionNode,
    with_type_parameters,
    tree_mapreduce,
    leaf_copy,
    branch_copy,
    set_node!,
    set_children!

import BorrowChecker

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

"""
    copy_into!(dest::AbstractArray{N}, src::N) where {N<:AbstractExpressionNode}

Copy a node, recursively copying all children nodes, in-place to a preallocated container.
This should result in no extra allocations.
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
        N,
    )
end
BorrowChecker.@safe function leaf_copy_into!(dest::N, src::N) where {N<:AbstractExpressionNode}
    set_node!(dest, src)
    return dest
end
BorrowChecker.@safe function branch_copy_into!(
    dest::N, src::N, children::Vararg{Any,M}
) where {T,D,N<:AbstractExpressionNode{T,D},M}
    dest.degree = M
    dest.op = src.op
    set_children!(dest, children)
    return dest
end

end
