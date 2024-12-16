module AsArrayModule

using Compat: Fix

using ..NodeModule: AbstractExpressionNode, tree_mapreduce, count_nodes
using ..EvaluateModule: ArrayBuffer, get_array, get_filled_array

function as_array(
    ::Type{I}, trees::N; buffer::Union{ArrayBuffer,Nothing}=nothing
) where {T,N<:AbstractExpressionNode{T},I}
    return as_array(I, (trees,); buffer=buffer)
end

Base.@kwdef struct TreeBuffer{
    T,I,A<:AbstractArray{I},B<:AbstractArray{T},C,D<:AbstractArray{I}
}
    # Corresponds to the `Node` fields
    degree::A
    constant::A
    val::B
    feature::A
    op::A
    idx_l::A
    idx_r::A

    # Indexing information
    execution_order::A
    idx_self::A
    num_launches::Base.RefValue{I}
    cursor::Base.RefValue{I}

    # Segment information
    roots::C
    num_nodes::I

    # Original buffer
    buffer::D
end

function as_array(
    ::Type{I},
    trees::Union{Tuple{N,Vararg{N}},AbstractVector{N}};
    buffer::Union{AbstractArray{I},Nothing}=nothing,
) where {T,N<:AbstractExpressionNode{T},I}
    each_num_nodes = map(t -> count_nodes(t; break_sharing=Val(true)), trees)
    num_nodes = sum(each_num_nodes)

    # Compute the roots array for indexing.
    roots = cumsum(
        if each_num_nodes isa Tuple
            tuple(one(I), each_num_nodes[begin:(end - 1)]...)
        else
            vcat(one(I), @view(each_num_nodes[begin:(end - 1)]))
        end,
    )

    val = Array{T}(undef, num_nodes)

    # If no buffer is provided, create a new ArrayBuffer from scratch
    buffer = @something(buffer, Array{I}(undef, 8, num_nodes))

    # Obtain arrays from the buffer. Each call to get_array consumes one "slot".
    #! format: off
    degree =          @view buffer[1, :]
    feature =         @view buffer[2, :]
    op =              @view buffer[3, :]
    execution_order = @view buffer[4, :]
    idx_self =        @view buffer[5, :]
    idx_l =           @view buffer[6, :]
    idx_r =           @view buffer[7, :]
    constant =        @view buffer[8, :]
    #! format: on

    tree_buffers = TreeBuffer(;
        degree=degree,
        constant=constant,
        val=val,
        feature=feature,
        op=op,
        idx_l=idx_l,
        idx_r=idx_r,

        # Indexing information
        execution_order=execution_order,
        idx_self=idx_self,
        num_launches=Ref(zero(I)),
        cursor=Ref(zero(I)),

        # Segment information
        roots=roots,
        num_nodes=I(num_nodes),

        # Original buffer
        buffer=buffer,
    )

    fill_tree_buffer!(tree_buffers, trees)

    return tree_buffers
end

function fill_tree_buffer!(
    tree_buffers::TreeBuffer{T,I}, trees::Union{Tuple{N,Vararg{N}},AbstractVector{N}}
) where {T,I,N<:AbstractExpressionNode{T}}
    return foreach(Fix{1}(fill_single_tree!, tree_buffers), trees)
end

function fill_single_tree!(
    tree_buffers::TreeBuffer{T,I}, tree::N
) where {T,I,N<:AbstractExpressionNode{T}}
    return tree_mapreduce(
        Fix{1}(fill_single_leaf!, tree_buffers),
        Fix{1}(fill_single_branch!, tree_buffers),
        Fix{1}(link_parent_and_children!, tree_buffers),
        tree;
        break_sharing=Val(true),
    )
end

function fill_single_leaf!(
    tree_buffers::TreeBuffer{T,I}, leaf::N
) where {T,I,N<:AbstractExpressionNode{T}}
    self = (tree_buffers.cursor[] += one(I))
    tree_buffers.idx_self[self] = self
    tree_buffers.degree[self] = 0
    tree_buffers.execution_order[self] = one(I)
    tree_buffers.constant[self] = leaf.constant
    if leaf.constant
        tree_buffers.val[self] = leaf.val::T
    else
        tree_buffers.feature[self] = leaf.feature
    end

    return (id=self, order=one(I))
end

function fill_single_branch!(
    tree_buffers::TreeBuffer{T,I}, branch::N
) where {T,I,N<:AbstractExpressionNode{T}}
    self = (tree_buffers.cursor[] += one(I))
    tree_buffers.idx_self[self] = self
    tree_buffers.op[self] = branch.op
    tree_buffers.degree[self] = branch.degree

    return (id=self, order=one(I))
end

function link_parent_and_children!(
    tree_buffers::TreeBuffer{T,I}, parent, children::Vararg{Any,C}
) where {T,I,C}
    tree_buffers.idx_l[parent.id] = children[1].id
    if C == 2
        tree_buffers.idx_r[parent.id] = children[2].id
    end
    parent_execution_order = if C == 1
        children[1].order + one(I)
    else
        max(children[1].order, children[2].order) + one(I)
    end

    tree_buffers.execution_order[parent.id] = parent_execution_order

    if parent_execution_order > tree_buffers.num_launches[]
        tree_buffers.num_launches[] = parent_execution_order
    end

    return (id=parent.id, order=parent_execution_order)
end

end
