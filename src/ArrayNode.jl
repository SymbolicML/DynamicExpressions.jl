module ArrayNodeModule

using ..NodeModule: AbstractExpressionNode, Nullable
using ..UtilsModule: Undefined
using StructArrays: StructArray, StructVector

import ..NodeModule:
    constructorof,
    with_type_parameters,
    with_max_degree,
    default_allocator,
    get_children,
    set_children!,
    unsafe_get_children,
    copy_node

export ArrayNode

# Node data struct
struct NodeData{T,D}
    degree::UInt8
    constant::Bool
    val::T
    feature::UInt16
    op::UInt8
    children::NTuple{D,UInt16}
end

# Constructor for empty node
function NodeData{T,D}() where {T,D}
    return NodeData{T,D}(
        UInt8(0), true, zero(T), UInt16(0), UInt8(0), ntuple(_ -> UInt16(0), Val(D))
    )
end

mutable struct ArrayTree{T,D,S<:StructVector{NodeData{T,D}}}
    const nodes::S
    root_idx::UInt16
    n_nodes::UInt16
    const free_list::Vector{UInt16}
    free_count::UInt16

    function ArrayTree{T,D}(n::Int; array_type::Type{<:AbstractVector}=Vector) where {T,D}
        # Create uninitialized StructVector directly
        # For custom array types, we'd need to pass them to StructVector somehow
        # For now, just use the default
        nodes = if array_type === Vector
            StructVector{NodeData{T,D}}(undef, n)
        else
            # For other array types, create backing arrays manually
            degree = array_type{UInt8}(undef, n)
            constant = array_type{Bool}(undef, n)
            val = array_type{T}(undef, n)
            feature = array_type{UInt16}(undef, n)
            op = array_type{UInt8}(undef, n)
            children = array_type{NTuple{D,UInt16}}(undef, n)
            StructVector{NodeData{T,D}}((
                degree=degree,
                constant=constant,
                val=val,
                feature=feature,
                op=op,
                children=children,
            ))
        end

        S = typeof(nodes)
        free_list = Vector{UInt16}(undef, n)
        tree = new{T,D,S}(nodes, UInt16(0), UInt16(0), free_list, UInt16(n))
        # Initialize free list in-place
        @inbounds @simd for i in 1:n
            tree.free_list[i] = UInt16(i)
        end
        return tree
    end
end

struct ArrayNode{T,D,S} <: AbstractExpressionNode{T,D}
    tree::ArrayTree{T,D,S}
    idx::UInt16
end

@inline function Base.getproperty(n::ArrayNode{T,D,S}, k::Symbol) where {T,D,S}
    tree = getfield(n, :tree)
    idx = getfield(n, :idx)
    nodes = getfield(tree, :nodes)

    if k == :tree
        return tree
    elseif k == :idx
        return idx
    elseif k == :degree
        return @inbounds nodes.degree[idx]
    elseif k == :constant
        return @inbounds nodes.constant[idx]
    elseif k == :val
        return @inbounds nodes.val[idx]
    elseif k == :feature
        return @inbounds nodes.feature[idx]
    elseif k == :op
        return @inbounds nodes.op[idx]
    elseif k == :children
        # Return tuple of child ArrayNodes wrapped in Nullable
        return ntuple(Val(D)) do i
            child_idx = @inbounds nodes.children[idx][i]
            if child_idx == 0
                Nullable(true, n)  # Poison node
            else
                Nullable(false, ArrayNode{T,D,S}(tree, child_idx))
            end
        end
    elseif k == :l
        child_idx = @inbounds nodes.children[idx][1]
        child_idx == 0 && error("No left child")
        return ArrayNode{T,D,S}(tree, child_idx)
    elseif k == :r
        child_idx = @inbounds nodes.children[idx][2]
        child_idx == 0 && error("No right child")
        return ArrayNode{T,D,S}(tree, child_idx)
    else
        error("Unknown field $k")
    end
end

@inline function Base.setproperty!(n::ArrayNode{T,D,S}, k::Symbol, v) where {T,D,S}
    tree = getfield(n, :tree)
    idx = getfield(n, :idx)
    nodes = getfield(tree, :nodes)

    if k == :degree
        @inbounds nodes.degree[idx] = v
    elseif k == :constant
        @inbounds nodes.constant[idx] = v
    elseif k == :val
        @inbounds nodes.val[idx] = v
    elseif k == :feature
        @inbounds nodes.feature[idx] = v
    elseif k == :op
        @inbounds nodes.op[idx] = v
    elseif k == :l
        !isa(v, ArrayNode) && error("Cannot set left child to non-ArrayNode")
        children = nodes.children[idx]
        @inbounds nodes.children[idx] = (getfield(v, :idx), children[2:end]...)
    elseif k == :r
        !isa(v, ArrayNode) && error("Cannot set right child to non-ArrayNode")
        children = nodes.children[idx]
        @inbounds nodes.children[idx] = (children[1], getfield(v, :idx), children[3:end]...)
    else
        error("Cannot set field $k")
    end
    return v
end

# Allocation management
@inline function allocate_node!(tree::ArrayTree)
    tree.free_count == 0 && error("ArrayTree full")
    idx = tree.free_list[tree.free_count]
    tree.free_count -= 1
    tree.n_nodes += 1
    return idx
end

@inline function free_node!(tree::ArrayTree, idx::UInt16)
    tree.free_count += 1
    tree.free_list[tree.free_count] = idx
    return tree.n_nodes -= 1
end

# Default constructors - now include array type parameters
function ArrayNode{T,D}(n::Int; array_type::Type{<:AbstractVector}=Vector) where {T,D}
    return ArrayNode{T,D}(Undefined; allocator=ArrayTree{T,D}(n; array_type=array_type))
end
function ArrayNode{T}(n::Int; array_type::Type{<:AbstractVector}=Vector) where {T}
    return ArrayNode{T,2}(n; array_type=array_type)
end

# Keyword constructors for partial type signatures  
ArrayNode{T,D}(; kwargs...) where {T,D} = ArrayNode{T,D}(Undefined; kwargs...)
ArrayNode{T,D,S}(; kwargs...) where {T,D,S} = ArrayNode{T,D}(Undefined; kwargs...)
ArrayNode{T}(; kwargs...) where {T} = ArrayNode{T,2}(; kwargs...)

# Constructor with keyword arguments - matches Node interface  
function ArrayNode{T,D}(
    ::Type{T1};
    val=nothing,
    feature=nothing,
    op=nothing,
    l=nothing,
    r=nothing,
    children=nothing,
    allocator=nothing,
) where {T,D,T1}
    # Determine tree source
    tree = if !isnothing(allocator) && isa(allocator, ArrayTree)
        allocator
    else
        # Default size of 64 nodes for small expressions
        # This is wasteful if building incrementally, but matches Node semantics
        ArrayTree{T,D}(64)
    end

    idx = allocate_node!(tree)
    # Only set root_idx if this tree is new (no nodes allocated yet except this one)
    if tree.n_nodes == 1
        tree.root_idx = idx
    end

    if !isnothing(val)
        tree.nodes.degree[idx] = 0
        tree.nodes.constant[idx] = true
        tree.nodes.val[idx] = val
        # Clear children for leaf node
        tree.nodes.children[idx] = ntuple(_ -> UInt16(0), Val(D))
        return ArrayNode{T,D,typeof(tree.nodes)}(tree, idx)
    end

    if !isnothing(feature)
        tree.nodes.degree[idx] = 0
        tree.nodes.constant[idx] = false
        tree.nodes.feature[idx] = feature
        # Clear children for leaf node  
        tree.nodes.children[idx] = ntuple(_ -> UInt16(0), Val(D))
        return ArrayNode{T,D,typeof(tree.nodes)}(tree, idx)
    end

    if !isnothing(op)
        _children = if !isnothing(l) && isnothing(r)
            (l,)
        elseif !isnothing(l) && !isnothing(r)
            (l, r)
        else
            children
        end

        if !isnothing(_children)
            degree = length(_children)
            tree.nodes.degree[idx] = degree
            tree.nodes.op[idx] = op

            # Copy children into this tree
            child_indices = ntuple(
                i -> begin
                    if i <= length(_children)
                        child = _children[i]
                        if isa(child, ArrayNode)
                            child_tree = getfield(child, :tree)
                            child_idx = getfield(child, :idx)
                            if child_tree === tree
                                # Same tree - just link
                                child_idx
                            else
                                # Different tree - copy
                                copy_subtree!(tree, child_tree, child_idx)
                            end
                        else
                            UInt16(0)
                        end
                    else
                        UInt16(0)
                    end
                end,
                Val(D),
            )
            tree.nodes.children[idx] = child_indices

            return ArrayNode{T,D,typeof(tree.nodes)}(tree, idx)
        end
    end

    # Default: empty constant
    tree.nodes.degree[idx] = 0
    tree.nodes.constant[idx] = true
    tree.nodes.val[idx] = zero(T)
    tree.nodes.children[idx] = ntuple(_ -> UInt16(0), Val(D))
    return ArrayNode{T,D,typeof(tree.nodes)}(tree, idx)
end

function copy_subtree!(
    dst::ArrayTree{T,D}, src::ArrayTree{T,D}, src_idx::UInt16
) where {T,D}
    dst_idx = allocate_node!(dst)

    @inbounds begin
        dst.nodes.degree[dst_idx] = src.nodes.degree[src_idx]
        dst.nodes.constant[dst_idx] = src.nodes.constant[src_idx]
        dst.nodes.val[dst_idx] = src.nodes.val[src_idx]
        dst.nodes.feature[dst_idx] = src.nodes.feature[src_idx]
        dst.nodes.op[dst_idx] = src.nodes.op[src_idx]
    end

    degree = @inbounds src.nodes.degree[src_idx]
    child_indices = ntuple(
        i -> begin
            if i <= degree
                child_idx = @inbounds src.nodes.children[src_idx][i]
                if child_idx > 0
                    copy_subtree!(dst, src, child_idx)
                else
                    UInt16(0)
                end
            else
                UInt16(0)
            end
        end, Val(D)
    )
    dst.nodes.children[dst_idx] = child_indices

    return dst_idx
end

constructorof(::Type{<:ArrayNode}) = ArrayNode
with_type_parameters(::Type{<:ArrayNode}, ::Type{T}) where {T} = ArrayNode{T,2}
with_max_degree(::Type{<:ArrayNode{T,D}}, ::Val{D2}) where {T,D,D2} = ArrayNode{T,D2}
function default_allocator(
    ::Type{ArrayNode{T,D}}; array_type::Type{<:AbstractVector}=Vector
) where {T,D}
    return ArrayTree{T,D}(32; array_type=array_type)
end

# get_children and set_children!
function unsafe_get_children(n::ArrayNode{T,D,S}) where {T,D,S}
    tree = getfield(n, :tree)
    idx = getfield(n, :idx)
    return ntuple(i -> begin
        child_idx = @inbounds tree.nodes.children[idx][i]
        if child_idx == 0
            Nullable(true, n)
        else
            Nullable(false, ArrayNode{T,D,S}(tree, child_idx))
        end
    end, Val(D))
end

function get_children(n::ArrayNode{T,D,S}, ::Val{d}) where {T,D,S,d}
    tree = getfield(n, :tree)
    idx = getfield(n, :idx)
    return ntuple(i -> begin
        child_idx = @inbounds tree.nodes.children[idx][i]
        ArrayNode{T,D,S}(tree, child_idx)
    end, Val(Int(d)))
end

function set_children!(n::ArrayNode{T,D,S}, cs::Tuple) where {T,D,S}
    tree = getfield(n, :tree)
    idx = getfield(n, :idx)
    child_indices = ntuple(Val(D)) do i
        if i <= length(cs)
            child = cs[i]
            if isa(child, Nullable)
                # Handle Nullable wrapped children
                if child.null
                    UInt16(0)
                else
                    child_node = child.x
                    child_tree = getfield(child_node, :tree)
                    child_idx = getfield(child_node, :idx)
                    if child_tree === tree
                        # Same tree - just use the index
                        child_idx
                    else
                        # Different tree - need to copy the subtree
                        copy_subtree!(tree, child_tree, child_idx)
                    end
                end
            elseif isa(child, ArrayNode)
                child_tree = getfield(child, :tree)
                child_idx = getfield(child, :idx)
                if child_tree === tree
                    # Same tree - just use the index
                    child_idx
                else
                    # Different tree - need to copy the subtree
                    copy_subtree!(tree, child_tree, child_idx)
                end
            else
                UInt16(0)
            end
        else
            UInt16(0)
        end
    end
    tree.nodes.children[idx] = child_indices
    return nothing
end

# Helper to mark nodes as reachable from a given root
function mark_reachable!(
    reachable::Vector{Bool}, tree::ArrayTree{T,D}, idx::UInt16
) where {T,D}
    if idx == 0 || reachable[idx]
        return nothing
    end
    reachable[idx] = true
    degree = @inbounds tree.nodes.degree[idx]
    for i in 1:degree
        child_idx = @inbounds tree.nodes.children[idx][i]
        if child_idx != 0
            mark_reachable!(reachable, tree, child_idx)
        end
    end
end

# Copy
# Note: break_sharing parameter is ignored since ArrayNode doesn't preserve sharing
function copy_node(n::ArrayNode{T,D,S}; break_sharing::Val{BS}=Val(false)) where {T,D,S,BS}
    # BS parameter unused - ArrayNode always breaks sharing since each node owns its tree
    tree = getfield(n, :tree)
    idx = getfield(n, :idx)
    n_capacity = length(tree.nodes)

    # Create new tree with same capacity
    new_tree = if tree.nodes.degree isa Vector
        ArrayTree{T,D}(n_capacity; array_type=Vector)
    else
        ArrayTree{T,D}(n_capacity)
    end

    # Direct array copy - works for both full tree and subtree
    new_tree.nodes.degree[:] = tree.nodes.degree
    new_tree.nodes.constant[:] = tree.nodes.constant
    new_tree.nodes.val[:] = tree.nodes.val
    new_tree.nodes.feature[:] = tree.nodes.feature
    new_tree.nodes.op[:] = tree.nodes.op
    new_tree.nodes.children[:] = tree.nodes.children

    # Set the root to our copied node
    new_tree.root_idx = idx

    if idx == tree.root_idx
        # Full tree copy - just copy all metadata
        new_tree.n_nodes = tree.n_nodes
        new_tree.free_count = tree.free_count
        new_tree.free_list[:] = tree.free_list
    else
        # Subtree copy - need to update free list to exclude unreachable nodes
        reachable = fill(false, n_capacity)
        mark_reachable!(reachable, new_tree, idx)

        # Reset free list with unreachable nodes
        new_tree.free_count = 0
        new_tree.n_nodes = 0
        for i in 1:n_capacity
            if !reachable[i]
                new_tree.free_count += 1
                new_tree.free_list[new_tree.free_count] = UInt16(i)
            else
                new_tree.n_nodes += 1
            end
        end
    end

    return ArrayNode{T,D,S}(new_tree, new_tree.root_idx)
end

Base.copy(n::ArrayNode) = copy_node(n)

end # module
