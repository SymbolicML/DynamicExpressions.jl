module ArrayNodeModule

using ..NodeModule: AbstractExpressionNode, Nullable
using ..UtilsModule: Undefined
using StructArrays: StructArray, StructVector

import ..NodeModule:
    constructorof,
    with_type_parameters,
    with_max_degree,
    preserve_sharing,
    max_degree,
    default_allocator,
    get_children,
    set_children!,
    unsafe_get_children,
    tree_mapreduce,
    count_nodes,
    set_node!,
    any,
    copy_node

export ArrayNode

# Node data struct
struct NodeData{T,D}
    degree::UInt8
    constant::Bool
    val::T
    feature::UInt16
    op::UInt8
    children::NTuple{D,Int8}
end

# Constructor for empty node
function NodeData{T,D}() where {T,D}
    return NodeData{T,D}(
        UInt8(0), true, zero(T), UInt16(0), UInt8(0), ntuple(_ -> Int8(-1), Val(D))
    )
end

mutable struct ArrayTree{T,D,S<:StructVector{NodeData{T,D}}}
    const nodes::S
    root_idx::Int8
    n_nodes::Int8
    const free_list::Vector{Int8}
    free_count::Int8

    function ArrayTree{T,D}(n::Int; array_type::Type{<:AbstractVector}=Vector) where {T,D}
        # Create backing arrays of the specified type
        degree = array_type{UInt8}(undef, n)
        constant = array_type{Bool}(undef, n)
        val = array_type{T}(undef, n)
        feature = array_type{UInt16}(undef, n)
        op = array_type{UInt8}(undef, n)
        children = array_type{NTuple{D,Int8}}(undef, n)

        # Create a StructVector from the backing arrays
        nodes = StructVector{NodeData{T,D}}((
            degree=degree,
            constant=constant,
            val=val,
            feature=feature,
            op=op,
            children=children,
        ))

        # Initialize all nodes to default values
        for i in 1:n
            nodes.degree[i] = UInt8(0)
            nodes.constant[i] = true
            nodes.val[i] = zero(T)
            nodes.feature[i] = UInt16(0)
            nodes.op[i] = UInt8(0)
            nodes.children[i] = ntuple(_ -> Int8(-1), Val(D))
        end

        S = typeof(nodes)
        tree = new{T,D,S}(nodes, Int8(0), Int8(0), Vector{Int8}(undef, n), Int8(n))
        # Initialize free list
        for i in 1:n
            tree.free_list[i] = Int8(i)
        end
        return tree
    end
end

struct ArrayNode{T,D,S} <: AbstractExpressionNode{T,D}
    tree::ArrayTree{T,D,S}
    idx::Int8
end

function Base.getproperty(n::ArrayNode{T,D,S}, k::Symbol) where {T,D,S}
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
            if child_idx < 0
                Nullable(true, n)  # Poison node
            else
                Nullable(false, ArrayNode{T,D,S}(tree, child_idx))
            end
        end
    elseif k == :l  # Left child for compatibility
        child_idx = @inbounds nodes.children[idx][1]
        return child_idx < 0 ? error("No left child") : ArrayNode{T,D,S}(tree, child_idx)
    elseif k == :r  # Right child for compatibility
        child_idx = @inbounds nodes.children[idx][2]
        return child_idx < 0 ? error("No right child") : ArrayNode{T,D,S}(tree, child_idx)
    else
        error("Unknown field $k")
    end
end

function Base.setproperty!(n::ArrayNode{T,D,S}, k::Symbol, v) where {T,D,S}
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
        isa(v, ArrayNode) || error("Cannot set left child to non-ArrayNode")
        children = nodes.children[idx]
        @inbounds nodes.children[idx] = (getfield(v, :idx), children[2:end]...)
    elseif k == :r
        isa(v, ArrayNode) || error("Cannot set right child to non-ArrayNode")
        children = nodes.children[idx]
        @inbounds nodes.children[idx] = (children[1], getfield(v, :idx), children[3:end]...)
    else
        error("Cannot set field $k")
    end
    return v
end

# Allocation management
function allocate_node!(tree::ArrayTree)
    tree.free_count == 0 && error("ArrayTree full")
    idx = tree.free_list[tree.free_count]
    tree.free_count -= 1
    tree.n_nodes += 1
    return idx
end

function free_node!(tree::ArrayTree, idx::Int8)
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
        return ArrayNode{T,D,typeof(tree.nodes)}(tree, idx)
    end

    if !isnothing(feature)
        tree.nodes.degree[idx] = 0
        tree.nodes.constant[idx] = false
        tree.nodes.feature[idx] = feature
        return ArrayNode{T,D,typeof(tree.nodes)}(tree, idx)
    end

    if !isnothing(op)
        # DEBUG: op=$op, l is nothing? $(isnothing(l)), r is nothing? $(isnothing(r))
        _children = if !isnothing(l) && isnothing(r)
            (l,)
        elseif !isnothing(l) && !isnothing(r)
            (l, r)
        else
            children
        end

        if !isnothing(_children)
            # DEBUG: Building node with children, length=length(_children)
            degree = length(_children)
            tree.nodes.degree[idx] = degree
            tree.nodes.op[idx] = op

            # Copy children into this tree
            child_indices = ntuple(
                i -> begin
                    if i <= length(_children)
                        child = _children[i]
                        # DEBUG: Processing child $i, isa ArrayNode? isa(child, ArrayNode)
                        if isa(child, ArrayNode)
                            child_tree = getfield(child, :tree)
                            child_idx = getfield(child, :idx)
                            if child_tree === tree
                                # Same tree - just link
                                child_idx
                            else
                                # Different tree - copy
                                new_idx = copy_subtree!(tree, child_tree, child_idx)
                                # DEBUG
                                # println("Copied child from idx $child_idx to new idx $new_idx")
                                # println("  Original: constant=", child_tree.nodes.constant[child_idx], 
                                #         child_tree.nodes.constant[child_idx] ? ", val=" : ", feature=",
                                #         child_tree.nodes.constant[child_idx] ? child_tree.nodes.val[child_idx] : child_tree.nodes.feature[child_idx])
                                # println("  Copied: constant=", tree.nodes.constant[new_idx],
                                #         tree.nodes.constant[new_idx] ? ", val=" : ", feature=",
                                #         tree.nodes.constant[new_idx] ? tree.nodes.val[new_idx] : tree.nodes.feature[new_idx])
                                new_idx
                            end
                        else
                            Int8(-1)
                        end
                    else
                        Int8(-1)
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
    return ArrayNode{T,D,typeof(tree.nodes)}(tree, idx)
end

function copy_subtree!(dst::ArrayTree{T,D}, src::ArrayTree{T,D}, src_idx::Int8) where {T,D}
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
                if child_idx >= 0
                    copy_subtree!(dst, src, child_idx)
                else
                    Int8(-1)
                end
            else
                Int8(-1)
            end
        end, Val(D)
    )
    dst.nodes.children[dst_idx] = child_indices

    return dst_idx
end

# Core interface implementations
Base.eltype(::Type{<:ArrayNode{T}}) where {T} = T
Base.eltype(::ArrayNode{T}) where {T} = T

max_degree(::Type{<:ArrayNode}) = 2
max_degree(::Type{<:ArrayNode{T,D}}) where {T,D} = D
max_degree(n::ArrayNode) = max_degree(typeof(n))

preserve_sharing(::Type{<:ArrayNode}) = false

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
    return ntuple(
        i -> begin
            child_idx = @inbounds tree.nodes.children[idx][i]
            if child_idx < 0
                Nullable(true, n)
            else
                Nullable(false, ArrayNode{T,D,typeof(tree.nodes)}(tree, child_idx))
            end
        end,
        Val(D),
    )
end

function get_children(n::ArrayNode{T,D,S}, ::Val{d}) where {T,D,S,d}
    tree = getfield(n, :tree)
    idx = getfield(n, :idx)
    return ntuple(i -> begin
        child_idx = @inbounds tree.nodes.children[idx][i]
        ArrayNode{T,D,typeof(tree.nodes)}(tree, child_idx)
    end, Val(Int(d)))
end

get_children(n::ArrayNode, d::Integer) = get_children(n, Val(d))

function set_children!(n::ArrayNode{T,D,S}, cs::Tuple) where {T,D,S}
    tree = getfield(n, :tree)
    idx = getfield(n, :idx)
    child_indices = ntuple(i -> begin
        if i <= length(cs)
            child = cs[i]
            if isa(child, ArrayNode)
                getfield(child, :idx)
            else
                Int8(-1)
            end
        else
            Int8(-1)
        end
    end, Val(D))
    return tree.nodes.children[idx] = child_indices
end

# Copy
function copy_node(n::ArrayNode{T,D,S}; break_sharing::Val{BS}=Val(false)) where {T,D,S,BS}
    tree = getfield(n, :tree)
    idx = getfield(n, :idx)

    # Count nodes to determine tree size needed
    node_count = count_nodes(n)
    # Add some buffer space
    tree_size = max(32, node_count * 2)

    # Create new tree for the copy
    new_tree = ArrayTree{T,D}(tree_size)
    new_idx = copy_subtree!(new_tree, tree, idx)
    new_tree.root_idx = new_idx

    return ArrayNode{T,D,typeof(new_tree.nodes)}(new_tree, new_idx)
end

Base.copy(n::ArrayNode) = copy_node(n)

# count_nodes - optimized version that checks if we're at root
function count_nodes(n::ArrayNode)
    tree = getfield(n, :tree)
    idx = getfield(n, :idx)
    # Optimization: if this is the root of the tree, just return total nodes
    if tree.root_idx == idx
        return Int(tree.n_nodes)
    else
        # Fall back to tree_mapreduce for subtrees
        return tree_mapreduce(_ -> 1, +, n, Int)
    end
end

# Equality and hash
function Base.:(==)(a::ArrayNode, b::ArrayNode)
    a.degree != b.degree && return false

    if a.degree == 0
        a.constant != b.constant && return false
        if a.constant
            return a.val == b.val
        else
            return a.feature == b.feature
        end
    else
        a.op != b.op && return false

        # Compare children recursively
        for i in 1:(a.degree)
            ca = get_children(a, Val(Int(a.degree)))[i]
            cb = get_children(b, Val(Int(b.degree)))[i]
            ca != cb && return false
        end
        return true
    end
end

function Base.hash(n::ArrayNode, h::UInt=zero(UInt))
    if n.degree == 0
        if n.constant
            return hash((0, n.val), h)
        else
            return hash((1, n.feature), h)
        end
    else
        children_hashes = ntuple(
            i -> begin
                child = get_children(n, Val(Int(n.degree)))[i]
                hash(child, h)
            end, Val(Int(n.degree))
        )
        return hash((n.degree + 1, n.op, children_hashes), h)
    end
end

# set_node! implementation
function set_node!(dst::ArrayNode, src::ArrayNode)
    dst_tree = getfield(dst, :tree)
    src_tree = getfield(src, :tree)
    dst_idx = getfield(dst, :idx)
    src_idx = getfield(src, :idx)

    dst.degree = src.degree

    if src.degree == 0
        dst.constant = src.constant
        if src.constant
            dst.val = src.val
        else
            dst.feature = src.feature
        end
    else
        dst.op = src.op

        D = max_degree(typeof(dst))
        child_indices = ntuple(
            i -> begin
                if i <= src.degree
                    child_idx = @inbounds src_tree.nodes.children[src_idx][i]
                    if child_idx >= 0
                        if dst_tree === src_tree
                            # Same tree
                            child_idx
                        else
                            # Different tree - need to copy
                            copy_subtree!(dst_tree, src_tree, child_idx)
                        end
                    else
                        Int8(-1)
                    end
                else
                    Int8(-1)
                end
            end,
            Val(D),
        )
        dst_tree.nodes.children[dst_idx] = child_indices
    end

    return nothing
end

# tree_mapreduce and any
function tree_mapreduce(
    f::F,
    op::G,
    n::ArrayNode,
    (::Type{RT})=Any;
    f_on_shared=nothing,
    break_sharing=Val(false),
    kwargs...,
) where {F<:Function,G<:Function,RT}
    tree = getfield(n, :tree)
    return mapreduce_impl(f, op, tree, getfield(n, :idx))
end

function mapreduce_impl(f::F, op::G, tree::ArrayTree{T,D,S}, idx::Int8) where {F,G,T,D,S}
    degree = @inbounds tree.nodes.degree[idx]
    node = ArrayNode{T,D,S}(tree, idx)
    result = f(node)

    if degree > 0
        child_results = ntuple(
            i -> begin
                child_idx = @inbounds tree.nodes.children[idx][i]
                if child_idx >= 0
                    mapreduce_impl(f, op, tree, child_idx)
                else
                    nothing
                end
            end, Val(Int(degree))
        )

        # Filter out nothings and apply op
        valid_results = filter(x -> !isnothing(x), child_results)
        if !isempty(valid_results)
            return op(result, valid_results...)
        end
    end

    return result
end

function any(f::F, n::ArrayNode) where {F<:Function}
    tree = getfield(n, :tree)
    return any_impl(f, tree, getfield(n, :idx))
end

function any_impl(f::F, tree::ArrayTree{T,D,S}, idx::Int8) where {F,T,D,S}
    node = ArrayNode{T,D,S}(tree, idx)
    f(node) && return true

    degree = @inbounds tree.nodes.degree[idx]
    for i in 1:degree
        child_idx = @inbounds tree.nodes.children[idx][i]
        if child_idx >= 0 && any_impl(f, tree, child_idx)
            return true
        end
    end

    return false
end

end # module
