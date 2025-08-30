module ArrayNodeModule

using ..NodeModule: AbstractExpressionNode, Nullable
using ..UtilsModule: Undefined

import ..NodeModule:
    constructorof, with_type_parameters, with_max_degree,
    preserve_sharing, max_degree, default_allocator,
    get_children, set_children!, unsafe_get_children,
    tree_mapreduce, count_nodes, set_node!, any, copy_node

import Base: copy, hash, ==, getproperty, setproperty!, eltype

export ArrayNode

# Helper function to create array of specific type
@inline function create_array(::Type{A}, ::Type{T}, n::Int) where {A,T}
    return A{T}(undef, n)
end

mutable struct ArrayTree{T,D,A<:AbstractVector}
    degrees::A  # Vector of UInt8
    constants::A  # Vector of Bool
    vals::A  # Vector of T
    features::A  # Vector of UInt16
    ops::A  # Vector of UInt8
    children::A  # Vector of NTuple{D,Int8}
    
    root_idx::Int8
    n_nodes::Int8
    free_list::A  # Vector of Int8
    free_count::Int8
    
    function ArrayTree{T,D,A}(n::Int) where {T,D,A<:AbstractVector}
        tree = new{T,D,A}(
            create_array(A, UInt8, n),
            create_array(A, Bool, n),
            create_array(A, T, n),
            create_array(A, UInt16, n),
            create_array(A, UInt8, n),
            create_array(A, NTuple{D,Int8}, n),
            Int8(0), Int8(0),
            create_array(A, Int8, n),
            Int8(n)
        )
        # Initialize free list and children
        for i in 1:n
            tree.free_list[i] = Int8(i)
            tree.children[i] = ntuple(_ -> Int8(-1), Val(D))
        end
        return tree
    end
end

# Default constructor using regular arrays
ArrayTree{T,D}(n::Int) where {T,D} = ArrayTree{T,D,Vector}(n)

mutable struct ArrayNode{T,D,A<:AbstractVector} <: AbstractExpressionNode{T,D}
    tree::ArrayTree{T,D,A}
    idx::Int8
end

function getproperty(n::ArrayNode, k::Symbol)
    tree = getfield(n, :tree)
    idx = getfield(n, :idx)
    
    if k == :tree
        return tree
    elseif k == :idx
        return idx
    elseif k == :degree
        return @inbounds tree.degrees[idx]
    elseif k == :constant
        return @inbounds tree.constants[idx]
    elseif k == :val
        return @inbounds tree.vals[idx]
    elseif k == :feature
        return @inbounds tree.features[idx]
    elseif k == :op
        return @inbounds tree.ops[idx]
    elseif k == :children
        # Return tuple of child ArrayNodes wrapped in Nullable
        D = max_degree(typeof(n))
        return ntuple(i -> begin
            child_idx = @inbounds tree.children[idx][i]
            if child_idx < 0
                Nullable(true, n)  # Poison node
            else
                Nullable(false, ArrayNode(tree, child_idx))
            end
        end, Val(D))
    elseif k == :l  # Left child for compatibility
        child_idx = @inbounds tree.children[idx][1]
        return child_idx < 0 ? error("No left child") : ArrayNode(tree, child_idx)
    elseif k == :r  # Right child for compatibility
        child_idx = @inbounds tree.children[idx][2]
        return child_idx < 0 ? error("No right child") : ArrayNode(tree, child_idx)
    else
        error("Unknown field $k")
    end
end

function setproperty!(n::ArrayNode, k::Symbol, v)
    tree = getfield(n, :tree)
    idx = getfield(n, :idx)
    
    if k == :degree
        @inbounds tree.degrees[idx] = v
    elseif k == :constant
        @inbounds tree.constants[idx] = v
    elseif k == :val
        @inbounds tree.vals[idx] = v
    elseif k == :feature
        @inbounds tree.features[idx] = v
    elseif k == :op
        @inbounds tree.ops[idx] = v
    elseif k == :l
        if isa(v, ArrayNode)
            children = tree.children[idx]
            @inbounds tree.children[idx] = (getfield(v, :idx), children[2:end]...)
        end
    elseif k == :r
        if isa(v, ArrayNode)
            children = tree.children[idx]
            @inbounds tree.children[idx] = (children[1], getfield(v, :idx), children[3:end]...)
        end
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
    tree.n_nodes -= 1
end

# Default constructors - now include array type parameters
ArrayNode{T,D,A}(n::Int=32) where {T,D,A} = ArrayNode{T,D,A}(Undefined; allocator=ArrayTree{T,D,A}(n))
ArrayNode{T,D}(n::Int) where {T,D} = ArrayNode{T,D,Vector}(n)
ArrayNode{T}(n::Int) where {T} = ArrayNode{T,2,Vector}(n)

# Keyword constructors for partial type signatures  
ArrayNode{T,D}(; kwargs...) where {T,D} = ArrayNode{T,D,Vector}(Undefined; kwargs...)
ArrayNode{T}(; kwargs...) where {T} = ArrayNode{T,2,Vector}(Undefined; kwargs...)

# Constructor with keyword arguments - matches Node interface  
function ArrayNode{T,D,A}(
    ::Type{T1}=Undefined;
    val=nothing,
    feature=nothing, 
    op=nothing,
    l=nothing,
    r=nothing,
    children=nothing,
    allocator=nothing
) where {T,D,A<:AbstractVector,T1}
    # Determine tree source
    # Always create a new tree unless an allocator is explicitly provided
    tree = if !isnothing(allocator) && isa(allocator, ArrayTree)
        allocator
    else
        # Just use a reasonable default size
        ArrayTree{T,D,A}(64)
    end
    
    idx = allocate_node!(tree)
    # Only set root_idx if this tree is new (no nodes allocated yet except this one)
    if tree.n_nodes == 1
        tree.root_idx = idx
    end
    
    if !isnothing(val)
        tree.degrees[idx] = 0
        tree.constants[idx] = true
        tree.vals[idx] = val
        return ArrayNode(tree, idx)
    end
    
    if !isnothing(feature)
        tree.degrees[idx] = 0
        tree.constants[idx] = false
        tree.features[idx] = feature
        return ArrayNode(tree, idx)
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
            tree.degrees[idx] = degree
            tree.ops[idx] = op
            
            # Copy children into this tree
            child_indices = ntuple(i -> begin
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
                        Int8(-1)
                    end
                else
                    Int8(-1)
                end
            end, Val(D))
            tree.children[idx] = child_indices
            
            return ArrayNode(tree, idx)
        end
    end
    
    # Default: empty constant
    tree.degrees[idx] = 0
    tree.constants[idx] = true
    tree.vals[idx] = zero(T)
    return ArrayNode(tree, idx)
end

function copy_subtree!(dst::ArrayTree{T,D,A}, src::ArrayTree{T,D,A}, src_idx::Int8) where {T,D,A}
    dst_idx = allocate_node!(dst)
    
    @inbounds begin
        dst.degrees[dst_idx] = src.degrees[src_idx]
        dst.constants[dst_idx] = src.constants[src_idx]
        dst.vals[dst_idx] = src.vals[src_idx]
        dst.features[dst_idx] = src.features[src_idx]
        dst.ops[dst_idx] = src.ops[src_idx]
    end
    
    degree = @inbounds src.degrees[src_idx]
    child_indices = ntuple(i -> begin
        if i <= degree
            child_idx = @inbounds src.children[src_idx][i]
            if child_idx >= 0
                copy_subtree!(dst, src, child_idx)
            else
                Int8(-1)
            end
        else
            Int8(-1)
        end
    end, Val(D))
    dst.children[dst_idx] = child_indices
    
    return dst_idx
end

# Core interface implementations
eltype(::Type{<:ArrayNode{T}}) where {T} = T
eltype(::ArrayNode{T}) where {T} = T

max_degree(::Type{<:ArrayNode}) = 2
max_degree(::Type{<:ArrayNode{T,D}}) where {T,D} = D
max_degree(n::ArrayNode) = max_degree(typeof(n))

preserve_sharing(::Type{<:ArrayNode}) = false

constructorof(::Type{<:ArrayNode}) = ArrayNode
with_type_parameters(::Type{<:ArrayNode}, ::Type{T}) where {T} = ArrayNode{T,2,Vector}
with_max_degree(::Type{<:ArrayNode{T,D,A}}, ::Val{D2}) where {T,D,A,D2} = ArrayNode{T,D2,A}
default_allocator(::Type{ArrayNode{T,D,A}}) where {T,D,A} = ArrayTree{T,D,A}(32)

# get_children and set_children!
function unsafe_get_children(n::ArrayNode{T,D}) where {T,D}
    tree = getfield(n, :tree)
    idx = getfield(n, :idx)
    return ntuple(i -> begin
        child_idx = @inbounds tree.children[idx][i]
        if child_idx < 0
            Nullable(true, n)
        else
            Nullable(false, ArrayNode(tree, child_idx))
        end
    end, Val(D))
end

function get_children(n::ArrayNode{T,D}, ::Val{d}) where {T,D,d}
    tree = getfield(n, :tree)
    idx = getfield(n, :idx)
    return ntuple(i -> begin
        child_idx = @inbounds tree.children[idx][i]
        ArrayNode(tree, child_idx)
    end, Val(Int(d)))
end

get_children(n::ArrayNode, d::Integer) = get_children(n, Val(d))

function set_children!(n::ArrayNode{T,D,A}, cs::Tuple) where {T,D,A}
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
    tree.children[idx] = child_indices
end

# Copy
function copy_node(n::ArrayNode{T,D,A}; break_sharing::Val{BS}=Val(false)) where {T,D,A,BS}
    tree = getfield(n, :tree)
    idx = getfield(n, :idx)
    
    # Count nodes to determine tree size needed
    node_count = count_subtree(tree, idx)
    # Add some buffer space
    tree_size = max(32, node_count * 2)
    
    # Create new tree for the copy
    new_tree = ArrayTree{T,D,A}(tree_size)
    new_idx = copy_subtree!(new_tree, tree, idx)
    new_tree.root_idx = new_idx
    
    return ArrayNode(new_tree, new_idx)
end

copy(n::ArrayNode) = copy_node(n)

# count_nodes
function count_nodes(n::ArrayNode)
    tree = getfield(n, :tree)
    return count_subtree(tree, getfield(n, :idx))
end

function count_subtree(tree::ArrayTree, idx::Int8)
    count = 1
    degree = @inbounds tree.degrees[idx]
    for i in 1:degree
        child_idx = @inbounds tree.children[idx][i]
        if child_idx >= 0
            count += count_subtree(tree, child_idx)
        end
    end
    return count
end

# Equality and hash
function ==(a::ArrayNode, b::ArrayNode)
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
        for i in 1:a.degree
            ca = get_children(a, Val(Int(a.degree)))[i]
            cb = get_children(b, Val(Int(b.degree)))[i]
            ca != cb && return false
        end
        return true
    end
end

function hash(n::ArrayNode, h::UInt=zero(UInt))
    if n.degree == 0
        if n.constant
            return hash((0, n.val), h)
        else
            return hash((1, n.feature), h)
        end
    else
        children_hashes = ntuple(i -> begin
            child = get_children(n, Val(Int(n.degree)))[i]
            hash(child, h)
        end, Val(Int(n.degree)))
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
        child_indices = ntuple(i -> begin
            if i <= src.degree
                child_idx = @inbounds src_tree.children[src_idx][i]
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
        end, Val(D))
        dst_tree.children[dst_idx] = child_indices
    end
    
    return nothing
end

# tree_mapreduce and any
function tree_mapreduce(f::F, op::G, n::ArrayNode, ::Type{RT}=Any; f_on_shared=nothing, break_sharing=Val(false), kwargs...) where {F<:Function,G<:Function,RT}
    tree = getfield(n, :tree)
    return mapreduce_impl(f, op, tree, getfield(n, :idx))
end

function mapreduce_impl(f::F, op::G, tree::ArrayTree, idx::Int8) where {F,G}
    degree = @inbounds tree.degrees[idx]
    node = ArrayNode(tree, idx)
    result = f(node)
    
    if degree > 0
        child_results = ntuple(i -> begin
            child_idx = @inbounds tree.children[idx][i]
            if child_idx >= 0
                mapreduce_impl(f, op, tree, child_idx)
            else
                nothing
            end
        end, Val(Int(degree)))
        
        # Filter out nothings and apply op
        valid_results = filter(x -> !isnothing(x), child_results)
        if !isempty(valid_results)
            return op(result, valid_results...)
        end
    end
    
    return result
end

function any(f::F, n::ArrayNode) where F<:Function
    tree = getfield(n, :tree)
    return any_impl(f, tree, getfield(n, :idx))
end

function any_impl(f::F, tree::ArrayTree, idx::Int8) where F
    node = ArrayNode(tree, idx)
    f(node) && return true
    
    degree = @inbounds tree.degrees[idx]
    for i in 1:degree
        child_idx = @inbounds tree.children[idx][i]
        if child_idx >= 0 && any_impl(f, tree, child_idx)
            return true
        end
    end
    
    return false
end

end # module