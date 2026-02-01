module ArenaNodeModule

using ..UtilsModule: Nullable

import ..NodeModule:
    AbstractNode,
    AbstractExpressionNode,
    Node,
    poison_node,
    unsafe_get_children,
    get_child,
    set_child!,
    set_children!

"""Array-backed arena storing the fields of a tree node in a struct-of-arrays form.

Indices are `Int32` and are 1-based. A child index of `0` indicates an empty slot.

This is an *experimental prototype* intended to provide an arena-backed representation
with a `Node`-like facade (`ArenaNode`) that supports existing tree algorithms that are
written against `AbstractExpressionNode`.
"""
mutable struct Arena{T,D}
    degree::Vector{UInt8}
    constant::Vector{Bool}
    val::Vector{T}
    feature::Vector{UInt16}
    op::Vector{UInt8}
    children::Vector{NTuple{D,Int32}}

    function Arena{T,D}(; capacity::Integer=0) where {T,D}
        degree = UInt8[]
        constant = Bool[]
        val = T[]
        feature = UInt16[]
        op = UInt8[]
        children = NTuple{D,Int32}[]
        if capacity > 0
            sizehint!(degree, capacity)
            sizehint!(constant, capacity)
            sizehint!(val, capacity)
            sizehint!(feature, capacity)
            sizehint!(op, capacity)
            sizehint!(children, capacity)
        end
        return new{T,D}(degree, constant, val, feature, op, children)
    end
end

"""A lightweight facade for a node stored in an [`Arena`](@ref).

This wrapper is intentionally minimal: it stores an arena reference and an index.
Core fields are accessed and mutated via `getproperty`/`setproperty!`.
"""
struct ArenaNode{T,D} <: AbstractExpressionNode{T,D}
    arena::Arena{T,D}
    idx::Int32
end

@inline ArenaNode(arena::Arena{T,D}, idx::Integer) where {T,D} =
    ArenaNode{T,D}(arena, Int32(idx))

@inline function _zero_children(::Val{D}) where {D}
    return ntuple(_ -> Int32(0), Val(D))
end

@inline function _push_node!(
    arena::Arena{T,D},
    degree::UInt8,
    constant::Bool,
    val::T,
    feature::UInt16,
    op::UInt8,
    children::NTuple{D,Int32},
) where {T,D}
    push!(arena.degree, degree)
    push!(arena.constant, constant)
    push!(arena.val, val)
    push!(arena.feature, feature)
    push!(arena.op, op)
    push!(arena.children, children)
    return Int32(length(arena.degree))
end

@inline function push_constant!(arena::Arena{T,D}, value) where {T,D}
    return _push_node!(
        arena,
        UInt8(0),
        true,
        convert(T, value),
        UInt16(0),
        UInt8(0),
        _zero_children(Val(D)),
    )
end

@inline function push_feature!(arena::Arena{T,D}, feature::Integer) where {T,D}
    return _push_node!(
        arena,
        UInt8(0),
        false,
        zero(T),
        UInt16(feature),
        UInt8(0),
        _zero_children(Val(D)),
    )
end

@inline function push_branch!(
    arena::Arena{T,D}, op::Integer, child_idxs::NTuple{N,Int32}
) where {T,D,N}
    @assert N <= D
    children = ntuple(i -> (i <= N ? child_idxs[i] : Int32(0)), Val(D))
    return _push_node!(
        arena,
        UInt8(N),
        false,
        zero(T),
        UInt16(0),
        UInt8(op),
        children,
    )
end

"""Create a default node (a `0` constant leaf) in its own fresh arena."""
function ArenaNode{T,D}() where {T,D}
    arena = Arena{T,D}()
    idx = push_constant!(arena, zero(T))
    return ArenaNode{T,D}(arena, idx)
end

@inline function Base.getproperty(n::ArenaNode{T,D}, k::Symbol) where {T,D}
    if k === :degree
        return @inbounds n.arena.degree[Int(n.idx)]
    elseif k === :constant
        return @inbounds n.arena.constant[Int(n.idx)]
    elseif k === :val
        return @inbounds n.arena.val[Int(n.idx)]
    elseif k === :feature
        return @inbounds n.arena.feature[Int(n.idx)]
    elseif k === :op
        return @inbounds n.arena.op[Int(n.idx)]
    elseif k === :children
        return unsafe_get_children(n)
    elseif k === :l
        return get_child(n, 1)
    elseif k === :r
        return get_child(n, 2)
    else
        return getfield(n, k)
    end
end

@inline function Base.setproperty!(n::ArenaNode{T,D}, k::Symbol, v) where {T,D}
    i = Int(n.idx)
    if k === :degree
        @inbounds n.arena.degree[i] = UInt8(v)
        return v
    elseif k === :constant
        @inbounds n.arena.constant[i] = Bool(v)
        return v
    elseif k === :val
        @inbounds n.arena.val[i] = convert(T, v)
        return v
    elseif k === :feature
        @inbounds n.arena.feature[i] = UInt16(v)
        return v
    elseif k === :op
        @inbounds n.arena.op[i] = UInt8(v)
        return v
    elseif k === :l
        set_child!(n, v, 1)
        return v
    elseif k === :r
        set_child!(n, v, 2)
        return v
    else
        throw(ArgumentError("Unsupported field $k for ArenaNode"))
    end
end

"""Return an `NTuple{D,Nullable{ArenaNode}}` of children wrappers.

Unused slots are represented as poison nodes (mirroring `Node`), so that
accessing them throws an `UndefRefError`.
"""
@inline function unsafe_get_children(n::ArenaNode{T,D}) where {T,D}
    idxs = @inbounds n.arena.children[Int(n.idx)]
    return ntuple(Val(D)) do j
        c = idxs[j]
        if c == 0
            return poison_node(n)
        else
            return Nullable(false, ArenaNode(n.arena, c))
        end
    end
end

@inline function get_child(n::ArenaNode{T,D}, i::Int) where {T,D}
    c = @inbounds n.arena.children[Int(n.idx)][i]
    c == 0 && throw(UndefRefError())
    return ArenaNode(n.arena, c)
end

@inline function set_child!(
    n::ArenaNode{T,D}, child::AbstractNode{D}, i::Int
) where {T,D}
    child isa ArenaNode{T,D} ||
        throw(ArgumentError("ArenaNode children must be ArenaNode{T,D} (got $(typeof(child)))"))
    child.arena === n.arena ||
        throw(ArgumentError("Cannot link ArenaNodes from different arenas"))
    old = @inbounds n.arena.children[Int(n.idx)]
    @inbounds n.arena.children[Int(n.idx)] = Base.setindex(old, child.idx, i)
    return child
end

@inline function set_children!(
    n::ArenaNode{T,D}, children::Union{Tuple,AbstractVector}
) where {T,D}
    D2 = length(children)
    idxs = _zero_children(Val(D))
    @inbounds for i in 1:min(D, D2)
        c = children[i]
        if c isa Nullable
            if c.null
                # keep 0
            else
                c2 = c[]
                c2 isa ArenaNode{T,D} || throw(
                    ArgumentError(
                        "ArenaNode children must be ArenaNode{T,D} (got $(typeof(c2)))",
                    ),
                )
                c2.arena === n.arena ||
                    throw(ArgumentError("Cannot link ArenaNodes from different arenas"))
                idxs = Base.setindex(idxs, c2.idx, i)
            end
        else
            c isa ArenaNode{T,D} || throw(
                ArgumentError("ArenaNode children must be ArenaNode{T,D} (got $(typeof(c)))"),
            )
            c.arena === n.arena ||
                throw(ArgumentError("Cannot link ArenaNodes from different arenas"))
            idxs = Base.setindex(idxs, c.idx, i)
        end
    end
    @inbounds n.arena.children[Int(n.idx)] = idxs
    return nothing
end

"""Copy a tree into a new arena and return the new root node."""
function Base.copy(tree::ArenaNode{T,D}; break_sharing::Val{BS}=Val(false)) where {T,D,BS}
    # Sharing is not supported in ArenaNode; ignore `break_sharing`.
    arena = Arena{T,D}(; capacity=length(tree; break_sharing=Val(true)))
    idx = _copy_to_arena!(arena, tree)
    return ArenaNode{T,D}(arena, idx)
end

function _copy_to_arena!(arena::Arena{T,D}, tree::AbstractExpressionNode{T,D}) where {T,D}
    d = tree.degree
    if d == 0
        if tree.constant
            return push_constant!(arena, tree.val)
        else
            return push_feature!(arena, tree.feature)
        end
    end

    # Build a full `NTuple{D,Int32}` of copied child indices.
    idxs = _zero_children(Val(D))
    @inbounds for i in 1:Int(d)
        idxs = Base.setindex(idxs, _copy_to_arena!(arena, get_child(tree, i)), i)
    end
    return _push_node!(arena, UInt8(d), false, zero(T), UInt16(0), tree.op, idxs)
end

"""Convert an existing tree into an arena-backed representation.

This copies the entire tree into a fresh arena.
"""
function arena_from_tree(tree::AbstractExpressionNode{T,D}) where {T,D}
    arena = Arena{T,D}(; capacity=length(tree; break_sharing=Val(true)))
    idx = _copy_to_arena!(arena, tree)
    return ArenaNode{T,D}(arena, idx)
end

"""Convert an arena-backed node back into a heap-allocated `Node` tree."""
function tree_from_arena(tree::ArenaNode{T,D}) where {T,D}
    function rebuild(n::ArenaNode{T,D})
        d = n.degree
        if d == 0
            return n.constant ? Node{T,D}(; val=n.val) : Node{T,D}(T; feature=n.feature)
        else
            # Use a vector here to avoid `Val(d)` with runtime `d`.
            cs = Vector{Node{T,D}}(undef, Int(d))
            @inbounds for i in 1:Int(d)
                cs[i] = rebuild(get_child(n, i))
            end
            return Node{T,D}(T; op=n.op, children=cs)
        end
    end

    return rebuild(tree)
end

end
