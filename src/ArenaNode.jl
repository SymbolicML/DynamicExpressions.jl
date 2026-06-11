module ArenaNodeModule

using ..UtilsModule: Nullable

import ..NodeModule:
    AbstractNode,
    AbstractExpressionNode,
    Node,
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
struct Arena{T,D}
    degree::Vector{UInt8}
    constant::Vector{Bool}
    val::Vector{T}
    feature::Vector{UInt16}
    op::Vector{UInt8}
    children::Vector{NTuple{D,Int32}}

    function Arena{T,D}(; capacity::Integer=0) where {T,D}
        degree = sizehint!(UInt8[], capacity)
        constant = sizehint!(Bool[], capacity)
        val = sizehint!(T[], capacity)
        feature = sizehint!(UInt16[], capacity)
        op = sizehint!(UInt8[], capacity)
        children = sizehint!(NTuple{D,Int32}[], capacity)
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

    @inline function ArenaNode{T,D}(arena::Arena{T,D}, idx::Int32) where {T,D}
        return new{T,D}(arena, idx)
    end
end

@inline ArenaNode(arena::Arena{T,D}, idx::Int32) where {T,D} = ArenaNode{T,D}(arena, idx)

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
        arena, UInt8(0), false, zero(T), UInt16(feature), UInt8(0), _zero_children(Val(D))
    )
end

@inline function push_branch!(
    arena::Arena{T,D}, op::Integer, child_idxs::NTuple{N,Int32}
) where {T,D,N}
    @assert N <= D
    children = ntuple(i -> (i <= N ? child_idxs[i] : Int32(0)), Val(D))
    return _push_node!(arena, UInt8(N), false, zero(T), UInt16(0), UInt8(op), children)
end

"""Create a default node (a `0` constant leaf) in its own fresh arena."""
function ArenaNode{T,D}() where {T,D}
    arena = Arena{T,D}()
    idx = push_constant!(arena, zero(T))
    return ArenaNode{T,D}(arena, idx)
end

Base.@constprop :aggressive @inline function Base.getproperty(
    n::ArenaNode{T}, k::Symbol
) where {T}
    if k === :arena
        return getfield(n, :arena)
    elseif k === :idx
        return getfield(n, :idx)
    elseif k === :degree
        return @inbounds getfield(n, :arena).degree[getfield(n, :idx)]
    elseif k === :constant
        return @inbounds getfield(n, :arena).constant[getfield(n, :idx)]
    elseif k === :val
        return @inbounds getfield(n, :arena).val[getfield(n, :idx)]::T
    elseif k === :feature
        return @inbounds getfield(n, :arena).feature[getfield(n, :idx)]
    elseif k === :op
        return @inbounds getfield(n, :arena).op[getfield(n, :idx)]
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
    i = n.idx
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

@inline function _nullable_child(
    n::ArenaNode{T,D}, c::Int32
)::Nullable{ArenaNode{T,D}} where {T,D}
    child = ArenaNode{T,D}(n.arena, c)
    return Nullable{ArenaNode{T,D}}(c == 0, child)
end

"""Return an `NTuple{D,Nullable{ArenaNode}}` of children wrappers.

Unused slots are represented as poison nodes (mirroring `Node`), so that
accessing them throws an `UndefRefError`.
"""
@generated function unsafe_get_children(n::ArenaNode{T,D}) where {T,D}
    quote
        children = @inbounds getfield(n, :arena).children[getfield(n, :idx)]
        return Base.Cartesian.@ntuple($D, j -> _nullable_child(n, children[j]))
    end
end

@inline function get_child(n::ArenaNode{T,D}, i::Int) where {T,D}
    c = @inbounds n.arena.children[n.idx][i]
    c == 0 && throw(UndefRefError())
    return ArenaNode(n.arena, c)
end
@inline get_child(n::ArenaNode, i::Integer) = get_child(n, Int(i))

@inline function set_child!(n::ArenaNode{T,D}, child::AbstractNode{D}, i::Int) where {T,D}
    child isa AbstractExpressionNode{T,D} || throw(
        ArgumentError(
            "ArenaNode children must be AbstractExpressionNode{$T,$D} (got $(typeof(child)))",
        ),
    )

    # We cannot directly link across arenas, so we copy the subtree into `n`'s arena.
    idx = if child isa ArenaNode{T,D} && child.arena === n.arena
        child.idx
    else
        _copy_to_arena!(n.arena, child)
    end

    old = @inbounds n.arena.children[n.idx]
    @inbounds n.arena.children[n.idx] = Base.setindex(old, idx, i)
    return ArenaNode(n.arena, idx)
end

@inline function set_children!(
    n::ArenaNode{T,D}, children::Union{Tuple,AbstractVector{<:AbstractNode{D}}}
) where {T,D}
    D2 = length(children)
    idxs = _zero_children(Val(D))
    @inbounds for i in 1:min(D, D2)
        c = children[i]
        if c isa Nullable
            c.null && continue
            c = c[]
        end

        c isa AbstractExpressionNode{T,D} || throw(
            ArgumentError(
                "ArenaNode children must be AbstractExpressionNode{$T,$D} (got $(typeof(c)))",
            ),
        )

        idx = if c isa ArenaNode{T,D} && c.arena === n.arena
            c.idx
        else
            _copy_to_arena!(n.arena, c)
        end
        idxs = Base.setindex(idxs, idx, i)
    end

    @inbounds n.arena.children[n.idx] = idxs
    return nothing
end

"""Copy a tree into a new arena and return the new root node."""
function Base.copy(tree::ArenaNode{T,D}; break_sharing::Val{BS}=Val(false)) where {T,D,BS}
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

    idxs = _zero_children(Val(D))
    @inbounds for i in 1:d
        idxs = Base.setindex(idxs, _copy_to_arena!(arena, get_child(tree, i)), i)
    end
    return _push_node!(arena, UInt8(d), false, zero(T), UInt16(0), tree.op, idxs)
end

"""Convert an existing tree into an arena-backed representation.

This copies the entire tree into a fresh arena.
"""
@inline function Base.convert(
    ::Type{ArenaNode{T,D}}, tree::AbstractExpressionNode{T,D}
) where {T,D}
    arena = Arena{T,D}(; capacity=length(tree; break_sharing=Val(true)))
    idx = _copy_to_arena!(arena, tree)
    return ArenaNode{T,D}(arena, idx)
end
@inline function Base.convert(
    ::Type{ArenaNode{T}}, tree::AbstractExpressionNode{T,D}
) where {T,D}
    return convert(ArenaNode{T,D}, tree)
end

################################################################################
# Cursor + reusable stack (prototype)
################################################################################

"""A reusable traversal cursor for an [`Arena`](@ref).

This is the intended mechanism for allocation-free traversals/rewrites.
For now, it implements a simple *preorder* traversal using an explicit stack.

The stack is reusable: call [`reset!`](@ref) to traverse a new root without
reallocating the stack storage.
"""
struct ArenaCursor{T,D}
    arena::Arena{T,D}
    stack::Vector{Int32}

    function ArenaCursor(arena::Arena{T,D}; capacity::Integer=0) where {T,D}
        stack = sizehint!(Int32[], capacity)
        return new{T,D}(arena, stack)
    end
end

@inline function ArenaCursor(tree::ArenaNode{T,D}; capacity::Integer=0) where {T,D}
    return ArenaCursor(tree.arena; capacity=capacity)::ArenaCursor{T,D}
end

"""Reset the cursor stack to start a preorder traversal at `root`."""
@inline function reset!(c::ArenaCursor{T,D}, root::Int32) where {T,D}
    empty!(c.stack)
    push!(c.stack, root)
    return c
end
@inline reset!(c::ArenaCursor, root::ArenaNode) = reset!(c, root.idx)

"""Pop the next node in preorder (or return `nothing` when done)."""
function next!(c::ArenaCursor{T,D})::Nullable{ArenaNode{T,D}} where {T,D}
    if isempty(c.stack)
        return Nullable(true, ArenaNode{T,D}(c.arena, Int32(0)))
    end

    idx = pop!(c.stack)
    node = ArenaNode{T,D}(c.arena, idx)

    # Push children in reverse order so the leftmost child is visited next.
    d = @inbounds c.arena.degree[idx]
    if d != 0
        child_idxs = @inbounds c.arena.children[idx]
        @inbounds for i in d:-1:1
            child = child_idxs[i]
            child != 0 && push!(c.stack, child)
        end
    end

    return Nullable(false, node)
end

"""Traverse a tree in preorder using a reusable cursor."""
function foreach_preorder!(
    f::F, root::ArenaNode{T,D}, cursor::ArenaCursor{T,D}
) where {F,T,D}
    cursor.arena === root.arena ||
        throw(ArgumentError("Cursor arena does not match root arena"))

    reset!(cursor, root)
    while true
        maybe_n = next!(cursor)
        maybe_n.null && break
        f(maybe_n[])
    end
    return nothing
end

end
