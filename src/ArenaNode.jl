module ArenaNodeModule

using ..UtilsModule: Nullable

import ..NodeModule:
    AbstractNode,
    AbstractExpressionNode,
    Node,
    unsafe_get_children,
    get_child,
    set_child!,
    set_children!,
    count_nodes,
    copy_node
import ..NodeUtilsModule:
    count_constant_nodes,
    count_scalar_constants,
    has_constants,
    get_scalar_constants,
    set_scalar_constants!
import ..NodePreallocationModule: allocate_container, copy_into!
import ..ValueInterfaceModule: get_number_type

"""All per-node fields packed into a single isbits struct.

Storing nodes as one `Vector{ArenaEntry}` (array-of-structs) makes whole-tree
operations flat array operations: `copy` is a single `memcpy`, and traversals
touch one contiguous stream of memory.

Indices are `Int32` and are 1-based. A child index of `0` indicates an empty slot.
"""
struct ArenaEntry{T,D}
    val::T
    children::NTuple{D,Int32}
    feature::UInt16
    degree::UInt8
    op::UInt8
    constant::Bool
end

@inline function _replace(
    e::ArenaEntry{T,D};
    val=e.val,
    children=e.children,
    feature=e.feature,
    degree=e.degree,
    op=e.op,
    constant=e.constant,
) where {T,D}
    return ArenaEntry{T,D}(val, children, feature, degree, op, constant)
end

"""Array-backed arena storing the nodes of a tree contiguously.

This is an *experimental prototype* intended to provide an arena-backed representation
with a `Node`-like facade (`ArenaNode`) that supports existing tree algorithms that are
written against `AbstractExpressionNode`.

The `compact` flag tracks whether `nodes` is exactly one postfix-ordered tree
(children stored before parents, root last, no orphaned nodes and no shared
subtrees). Trees built via `convert`/`copy` are compact; structural mutations
through the facade may clear the flag, in which case whole-tree operations fall
back to generic traversals. A structural `copy` re-compacts.
"""
struct Arena{T,D}
    nodes::Vector{ArenaEntry{T,D}}
    compact::Base.RefValue{Bool}

    function Arena{T,D}(; capacity::Integer=0) where {T,D}
        return new{T,D}(sizehint!(ArenaEntry{T,D}[], capacity), Ref(true))
    end
    function Arena{T,D}(nodes::Vector{ArenaEntry{T,D}}, compact::Bool) where {T,D}
        return new{T,D}(nodes, Ref(compact))
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

"""Whether `tree` is the root of a compact arena, so that the arena contents
*are* the tree and whole-tree operations can act on the flat array directly."""
@inline function is_compact_root(tree::ArenaNode)
    a = getfield(tree, :arena)
    return a.compact[] && Int(getfield(tree, :idx)) == length(a.nodes)
end

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
    push!(arena.nodes, ArenaEntry{T,D}(val, children, feature, degree, op, constant))
    return Int32(length(arena.nodes))
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
        return @inbounds getfield(n, :arena).nodes[getfield(n, :idx)].degree
    elseif k === :constant
        return @inbounds getfield(n, :arena).nodes[getfield(n, :idx)].constant
    elseif k === :val
        return @inbounds getfield(n, :arena).nodes[getfield(n, :idx)].val::T
    elseif k === :feature
        return @inbounds getfield(n, :arena).nodes[getfield(n, :idx)].feature
    elseif k === :op
        return @inbounds getfield(n, :arena).nodes[getfield(n, :idx)].op
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
    a = n.arena
    i = n.idx
    e = @inbounds a.nodes[i]
    if k === :degree
        # Changing arity orphans or exposes child slots, so the flat layout can
        # no longer be assumed to be exactly this tree.
        UInt8(v) == e.degree || (a.compact[] = false)
        @inbounds a.nodes[i] = _replace(e; degree=UInt8(v))
        return v
    elseif k === :constant
        @inbounds a.nodes[i] = _replace(e; constant=Bool(v))
        return v
    elseif k === :val
        @inbounds a.nodes[i] = _replace(e; val=convert(T, v))
        return v
    elseif k === :feature
        @inbounds a.nodes[i] = _replace(e; feature=UInt16(v))
        return v
    elseif k === :op
        @inbounds a.nodes[i] = _replace(e; op=UInt8(v))
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
        children = @inbounds getfield(n, :arena).nodes[getfield(n, :idx)].children
        return Base.Cartesian.@ntuple($D, j -> _nullable_child(n, children[j]))
    end
end

@inline function get_child(n::ArenaNode{T,D}, i::Integer) where {T,D}
    c = @inbounds n.arena.nodes[n.idx].children[i]
    c == 0 && throw(UndefRefError())
    return ArenaNode(n.arena, c)
end

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

    a = n.arena
    e = @inbounds a.nodes[n.idx]
    if @inbounds(e.children[i]) != idx
        # Relinking orphans the old child subtree and may introduce sharing.
        a.compact[] = false
        @inbounds a.nodes[n.idx] = _replace(e; children=Base.setindex(e.children, idx, i))
    end
    return ArenaNode(a, idx)
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

    a = n.arena
    e = @inbounds a.nodes[n.idx]
    if e.children != idxs
        a.compact[] = false
        @inbounds a.nodes[n.idx] = _replace(e; children=idxs)
    end
    return nothing
end

"""Copy a tree into a new arena and return the new root node.

When `tree` is the root of a compact arena, this is a single flat copy of the
node array (child indices are arena-relative, so they remain valid verbatim).
Otherwise it falls back to a structural copy, which also re-compacts the
resulting arena.

This overloads `copy_node` (rather than `Base.copy`) since it is the generic
entry point: `Base.copy(::AbstractExpressionNode)` forwards here, and the
fallback `copy_node` would otherwise build a fresh arena per copied node via
`constructorof`.
"""
function copy_node(tree::ArenaNode{T,D}; break_sharing::Val{BS}=Val(false)) where {T,D,BS}
    if is_compact_root(tree)
        return ArenaNode{T,D}(Arena{T,D}(copy(tree.arena.nodes), true), tree.idx)
    end
    arena = Arena{T,D}(; capacity=length(tree; break_sharing=Val(true)))
    idx = _copy_to_arena!(arena, tree)
    return ArenaNode{T,D}(arena, idx)
end

"""Preallocate an arena for [`copy_into!`](@ref), enabling zero-allocation copies."""
function allocate_container(
    prototype::ArenaNode{T,D}, n::Union{Nothing,Integer}=nothing
) where {T,D}
    return Arena{T,D}(; capacity=@something(n, length(prototype)))
end

"""Copy `src` into the preallocated arena `dest`, reusing its storage.

This is the steady-state copy path for population-based search: no allocations
once `dest` has sufficient capacity.
"""
function copy_into!(
    dest::Arena{T,D},
    src::ArenaNode{T,D};
    ref::Union{Nothing,Base.RefValue{<:Integer}}=nothing,
) where {T,D}
    @assert dest !== src.arena
    if is_compact_root(src)
        nodes = src.arena.nodes
        resize!(dest.nodes, length(nodes))
        copyto!(dest.nodes, nodes)
        dest.compact[] = true
        return ArenaNode{T,D}(dest, src.idx)
    end
    empty!(dest.nodes)
    idx = _copy_to_arena!(dest, src)
    dest.compact[] = true
    return ArenaNode{T,D}(dest, idx)
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

This copies the entire tree into a fresh arena, in postfix (children-first) order.
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
# Flat whole-tree operations
#
# For a compact arena the node array *is* the tree, so tree-wide reductions
# become linear array scans with no pointer chasing. Each of these falls back
# to the generic traversal-based implementation when the invariant doesn't hold.
################################################################################

function count_nodes(tree::ArenaNode; break_sharing::Val{BS}=Val(false)) where {BS}
    if is_compact_root(tree)
        return length(tree.arena.nodes)
    end
    return invoke(count_nodes, Tuple{AbstractNode}, tree; break_sharing=Val(BS))::Int64
end

function count_constant_nodes(tree::ArenaNode)
    if is_compact_root(tree)
        return count(e -> e.degree == 0x00 && e.constant, tree.arena.nodes)
    end
    return invoke(count_constant_nodes, Tuple{AbstractExpressionNode}, tree)
end

function has_constants(tree::ArenaNode)
    if is_compact_root(tree)
        return any(e -> e.degree == 0x00 && e.constant, tree.arena.nodes)
    end
    return invoke(has_constants, Tuple{AbstractExpressionNode}, tree)
end

function count_scalar_constants(tree::ArenaNode{T}) where {T<:Number}
    if is_compact_root(tree)
        return count(e -> e.degree == 0x00 && e.constant, tree.arena.nodes)
    end
    return invoke(count_scalar_constants, Tuple{AbstractExpressionNode{T}}, tree)
end

"""Used by `NodeSampler` (random node selection in mutations), once per sample."""
function Base.count(
    f::F, tree::ArenaNode{T,D}; init=0, break_sharing::Val{BS}=Val(false)
) where {F<:Function,T,D,BS}
    if is_compact_root(tree)
        a = tree.arena
        c = init
        @inbounds for i in 1:length(a.nodes)
            c += f(ArenaNode{T,D}(a, Int32(i))) ? 1 : 0
        end
        return c
    end
    return invoke(Base.count, Tuple{F,AbstractNode}, f, tree; init, break_sharing=Val(BS))
end

"""For compact arenas, constants are gathered by a linear scan, and the
returned `refs` are plain arena indices (which also remain valid in flat
copies of the tree)."""
function get_scalar_constants(
    tree::ArenaNode{T}, ::Type{BT}=get_number_type(T)
) where {T<:Number,BT}
    if is_compact_root(tree)
        nodes = tree.arena.nodes
        n_constants = count(e -> e.degree == 0x00 && e.constant, nodes)
        vals = Vector{T}(undef, n_constants)
        refs = Vector{Int32}(undef, n_constants)
        j = 0
        @inbounds for i in eachindex(nodes)
            e = nodes[i]
            if e.degree == 0x00 && e.constant
                j += 1
                vals[j] = e.val
                refs[j] = Int32(i)
            end
        end
        return vals, refs
    end
    return invoke(get_scalar_constants, Tuple{AbstractExpressionNode{T},Type{BT}}, tree, BT)
end

function set_scalar_constants!(
    tree::ArenaNode{T}, constants, refs::AbstractVector{Int32}
) where {T<:Number}
    nodes = tree.arena.nodes
    @inbounds for j in eachindex(refs, constants)
        i = refs[j]
        nodes[i] = _replace(nodes[i]; val=constants[j]::T)
    end
    return nothing
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
    e = @inbounds c.arena.nodes[idx]
    if e.degree != 0
        @inbounds for i in (e.degree):-1:1
            child = e.children[i]
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
