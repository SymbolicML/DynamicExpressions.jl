module ArenaNodeModule

using ..UtilsModule: Nullable, Undefined, ResultOk

import ..NodeModule:
    AbstractNode,
    AbstractExpressionNode,
    Node,
    unsafe_get_children,
    get_child,
    set_child!,
    set_children!,
    count_nodes,
    copy_node,
    filter_map,
    tree_mapreduce
import ..NodeUtilsModule:
    get_scalar_constants, set_scalar_constants!, is_node_constant, is_constant
import ..NodePreallocationModule: allocate_container, copy_into!
import ..ValueInterfaceModule: get_number_type, is_valid, is_valid_array
import ..OperatorEnumModule: OperatorEnum
import ..EvaluateModule: _eval_tree_array, EvalOptions, ArrayBuffer, get_nops

"""All per-node fields packed into a single isbits struct.

Storing nodes as one `Vector{ArenaEntry}` (array-of-structs) makes whole-tree
operations flat array operations: `copy` is a single `memcpy`, and traversals
touch one contiguous stream of memory.

Indices are `Int32` and are 1-based. A child index of `0` indicates an empty slot.
"""
struct ArenaEntry{T<:Number,D}
    val::T
    children::NTuple{D,Int32}
    feature::UInt16
    degree::UInt8
    op::UInt8
    constant::Bool
end

function _replace(
    entry::ArenaEntry{T,D};
    val=entry.val,
    children=entry.children,
    feature=entry.feature,
    degree=entry.degree,
    op=entry.op,
    constant=entry.constant,
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

`Arena` implements the (1-based, linear) array interface over its entries, and
this is the only sanctioned way to mutate entries: `setindex!` compares the old
and new entry and automatically clears `compact` whenever the structural fields
(`degree`, `children`) change. Non-structural writes (`val`/`op`/`feature`/
`constant`) preserve the flag, which keeps constant optimization on the fast
paths. Do not write `arena.nodes` directly outside this file's bulk-copy
internals.
"""
struct Arena{T<:Number,D} <: AbstractVector{ArenaEntry{T,D}}
    nodes::Vector{ArenaEntry{T,D}}
    compact::Base.RefValue{Bool}

    function Arena{T,D}(; capacity::Integer=0) where {T,D}
        return new{T,D}(sizehint!(ArenaEntry{T,D}[], capacity), Ref(true))
    end
    function Arena{T,D}(nodes::Vector{ArenaEntry{T,D}}, compact::Bool) where {T,D}
        return new{T,D}(nodes, Ref(compact))
    end
end

# Mark/clear the one-postfix-tree invariant (root last, no orphans):
mark_compact!(arena::Arena) = (arena.compact[]=true; arena)
invalidate_compact!(arena::Arena) = (arena.compact[]=false; arena)
is_compact(arena::Arena) = arena.compact[]

Base.size(arena::Arena) = size(arena.nodes)
Base.IndexStyle(::Type{<:Arena}) = IndexLinear()
Base.@propagate_inbounds Base.getindex(arena::Arena, i::Integer) = arena.nodes[i]
Base.@propagate_inbounds function Base.setindex!(
    arena::Arena{T,D}, entry::ArenaEntry{T,D}, i::Integer
) where {T,D}
    nodes = arena.nodes
    old = nodes[i]
    if entry.degree != old.degree || entry.children != old.children
        invalidate_compact!(arena)
    end
    nodes[i] = entry
    return arena
end
function Base.push!(arena::Arena{T,D}, entry::ArenaEntry{T,D}) where {T,D}
    nodes = arena.nodes
    # A single leaf in a fresh arena is a valid tree; any further append breaks
    # the one-postfix-tree invariant until a builder re-establishes it.
    isempty(nodes) || invalidate_compact!(arena)
    push!(nodes, entry)
    return arena
end
function Base.sizehint!(arena::Arena, capacity::Integer)
    sizehint!(arena.nodes, capacity)
    return arena
end

"""A lightweight facade for a node stored in an [`Arena`](@ref).

This wrapper is intentionally minimal: it stores an arena reference and an index.
Core fields are accessed and mutated via `getproperty`/`setproperty!`.

!!! warning
    Unlike `Node`, attaching a child from a *different* arena
    (`set_child!`/`set_children!`, including keyword construction) copies the
    subtree into the parent's arena: the original handle stays attached to its
    own arena, so later mutations through it do not affect the new parent.
    Same-arena attachments keep reference semantics.
"""
struct ArenaNode{T<:Number,D} <: AbstractExpressionNode{T,D}
    arena::Arena{T,D}
    idx::Int32

    function ArenaNode{T,D}(arena::Arena{T,D}, idx::Int32) where {T,D}
        return new{T,D}(arena, idx)
    end
end

ArenaNode(arena::Arena{T,D}, idx::Int32) where {T,D} = ArenaNode{T,D}(arena, idx)

# The only sanctioned `getfield` sites: internal code uses these instead of
# property access so that functions reachable from `getproperty` (e.g.
# `get_child` via the `:l`/`:r` branches) do not cycle back into it.
get_arena(node::ArenaNode) = getfield(node, :arena)
get_index(node::ArenaNode) = getfield(node, :idx)

# True when the arena contents *are* `tree`: compact, rooted at the last entry.
function is_compact_root(tree::ArenaNode)
    arena = get_arena(tree)
    return is_compact(arena) && get_index(tree) == length(arena.nodes)
end

_zero_children(::Val{D}) where {D} = ntuple(_ -> Int32(0), Val(D))

function _push_node!(
    arena::Arena{T,D};
    degree::UInt8=UInt8(0),
    constant::Bool=false,
    val::T=zero(T),
    feature::UInt16=UInt16(0),
    op::UInt8=UInt8(0),
    children::NTuple{D,Int32}=_zero_children(Val(D)),
) where {T,D}
    push!(arena, ArenaEntry{T,D}(val, children, feature, degree, op, constant))
    return Int32(length(arena))
end

function push_constant!(arena::Arena{T,D}, value) where {T,D}
    return _push_node!(arena; constant=true, val=convert(T, value))
end

function push_feature!(arena::Arena{T,D}, feature::Integer) where {T,D}
    return _push_node!(arena; feature=UInt16(feature))
end

# Default node: a zero constant leaf in its own fresh arena.
function ArenaNode{T,D}() where {T,D}
    arena = Arena{T,D}()
    idx = push_constant!(arena, zero(T))
    return ArenaNode{T,D}(arena, idx)
end

Base.@constprop :aggressive function Base.getproperty(
    node::ArenaNode{T}, property_name::Symbol
) where {T}
    if property_name === :arena
        return get_arena(node)
    elseif property_name === :idx
        return get_index(node)
    elseif property_name === :children
        return unsafe_get_children(node)
    elseif property_name === :l
        return get_child(node, UInt8(1))
    elseif property_name === :r
        return get_child(node, UInt8(2))
    end
    entry = @inbounds get_arena(node).nodes[get_index(node)]
    if property_name === :degree
        return entry.degree
    elseif property_name === :constant
        return entry.constant
    elseif property_name === :val
        return entry.val::T
    elseif property_name === :feature
        return entry.feature
    elseif property_name === :op
        return entry.op
    else
        return getfield(node, property_name)
    end
end

@inline function Base.setproperty!(
    node::ArenaNode{T,D}, property_name::Symbol, value
) where {T,D}
    arena = get_arena(node)
    i = get_index(node)
    entry = @inbounds arena[i]
    if property_name === :degree
        @inbounds arena[i] = _replace(entry; degree=UInt8(value))
    elseif property_name === :constant
        @inbounds arena[i] = _replace(entry; constant=Bool(value))
    elseif property_name === :val
        @inbounds arena[i] = _replace(entry; val=convert(T, value))
    elseif property_name === :feature
        @inbounds arena[i] = _replace(entry; feature=UInt16(value))
    elseif property_name === :op
        @inbounds arena[i] = _replace(entry; op=UInt8(value))
    elseif property_name === :l
        set_child!(node, value, 1)
    elseif property_name === :r
        set_child!(node, value, 2)
    else
        throw(ArgumentError("Unsupported field $property_name for ArenaNode"))
    end
    return value
end

function _nullable_child(
    node::ArenaNode{T,D}, child_idx::Int32
)::Nullable{ArenaNode{T,D}} where {T,D}
    child = ArenaNode{T,D}(get_arena(node), child_idx)
    return Nullable{ArenaNode{T,D}}(iszero(child_idx), child)
end

# Children as `Nullable` wrappers; unused slots are poison nodes (like `Node`),
# so accessing them throws an `UndefRefError`.
@generated function unsafe_get_children(node::ArenaNode{T,D}) where {T,D}
    quote
        $(Expr(:meta, :inline))
        children = @inbounds get_arena(node).nodes[get_index(node)].children
        return Base.Cartesian.@ntuple($D, j -> _nullable_child(node, children[j]))
    end
end

function get_child(node::ArenaNode{T,D}, i::Integer) where {T,D}
    # Avoid routing through getproperty here: the :l/:r property branches call
    # get_child, and the resulting inference cycle widens property access.
    arena = get_arena(node)
    entry = @inbounds arena.nodes[get_index(node)]
    child_idx = entry.children[i]  # bounds-checked: i > D must throw, not crash
    iszero(child_idx) && throw(UndefRefError())
    return ArenaNode{T,D}(arena, child_idx)
end

# Same-arena children attach by index; anything else is copied into `node`'s
# arena, since arenas cannot link across each other.
function _resolve_child_index!(node::ArenaNode{T,D}, child) where {T,D}
    child isa AbstractExpressionNode{T,D} || throw(
        ArgumentError(
            "ArenaNode children must be AbstractExpressionNode{$T,$D} (got $(typeof(child)))",
        ),
    )
    if child isa ArenaNode{T,D} && child.arena === node.arena
        return child.idx
    else
        return _copy_to_arena!(node.arena, child)
    end
end

function set_child!(node::ArenaNode{T,D}, child::AbstractNode{D}, i::Int) where {T,D}
    idx = _resolve_child_index!(node, child)
    arena = node.arena
    entry = @inbounds arena[node.idx]
    if @inbounds(entry.children[i]) != idx
        @inbounds arena[node.idx] = _replace(
            entry; children=Base.setindex(entry.children, idx, i)
        )
    end
    return ArenaNode{T,D}(arena, idx)
end

function set_children!(
    node::ArenaNode{T,D}, children::Union{Tuple,AbstractVector{<:AbstractNode{D}}}
) where {T,D}
    D2 = length(children)
    idxs = _zero_children(Val(D))
    @inbounds for i in 1:min(D, D2)
        child = children[i]
        if child isa Nullable
            child.null && continue
            child = child[]
        end
        idxs = Base.setindex(idxs, _resolve_child_index!(node, child), i)
    end

    arena = node.arena
    entry = @inbounds arena[node.idx]
    @inbounds arena[node.idx] = _replace(entry; children=idxs)
    return nothing
end

# Compact arenas copy as one flat array copy (child indices stay valid
# verbatim); otherwise fall back to a structural copy, which re-compacts.
# Overloads `copy_node` (not `Base.copy`) since that is the generic entry point.
function copy_node(tree::ArenaNode{T,D}; break_sharing::Val{BS}=Val(false)) where {T,D,BS}
    if is_compact_root(tree)
        return ArenaNode{T,D}(Arena{T,D}(copy(tree.arena.nodes), true), tree.idx)
    end
    return convert(ArenaNode{T,D}, tree)
end

# Preallocated arena for `copy_into!`, enabling zero-allocation copies.
function allocate_container(
    prototype::ArenaNode{T,D}, num_nodes::Union{Nothing,Integer}=nothing
) where {T,D}
    return Arena{T,D}(; capacity=@something(num_nodes, length(prototype)))
end

# Steady-state copy for population search: reuse `dest`'s storage, with no
# allocations once it has sufficient capacity.
function copy_into!(
    dest::Arena{T,D},
    src::ArenaNode{T,D};
    ref::Union{Nothing,Base.RefValue{<:Integer}}=nothing,
) where {T,D}
    if dest === get_arena(src)
        # Container reuse: the tree already lives in `dest`. A compact root is
        # a no-op; otherwise compact through a temporary copy.
        is_compact_root(src) && return src
        return copy_into!(dest, copy_node(src); ref)
    end
    if is_compact_root(src)
        nodes = src.arena.nodes
        resize!(dest.nodes, length(nodes))
        copyto!(dest.nodes, nodes)
        mark_compact!(dest)
        return ArenaNode{T,D}(dest, src.idx)
    end
    empty!(dest.nodes)
    idx = _copy_to_arena!(dest, src)
    mark_compact!(dest)
    return ArenaNode{T,D}(dest, idx)
end

function _copy_to_arena!(
    arena::Arena{T,D}, tree::AbstractExpressionNode{T2,D}
) where {T,T2,D}
    degree = tree.degree
    if degree == 0
        if tree.constant
            return push_constant!(arena, tree.val)
        else
            return push_feature!(arena, tree.feature)
        end
    end

    idxs = _zero_children(Val(D))
    @inbounds for i in 1:degree
        idxs = Base.setindex(idxs, _copy_to_arena!(arena, get_child(tree, i)), i)
    end
    return _push_node!(arena; degree=UInt8(degree), op=tree.op, children=idxs)
end

# Copy the tree into a fresh arena, in postfix (children-first) order.
function Base.convert(
    ::Type{ArenaNode{T,D}}, tree::AbstractExpressionNode{T2,D}
) where {T,T2,D}
    arena = Arena{T,D}(; capacity=length(tree; break_sharing=Val(true)))
    idx = _copy_to_arena!(arena, tree)
    mark_compact!(arena)
    return ArenaNode{T,D}(arena, idx)
end
function Base.convert(
    ::Type{ArenaNode{T}}, tree::AbstractExpressionNode{T2,D}
) where {T,T2,D}
    return convert(ArenaNode{T,D}, tree)
end

# Cross-representation comparisons (`==` promotes its arguments) promote
# toward the arena representation.
function Base.promote_rule(::Type{ArenaNode{T1,D}}, ::Type{Node{T2,D}}) where {T1,T2,D}
    return ArenaNode{promote_type(T1, T2),D}
end
function Base.promote_rule(::Type{ArenaNode{T1,D}}, ::Type{ArenaNode{T2,D}}) where {T1,T2,D}
    return ArenaNode{promote_type(T1, T2),D}
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

function Base.any(f::F, tree::ArenaNode{T,D}) where {F<:Function,T,D}
    return _arena_any(f, get_arena(tree), get_index(tree))
end
function _arena_any(f::F, arena::Arena{T,D}, idx::Int32) where {F<:Function,T,D}
    iszero(idx) && throw(UndefRefError())  # unset child slot, like Node
    entry = @inbounds arena.nodes[idx]
    @inline(f(ArenaNode{T,D}(arena, idx))) && return true
    @inbounds for j in 1:entry.degree
        _arena_any(f, arena, entry.children[j]) && return true
    end
    return false
end

function is_constant(tree::ArenaNode)
    return !_arena_any(
        node -> iszero(node.degree) && !node.constant, get_arena(tree), get_index(tree)
    )
end

function tree_mapreduce(
    f_leaf::F1,
    f_branch::F2,
    op::G,
    tree::ArenaNode{T,D},
    result_type::Type{RT}=Undefined;
    f_on_shared::H=(result, is_shared) -> result,
    break_sharing::Val{BS}=Val(false),
) where {T,D,F1<:Function,F2<:Function,G<:Function,H<:Function,RT,BS}
    return _arena_mapreduce(f_leaf, f_branch, op, get_arena(tree), get_index(tree))
end

@generated function _arena_mapreduce(
    f_leaf::F1, f_branch::F2, op::G, arena::Arena{T,D}, idx::Int32
) where {F1<:Function,F2<:Function,G<:Function,T,D}
    quote
        iszero(idx) && throw(UndefRefError())  # unset child slot, like Node
        entry = @inbounds arena.nodes[idx]
        degree = entry.degree
        if iszero(degree)
            return f_leaf(ArenaNode{T,D}(arena, idx))
        end
        branch = f_branch(ArenaNode{T,D}(arena, idx))
        children = entry.children
        return Base.Cartesian.@nif(
            $D,
            i -> i == degree,  # COV_EXCL_LINE
            i -> Base.Cartesian.@ncall(
                i,
                op,
                branch,
                j -> _arena_mapreduce(f_leaf, f_branch, op, arena, children[j])
            ),
        )
    end
end

# Constants as plain Int32 arena indices (also valid in flat copies): a
# linear scan when compact, a facade traversal otherwise.
function get_scalar_constants(
    tree::ArenaNode{T}, ::Type{BT}=get_number_type(T)
) where {T<:Number,BT}
    arena = tree.arena
    if is_compact_root(tree)
        nodes = arena.nodes
        n_constants = count(entry -> iszero(entry.degree) && entry.constant, nodes)
        vals = Vector{T}(undef, n_constants)
        refs = Vector{Int32}(undef, n_constants)
        j = 0
        @inbounds for i in eachindex(nodes)
            entry = nodes[i]
            if iszero(entry.degree) && entry.constant
                j += 1
                vals[j] = entry.val
                refs[j] = Int32(i)
            end
        end
        return vals, refs
    end
    refs = filter_map(is_node_constant, node -> node.idx, tree, Int32)
    vals = T[@inbounds(arena[i].val) for i in refs]
    return vals, refs
end

function set_scalar_constants!(
    tree::ArenaNode{T}, constants, refs::AbstractVector{Int32}
) where {T<:Number}
    arena = tree.arena
    @inbounds for j in eachindex(refs, constants)
        i = refs[j]
        arena[i] = _replace(arena[i]; val=convert(T, constants[j]))
    end
    return nothing
end

include("ArenaNodeEval.jl")

end
