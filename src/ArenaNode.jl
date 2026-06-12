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

@inline function _replace(
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

"""Mark the arena as holding exactly one postfix-ordered tree (root last)."""
@inline mark_compact!(arena::Arena) = (arena.compact[]=true; arena)
"""Record that the one-postfix-tree invariant may no longer hold."""
@inline invalidate_compact!(arena::Arena) = (arena.compact[]=false; arena)
@inline is_compact(arena::Arena) = arena.compact[]

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

    @inline function ArenaNode{T,D}(arena::Arena{T,D}, idx::Int32) where {T,D}
        return new{T,D}(arena, idx)
    end
end

@inline ArenaNode(arena::Arena{T,D}, idx::Int32) where {T,D} = ArenaNode{T,D}(arena, idx)

"""Raw accessors for the facade's two fields, and the only sanctioned
`getfield` call sites. Internal code uses these instead of property access so
that functions reachable from `getproperty` (e.g. `get_child` via the
`:l`/`:r` branches) do not create an inference cycle through it."""
@inline get_arena(node::ArenaNode) = getfield(node, :arena)
@inline get_index(node::ArenaNode) = getfield(node, :idx)

"""Whether `tree` is the root of a compact arena, so that the arena contents
*are* the tree and whole-tree operations can act on the flat array directly."""
@inline function is_compact_root(tree::ArenaNode)
    arena = get_arena(tree)
    return is_compact(arena) && get_index(tree) == length(arena.nodes)
end

@inline function _zero_children(::Val{D}) where {D}
    return ntuple(_ -> Int32(0), Val(D))
end

@inline function _push_node!(
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

@inline function push_constant!(arena::Arena{T,D}, value) where {T,D}
    return _push_node!(arena; constant=true, val=convert(T, value))
end

@inline function push_feature!(arena::Arena{T,D}, feature::Integer) where {T,D}
    return _push_node!(arena; feature=UInt16(feature))
end

"""Create a default node (a `0` constant leaf) in its own fresh arena."""
function ArenaNode{T,D}() where {T,D}
    arena = Arena{T,D}()
    idx = push_constant!(arena, zero(T))
    return ArenaNode{T,D}(arena, idx)
end

Base.@constprop :aggressive @inline function Base.getproperty(
    node::ArenaNode{T}, property_name::Symbol
) where {T}
    if property_name === :arena
        return get_arena(node)
    elseif property_name === :idx
        return get_index(node)
    elseif property_name === :degree
        return @inbounds get_arena(node).nodes[get_index(node)].degree
    elseif property_name === :constant
        return @inbounds get_arena(node).nodes[get_index(node)].constant
    elseif property_name === :val
        return @inbounds get_arena(node).nodes[get_index(node)].val::T
    elseif property_name === :feature
        return @inbounds get_arena(node).nodes[get_index(node)].feature
    elseif property_name === :op
        return @inbounds get_arena(node).nodes[get_index(node)].op
    elseif property_name === :children
        return unsafe_get_children(node)
    elseif property_name === :l
        return get_child(node, UInt8(1))
    elseif property_name === :r
        return get_child(node, UInt8(2))
    else
        return getfield(node, property_name)
    end
end

@inline function Base.setproperty!(
    node::ArenaNode{T,D}, property_name::Symbol, value
) where {T,D}
    arena = node.arena
    i = node.idx
    entry = @inbounds arena[i]
    if property_name === :degree
        @inbounds arena[i] = _replace(entry; degree=UInt8(value))
        return value
    elseif property_name === :constant
        @inbounds arena[i] = _replace(entry; constant=Bool(value))
        return value
    elseif property_name === :val
        @inbounds arena[i] = _replace(entry; val=convert(T, value))
        return value
    elseif property_name === :feature
        @inbounds arena[i] = _replace(entry; feature=UInt16(value))
        return value
    elseif property_name === :op
        @inbounds arena[i] = _replace(entry; op=UInt8(value))
        return value
    elseif property_name === :l
        set_child!(node, value, 1)
        return value
    elseif property_name === :r
        set_child!(node, value, 2)
        return value
    else
        throw(ArgumentError("Unsupported field $property_name for ArenaNode"))
    end
end

@inline function _nullable_child(
    node::ArenaNode{T,D}, child_idx::Int32
)::Nullable{ArenaNode{T,D}} where {T,D}
    child = ArenaNode{T,D}(node.arena, child_idx)
    return Nullable{ArenaNode{T,D}}(iszero(child_idx), child)
end

"""Return an `NTuple{D,Nullable{ArenaNode}}` of children wrappers.

Unused slots are represented as poison nodes (mirroring `Node`), so that
accessing them throws an `UndefRefError`.
"""
@generated function unsafe_get_children(node::ArenaNode{T,D}) where {T,D}
    quote
        $(Expr(:meta, :inline))
        children = @inbounds get_arena(node).nodes[get_index(node)].children
        return Base.Cartesian.@ntuple($D, j -> _nullable_child(node, children[j]))
    end
end

@inline function get_child(node::ArenaNode{T,D}, i::Integer) where {T,D}
    # Avoid routing through getproperty here: the :l/:r property branches call
    # get_child, and the resulting inference cycle widens property access.
    arena = get_arena(node)
    entry = @inbounds arena.nodes[get_index(node)]
    child_idx = entry.children[i]  # bounds-checked: i > D must throw, not crash
    iszero(child_idx) && throw(UndefRefError())
    return ArenaNode{T,D}(arena, child_idx)
end

"""Arena index for attaching `child` under `node`: a same-arena child is
linked by its existing index; anything else (a `Node`, or an `ArenaNode` from
a different arena) is copied into `node`'s arena, since arenas cannot link
across each other."""
@inline function _resolve_child_index!(node::ArenaNode{T,D}, child) where {T,D}
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

@inline function set_child!(
    node::ArenaNode{T,D}, child::AbstractNode{D}, i::Int
) where {T,D}
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

@inline function set_children!(
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
    return convert(ArenaNode{T,D}, tree)
end

"""Preallocate an arena for [`copy_into!`](@ref), enabling zero-allocation copies."""
function allocate_container(
    prototype::ArenaNode{T,D}, num_nodes::Union{Nothing,Integer}=nothing
) where {T,D}
    return Arena{T,D}(; capacity=@something(num_nodes, length(prototype)))
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

"""Convert an existing tree into an arena-backed representation.

This copies the entire tree into a fresh arena, in postfix (children-first) order.
"""
@inline function Base.convert(
    ::Type{ArenaNode{T,D}}, tree::AbstractExpressionNode{T2,D}
) where {T,T2,D}
    arena = Arena{T,D}(; capacity=length(tree; break_sharing=Val(true)))
    idx = _copy_to_arena!(arena, tree)
    mark_compact!(arena)
    return ArenaNode{T,D}(arena, idx)
end
@inline function Base.convert(
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

"""Constants are gathered as plain `Int32` arena indices (which also remain
valid in flat copies of the tree): a linear scan for compact arenas, and a
facade traversal otherwise."""
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

################################################################################
# Plan-style buffered evaluation
################################################################################

# Plan-style postfix evaluation into the caller-provided `EvalOptions.buffer`.
# The buffer is a flat pool of contiguous `n_rows` slots (slot 1 is the
# output), each used feature materializes once into a permanent slot,
# intermediates are register-allocated with a free list, and constant subtrees
# fold in a scalar lane. Falls back to the generic recursive evaluator when
# there is no buffer, the buffer is too small or mismatched, the arena is not
# compact, `T` is not isbits (the branchless kernels issue dead loads from
# unwritten slots), depth or feature count exceeds 64, turbo is requested, or
# `use_fused=Val(false)` (callers may overload `deg1_eval` etc., which this
# path bypasses). Note the plan addresses the whole pool directly and does not
# advance `buffer.index`.
function _eval_tree_array(
    tree::ArenaNode{T,D},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    eval_options::EvalOptions,
)::ResultOk where {T<:Number,D}
    buffer = eval_options.buffer
    if buffer isa ArrayBuffer{Matrix{T}} &&
        isbitstype(T) &&
        cX isa Matrix{T} &&
        size(buffer.array, 2) == size(cX, 2) &&
        is_compact_root(tree) &&
        eval_options.turbo isa Val{false} &&
        eval_options.use_fused isa Val{true}
        ok_plan, num_slots, max_stack, feature_mask = _plan_scratch(get_arena(tree))
        # +1 for the output slot; capacity is the buffer's row count
        if ok_plan && num_slots + 1 <= size(buffer.array, 1)
            return _arena_eval(
                get_arena(tree),
                cX,
                operators,
                eval_options.early_exit,
                num_slots,
                max_stack,
                feature_mask,
                buffer.array,
            )
        end
    end
    return invoke(
        _eval_tree_array,
        Tuple{AbstractExpressionNode{T,D},AbstractMatrix{T},OperatorEnum,EvalOptions},
        tree,
        cX,
        operators,
        eval_options,
    )
end

"""Pool slot holding materialized `feature`: slot 1 is the output, and used
features occupy slots 2, 3, ... in ascending feature order."""
@inline function _feature_slot(feature_mask::UInt64, feature::Integer)
    return count_ones(feature_mask & (_feature_bit(feature) - 1)) + 2
end

"""Kind of an evaluation-stack operand: a constant folded into the scalar
lane, a pinned pool slot (the output or a materialized feature; never
recycled), or a scratch slot holding an intermediate (recycled once
consumed)."""
@enum OperandKind::UInt8 FoldedConstant PinnedSlot ScratchSlot

# The planner (`_plan_scratch`) and the executor (`_push_leaf!`/`_exec_op!`)
# walk the same postfix program, so they must make identical kind and
# recycling decisions: the executor trusts the planner's slot counts under
# `@inbounds`. These three functions are the single source of that policy.

"""Kind of the descriptor a leaf pushes: constants fold into the scalar lane,
features live in permanent slots."""
@inline _leaf_kind(entry::ArenaEntry) = entry.constant ? FoldedConstant : PinnedSlot

"""Kind of the descriptor an operator pushes: an all-scalar application
constant-folds into the scalar lane, anything else lands in a recyclable
intermediate slot."""
@inline _op_result_kind(all_args_scalar::Bool) =
    all_args_scalar ? FoldedConstant : ScratchSlot

"""Whether consuming an operand of this kind frees its slot for reuse."""
@inline _is_recyclable(kind::OperandKind) = kind == ScratchSlot

# A stack descriptor is an Int64 packing a kind (low 2 bits) with a slot
# index; scalar descriptors carry no slot (their value lives in the scalar
# lane). `_feature_bit` is the feature's position in the `feature_mask`
# bitset of used features.
@inline _pack_descriptor(kind::OperandKind, slot::Integer=0) =
    Int64(UInt8(kind)) | (Int64(slot) << 2)
@inline _descriptor_kind(descriptor::Int64) = OperandKind(UInt8(descriptor & 3))
@inline _descriptor_slot(descriptor::Int64) = Int32(descriptor >> 2)
@inline _feature_bit(feature::Integer) = UInt64(1) << (feature - 1)

"""Alloc-free stack of descriptor kinds for the planner: two bitmask lanes
(bit 1 = top of stack) record whether each entry is `FoldedConstant` or
`PinnedSlot`; an entry in neither lane is `ScratchSlot`. Capacity is 64 entries."""
struct KindStack
    scalar::UInt64
    permanent::UInt64
end

@inline function _push_kind(kinds::KindStack, kind::OperandKind)
    return KindStack(
        (kinds.scalar << 1) | (kind == FoldedConstant),
        (kinds.permanent << 1) | (kind == PinnedSlot),
    )
end
@inline function _pop_kinds(kinds::KindStack, count::UInt8)
    return KindStack(kinds.scalar >> count, kinds.permanent >> count)
end
@inline function _args_all_scalar(kinds::KindStack, degree::UInt8)
    arity_mask = (UInt64(1) << degree) - 1
    return (kinds.scalar & arity_mask) == arity_mask
end
@inline function _count_recyclable_args(kinds::KindStack, degree::UInt8)
    arity_mask = (UInt64(1) << degree) - 1
    return count_ones(~kinds.scalar & ~kinds.permanent & arity_mask)
end

"""Alloc-free pre-pass: find which features are used and simulate the
descriptor stack to count the recyclable intermediate slots (register
allocation with a free list). The simulation makes the *same* kind and
recycling decisions as the executor — both sides call `_leaf_kind`,
`_op_result_kind`, and `_is_recyclable` — so the returned slot counts are
exact. Kinds live in a `KindStack`, so trees deeper than 64 or features
beyond 64 report failure and take the generic path."""
function _plan_scratch(arena::Arena{T,D}) where {T,D}
    nodes = arena.nodes
    feature_mask = UInt64(0)
    kinds = KindStack(0, 0)
    stack_top = 0
    max_stack = 0
    num_live = 0
    num_free = 0
    max_live_intermediates = 0
    @inbounds for i in eachindex(nodes)
        entry = nodes[i]
        degree = entry.degree
        if iszero(degree)
            stack_top >= 64 && return (false, 0, 0, UInt64(0))
            stack_top += 1
            max_stack = max(max_stack, stack_top)
            kind = _leaf_kind(entry)
            if kind == PinnedSlot
                feature = entry.feature
                (1 <= feature <= 64) || return (false, 0, 0, UInt64(0))
                feature_mask |= _feature_bit(feature)
            end
            kinds = _push_kind(kinds, kind)
        else
            result_kind = _op_result_kind(_args_all_scalar(kinds, degree))
            if _is_recyclable(result_kind)
                num_free += _count_recyclable_args(kinds, degree)
                if num_free > 0
                    num_free -= 1
                else
                    num_live += 1
                    max_live_intermediates = max(max_live_intermediates, num_live)
                end
            end
            kinds = _push_kind(_pop_kinds(kinds, degree), result_kind)
            stack_top -= degree - 1
        end
    end
    num_slots = count_ones(feature_mask) + max_live_intermediates
    # A well-formed postfix tree collapses the stack to exactly the root;
    # anything else (e.g. orphaned roots) must fail closed.
    return (stack_top == 1, num_slots, max_stack, feature_mask)
end

@generated function _scalar_degn(
    ::Val{A}, op_idx::UInt8, args::NTuple{A,T}, operators::O
) where {A,T,O<:OperatorEnum}
    nops = get_nops(O, Val(A))
    nops == 0 && return :(throw(ArgumentError("no operators of arity " * string($A))))
    return quote
        Base.Cartesian.@nif(
            $nops,
            i -> i == op_idx,  # COV_EXCL_LINE
            i -> (Base.Cartesian.@ncall($A, operators.ops[$A][i], k -> args[k]))::T,
        )
    end
end

"""Branchless arity-generic kernel: each operand is selected per element with
`ifelse` between its scalar value and its pool slot (scalar operands carry
offset 0, so the dead load stays in cache). This avoids generating 2^arity
kernel variants while remaining SIMD-friendly."""
@generated function _kern_n!(
    pool::Matrix{T},
    dest_offset::Int,
    op::F,
    is_scalar::NTuple{A,Bool},
    scalar_args::NTuple{A,T},
    offsets::NTuple{A,Int},
    num_rows::Int,
) where {T,F,A}
    quote
        @inbounds @simd for j in 1:num_rows
            pool[dest_offset + j] = Base.Cartesian.@ncall(
                $A, op, k -> ifelse(is_scalar[k], scalar_args[k], pool[offsets[k] + j])
            )
        end
        return nothing
    end
end
"""`is_valid_array` over a pool slot without constructing a view."""
@inline function _valid_slot(pool::Matrix{T}, offset::Int, num_rows::Int) where {T}
    total = zero(T)
    @inbounds @simd for j in 1:num_rows
        total += pool[offset + j]
    end
    return is_valid(total)
end
@inline _slot_offset(slot::Int32, nrows::Int) = (slot - 1) * nrows

@generated function _dispatch_degn!(
    ::Val{A},
    pool::Matrix{T},
    dest_offset::Int,
    op_idx::UInt8,
    is_scalar::NTuple{A,Bool},
    scalar_args::NTuple{A,T},
    offsets::NTuple{A,Int},
    nrows::Int,
    operators::O,
) where {A,T,O<:OperatorEnum}
    nops = get_nops(O, Val(A))
    nops == 0 && return :(throw(ArgumentError("no operators of arity " * string($A))))
    quote
        Base.Cartesian.@nif(
            $nops,
            i -> i == op_idx,  # COV_EXCL_LINE
            i -> _kern_n!(
                pool,
                dest_offset,
                operators.ops[$A][i],
                is_scalar,
                scalar_args,
                offsets,
                nrows,
            ),
        )
        return nothing
    end
end

"""Loop-invariant evaluation state: the slot pool, the descriptor/scalar
stacks, and the free-list base. Immutable, so passing it compiles to the
same code as passing the fields separately."""
struct PlanState{T}
    pool::Matrix{T}
    descriptors::Vector{Int64}
    scalar_vals::Vector{T}
    free_base::Int
    nrows::Int
end

"""The evaluator's register-like counters: descriptor stack top, free-list
length, and high-water slot. Threaded through `_push_leaf!`/`_exec_op!`."""
struct PlanRegisters
    stack_top::Int
    num_free::Int
    next_slot::Int32
end

@inline function _push_leaf!(
    state::PlanState{T}, regs::PlanRegisters, entry::ArenaEntry{T}, feature_mask::UInt64
) where {T}
    stack_top = regs.stack_top + 1
    @inbounds if _leaf_kind(entry) == FoldedConstant
        state.descriptors[stack_top] = _pack_descriptor(FoldedConstant)
        state.scalar_vals[stack_top] = entry.val
    else
        feature_slot = _feature_slot(feature_mask, entry.feature)
        state.descriptors[stack_top] = _pack_descriptor(PinnedSlot, feature_slot)
    end
    return PlanRegisters(stack_top, regs.num_free, regs.next_slot)
end

"""Execute one operator node of runtime degree `degree`: dispatch the
runtime degree to a compile-time arity, then fold (all-scalar operands) or
run the array kernel. Returns `(regs, ok)`."""
@generated function _exec_op!(
    state::PlanState{T},
    regs::PlanRegisters,
    op_idx::UInt8,
    degree::UInt8,
    is_root::Bool,
    early_exit::Bool,
    operators::O,
    ::Val{D},
) where {T,O<:OperatorEnum,D}
    quote
        return Base.Cartesian.@nif(
            $D,
            A -> A == degree,  # COV_EXCL_LINE
            A -> _exec_op_arity!(
                Val(A), state, regs, op_idx, is_root, early_exit, operators
            ),
        )
    end
end

"""Pop the top `A` operand descriptors and execute one arity-`A` operator
node: constant-fold if every operand is a scalar, otherwise run the kernel."""
@generated function _exec_op_arity!(
    ::Val{A},
    state::PlanState{T},
    regs::PlanRegisters,
    op_idx::UInt8,
    is_root::Bool,
    early_exit::Bool,
    operators::O,
) where {A,T,O<:OperatorEnum}
    quote
        (; descriptors, scalar_vals) = state
        (; stack_top, num_free, next_slot) = regs
        @inbounds begin
            kinds = Base.Cartesian.@ntuple(
                $A, k -> _descriptor_kind(descriptors[stack_top - $A + k])
            )
            idxs = Base.Cartesian.@ntuple(
                $A, k -> _descriptor_slot(descriptors[stack_top - $A + k])
            )
            scalar_args = Base.Cartesian.@ntuple(
                $A, k -> if kinds[k] == FoldedConstant
                    scalar_vals[stack_top - $A + k]
                else
                    zero(T)
                end
            )
        end
        regs = PlanRegisters(stack_top - ($A - 1), num_free, next_slot)
        all_args_scalar = Base.Cartesian.@nall($A, k -> kinds[k] == FoldedConstant)
        if _op_result_kind(all_args_scalar) == FoldedConstant
            return _fold_constant_args!(state, regs, op_idx, scalar_args, operators)
        else
            return _run_op_kernel!(
                state,
                regs,
                op_idx,
                kinds,
                idxs,
                scalar_args,
                is_root,
                early_exit,
                operators,
            )
        end
    end
end

"""Constant-fold an operator whose operands are all in the scalar lane,
writing the result descriptor at the (already popped) stack top. Mirrors
`dispatch_constant_tree`: operand values and the fold result are validated
unconditionally (not gated on `early_exit`); folded args are valid by
induction, so the operand check only screens constant leaves."""
@inline function _fold_constant_args!(
    state::PlanState{T},
    regs::PlanRegisters,
    op_idx::UInt8,
    scalar_args::NTuple{A,T},
    operators::OperatorEnum,
) where {A,T}
    all(is_valid, scalar_args) || return (regs, false)
    value = _scalar_degn(Val(A), op_idx, scalar_args, operators)
    is_valid(value) || return (regs, false)
    @inbounds state.descriptors[regs.stack_top] = _pack_descriptor(FoldedConstant)
    @inbounds state.scalar_vals[regs.stack_top] = value
    return (regs, true)
end

"""Run an operator over pool slots: recycle the freed argument slots,
allocate the destination (slot 1 at the root), and dispatch the kernel.

Validation under `early_exit` mirrors the generic evaluator at lower cost:
scalar operands are checked at consumption (O(1), like
`@return_on_nonfinite_val`), while slot operands are covered by checking each
kernel *output* at production — every non-root intermediate is consumed
exactly once, so this rejects the same trees as per-consumption checks
(`@return_on_nonfinite_array`), reading the slot while it is still hot.
Features are validated once at materialization. The *root* output is never
checked, as in the generic evaluator."""
@generated function _run_op_kernel!(
    state::PlanState{T},
    regs::PlanRegisters,
    op_idx::UInt8,
    kinds::NTuple{A,OperandKind},
    idxs::NTuple{A,Int32},
    scalar_args::NTuple{A,T},
    is_root::Bool,
    early_exit::Bool,
    operators::O,
) where {A,T,O<:OperatorEnum}
    quote
        (; pool, descriptors, free_base, nrows) = state
        (; stack_top, num_free, next_slot) = regs
        # free recyclable argument slots first; the destination may then reuse
        # one (kernels are alias-safe: reads and writes of the same slot are
        # at the same element index)
        @inbounds Base.Cartesian.@nexprs(
            $A, k -> if _is_recyclable(kinds[k])
                num_free += 1
                descriptors[free_base + num_free] = Int64(idxs[k])
            end
        )
        if is_root
            slot = Int32(1)
        elseif num_free > 0
            slot = Int32(@inbounds(descriptors[free_base + num_free]))
            num_free -= 1
        else
            next_slot += Int32(1)
            slot = next_slot
        end
        @inbounds descriptors[stack_top] = _pack_descriptor(ScratchSlot, slot)
        dest_offset = _slot_offset(slot, nrows)
        is_scalar = Base.Cartesian.@ntuple($A, k -> kinds[k] == FoldedConstant)
        offsets = Base.Cartesian.@ntuple(
            $A, k -> kinds[k] == FoldedConstant ? 0 : _slot_offset(idxs[k], nrows)
        )
        regs = PlanRegisters(stack_top, num_free, next_slot)
        scalars_valid =
            !early_exit ||
            Base.Cartesian.@nall($A, k -> !is_scalar[k] || is_valid(scalar_args[k]))
        scalars_valid || return (regs, false)
        _dispatch_degn!(
            Val($A),
            pool,
            dest_offset,
            op_idx,
            is_scalar,
            scalar_args,
            offsets,
            nrows,
            operators,
        )
        if early_exit && !is_root && !_valid_slot(pool, dest_offset, nrows)
            return (regs, false)
        end
        return (regs, true)
    end
end

"""Copy each used feature column of `cX` into its permanent pool slot,
returning whether all copied columns are valid. Slot layout in the pool:
1 = output; 2 .. 1+num_features = materialized features (ascending feature
order); intermediates after that.

The validity check is fused into the copy (one extra add per element of a
memory-bound loop) and replaces per-consumption checks: every materialized
feature is consumed by at least one operator, so "invalid here" and "invalid
at consumption" reject the same trees. `check_validity` is false when
`early_exit` is off, and for a single-leaf tree, where no operator ever
consumes the feature (`deg0_eval` never validates a bare leaf)."""
function _materialize_features!(
    pool::Matrix{T}, cX::Matrix{T}, feature_mask::UInt64, nrows::Int, check_validity::Bool
) where {T}
    slot = 1
    remaining = feature_mask
    while !iszero(remaining)
        feature = trailing_zeros(remaining) + 1
        slot += 1
        offset = (slot - 1) * nrows
        @inbounds @simd for j in 1:nrows
            pool[offset + j] = cX[feature, j]
        end
        # Separate validity pass over the just-written (cache-hot) slot keeps
        # the copy loop a pure memcpy pattern.
        check_validity && !_valid_slot(pool, offset, nrows) && return false
        remaining &= remaining - 1
    end
    return true
end

"""Normalize the finished evaluation so the result lands in pool row 1: copy
a scalar/passthrough root into the output chunk if needed, then convert the
contiguous output chunk into the strided row the generic buffered evaluator
returns (`@view(buffer.array[1, :])`), keeping `eval_tree_array` type stable."""
function _write_root_to_output!(
    pool::Matrix{T}, descriptors::Vector{Int64}, scalar_vals::Vector{T}, nrows::Int
) where {T}
    # Root never went through a kernel (bare leaf or fully folded scalar), or
    # an op-root wrote into a non-output slot via in-place deg1 reuse. A bare
    # leaf root is never validity-checked (`deg0_eval` semantics); a folded
    # scalar root is already valid by induction.
    root_kind = _descriptor_kind(descriptors[1])
    root_slot = _descriptor_slot(descriptors[1])
    if root_kind == FoldedConstant
        value = scalar_vals[1]
        @inbounds @simd for j in 1:nrows
            pool[j] = value
        end
    elseif !isone(root_slot)
        root_offset = _slot_offset(root_slot, nrows)
        @inbounds @simd for j in 1:nrows
            pool[j] = pool[root_offset + j]
        end
    end
    # The chunk and row 1 overlap in memory; iterating downward is safe: when
    # reading chunk index j, every already-written row position (j''-1)*B+1
    # with j'' > j exceeds j for B = size(pool, 1) >= 2, and for B == 1 the
    # chunk and row coincide elementwise.
    if size(pool, 1) > 1
        @inbounds for j in nrows:-1:1
            pool[1, j] = pool[j]
        end
    end
    return nothing
end

function _arena_eval(
    arena::Arena{T,D},
    cX::Matrix{T},
    operators::OperatorEnum,
    ::Val{early_exit},
    num_slots::Int,
    max_stack::Int,
    feature_mask::UInt64,
    pool::Matrix{T},
) where {T,D,early_exit}
    nodes = arena.nodes
    num_nodes = length(nodes)
    nrows = size(cX, 2)
    num_features = count_ones(feature_mask)
    output = @view(pool[1, :])

    check_features = early_exit && num_nodes > 1
    if !_materialize_features!(pool, cX, feature_mask, nrows, check_features)
        return ResultOk(output, false)
    end

    # Per-call descriptor state (tiny; the pool itself is caller-owned):
    descriptors = Vector{Int64}(undef, max_stack + num_slots)
    scalar_vals = Vector{T}(undef, max_stack)
    state = PlanState(pool, descriptors, scalar_vals, max_stack, nrows)
    regs = PlanRegisters(0, 0, Int32(1 + num_features))

    @inbounds for i in 1:num_nodes
        entry = nodes[i]
        if iszero(entry.degree)
            regs = _push_leaf!(state, regs, entry, feature_mask)
        else
            is_root = i == num_nodes
            regs, ok = _exec_op!(
                state, regs, entry.op, entry.degree, is_root, early_exit, operators, Val(D)
            )
            ok || return ResultOk(output, false)
        end
    end

    _write_root_to_output!(pool, descriptors, scalar_vals, nrows)
    return ResultOk(output, true)
end

end
