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

Base.size(arena::Arena) = size(getfield(arena, :nodes))
Base.IndexStyle(::Type{<:Arena}) = IndexLinear()
Base.@propagate_inbounds Base.getindex(arena::Arena, i::Integer) =
    getfield(arena, :nodes)[i]
Base.@propagate_inbounds function Base.setindex!(
    arena::Arena{T,D}, entry::ArenaEntry{T,D}, i::Integer
) where {T,D}
    nodes = getfield(arena, :nodes)
    old = nodes[i]
    if entry.degree != old.degree || entry.children != old.children
        arena.compact[] = false
    end
    nodes[i] = entry
    return arena
end
function Base.push!(arena::Arena{T,D}, entry::ArenaEntry{T,D}) where {T,D}
    nodes = getfield(arena, :nodes)
    # A single leaf in a fresh arena is a valid tree; any further append breaks
    # the one-postfix-tree invariant until a builder re-establishes it.
    isempty(nodes) || (arena.compact[] = false)
    push!(nodes, entry)
    return arena
end
function Base.sizehint!(arena::Arena, capacity::Integer)
    sizehint!(getfield(arena, :nodes), capacity)
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

"""Whether `tree` is the root of a compact arena, so that the arena contents
*are* the tree and whole-tree operations can act on the flat array directly."""
@inline function is_compact_root(tree::ArenaNode)
    arena = getfield(tree, :arena)
    return arena.compact[] && getfield(tree, :idx) == length(arena.nodes)
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
        return getfield(node, :arena)
    elseif property_name === :idx
        return getfield(node, :idx)
    elseif property_name === :degree
        return @inbounds getfield(node, :arena).nodes[getfield(node, :idx)].degree
    elseif property_name === :constant
        return @inbounds getfield(node, :arena).nodes[getfield(node, :idx)].constant
    elseif property_name === :val
        return @inbounds getfield(node, :arena).nodes[getfield(node, :idx)].val::T
    elseif property_name === :feature
        return @inbounds getfield(node, :arena).nodes[getfield(node, :idx)].feature
    elseif property_name === :op
        return @inbounds getfield(node, :arena).nodes[getfield(node, :idx)].op
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
        children = @inbounds getfield(node, :arena).nodes[getfield(node, :idx)].children
        return Base.Cartesian.@ntuple($D, j -> _nullable_child(node, children[j]))
    end
end

@inline function get_child(node::ArenaNode{T,D}, i::Integer) where {T,D}
    # Avoid routing through getproperty here: the :l/:r property branches call
    # get_child, and the resulting inference cycle widens property access.
    arena = getfield(node, :arena)
    entry = @inbounds getfield(arena, :nodes)[getfield(node, :idx)]
    child_idx = entry.children[i]  # bounds-checked: i > D must throw, not crash
    iszero(child_idx) && throw(UndefRefError())
    return ArenaNode{T,D}(arena, child_idx)
end

@inline function set_child!(
    node::ArenaNode{T,D}, child::AbstractNode{D}, i::Int
) where {T,D}
    child isa AbstractExpressionNode{T,D} || throw(
        ArgumentError(
            "ArenaNode children must be AbstractExpressionNode{$T,$D} (got $(typeof(child)))",
        ),
    )

    # We cannot directly link across arenas, so we copy the subtree into `node`'s arena.
    idx = if child isa ArenaNode{T,D} && child.arena === node.arena
        child.idx
    else
        _copy_to_arena!(node.arena, child)
    end

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

        child isa AbstractExpressionNode{T,D} || throw(
            ArgumentError(
                "ArenaNode children must be AbstractExpressionNode{$T,$D} (got $(typeof(child)))",
            ),
        )

        idx = if child isa ArenaNode{T,D} && child.arena === node.arena
            child.idx
        else
            _copy_to_arena!(node.arena, child)
        end
        idxs = Base.setindex(idxs, idx, i)
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
    if dest === getfield(src, :arena)
        # Container reuse: the tree already lives in `dest`. A compact root is
        # a no-op; otherwise compact through a temporary copy.
        is_compact_root(src) && return src
        return copy_into!(dest, copy_node(src); ref)
    end
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
    arena.compact[] = true
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
    return _arena_any(f, getfield(tree, :arena), getfield(tree, :idx))
end
function _arena_any(f::F, arena::Arena{T,D}, idx::Int32) where {F<:Function,T,D}
    iszero(idx) && throw(UndefRefError())  # unset child slot, like Node
    entry = @inbounds getfield(arena, :nodes)[idx]
    @inline(f(ArenaNode{T,D}(arena, idx))) && return true
    @inbounds for j in 1:entry.degree
        _arena_any(f, arena, entry.children[j]) && return true
    end
    return false
end

function is_constant(tree::ArenaNode)
    return !_arena_any(
        node -> iszero(node.degree) && !node.constant,
        getfield(tree, :arena),
        getfield(tree, :idx),
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
    return _arena_mapreduce(
        f_leaf, f_branch, op, getfield(tree, :arena), getfield(tree, :idx)
    )
end

@generated function _arena_mapreduce(
    f_leaf::F1, f_branch::F2, op::G, arena::Arena{T,D}, idx::Int32
) where {F1<:Function,F2<:Function,G<:Function,T,D}
    quote
        iszero(idx) && throw(UndefRefError())  # unset child slot, like Node
        entry = @inbounds getfield(arena, :nodes)[idx]
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
    # Deliberately bounds-checked: refs are caller-supplied and may be stale.
    for j in eachindex(refs, constants)
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
        ok_plan, num_slots, max_stack, feature_mask = _plan_scratch(getfield(tree, :arena))
        # +1 for the output slot; capacity is the buffer's row count
        if ok_plan && num_slots + 1 <= size(buffer.array, 1)
            return _arena_eval(
                getfield(tree, :arena),
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
    return count_ones(feature_mask & ((UInt64(1) << (feature - 1)) - 1)) + 2
end

# Descriptor kinds for evaluation stack slots:
const _K_SCALAR = 0x00  # folded constant; value lives in the scalar lane
const _K_PSLOT = 0x01   # permanent slot (output or a materialized feature)
const _K_SLOT = 0x02    # recyclable slot (an intermediate)

"""Alloc-free pre-pass: find which features are used and simulate the
descriptor stack to count the recyclable intermediate slots (register
allocation with a free list). Kinds are tracked in `UInt64` bitmask stacks,
so trees deeper than 64 or features beyond 64 report failure and take the
generic path."""
function _plan_scratch(arena::Arena{T,D}) where {T,D}
    nodes = getfield(arena, :nodes)
    feature_mask = UInt64(0)
    scalar_stack = UInt64(0)
    permanent_stack = UInt64(0)
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
            scalar_stack = (scalar_stack << 1) | (entry.constant ? 1 : 0)
            permanent_stack <<= 1
            if !entry.constant
                feature = entry.feature
                (1 <= feature <= 64) || return (false, 0, 0, UInt64(0))
                feature_mask |= UInt64(1) << (feature - 1)
                permanent_stack |= 1
            end
        else
            arity_mask = (UInt64(1) << degree) - 1
            all_args_scalar = (scalar_stack & arity_mask) == arity_mask
            num_recyclable_args = count_ones(~permanent_stack & ~scalar_stack & arity_mask)
            scalar_stack >>= (degree - 1)
            permanent_stack >>= (degree - 1)
            stack_top -= degree - 1
            if all_args_scalar
                scalar_stack |= 1
                permanent_stack &= ~UInt64(1)
            else
                num_free += num_recyclable_args
                if num_free > 0
                    num_free -= 1
                else
                    num_live += 1
                    max_live_intermediates = max(max_live_intermediates, num_live)
                end
                scalar_stack &= ~UInt64(1)
                permanent_stack &= ~UInt64(1)
            end
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
@inline _slotoff(slot::Int32, nrows::Int) = (slot - 1) * nrows

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
    @inbounds if entry.constant
        state.descriptors[stack_top] = Int64(_K_SCALAR)
        state.scalar_vals[stack_top] = entry.val
    else
        feature_slot = _feature_slot(feature_mask, entry.feature)
        state.descriptors[stack_top] = Int64(_K_PSLOT) | (Int64(feature_slot) << 2)
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
                $A, k -> UInt8(descriptors[stack_top - $A + k] & 3)
            )
            idxs = Base.Cartesian.@ntuple(
                $A, k -> Int32(descriptors[stack_top - $A + k] >> 2)
            )
            scalar_args = Base.Cartesian.@ntuple(
                $A, k -> if kinds[k] == _K_SCALAR
                    scalar_vals[stack_top - $A + k]
                else
                    zero(T)
                end
            )
        end
        regs = PlanRegisters(stack_top - ($A - 1), num_free, next_slot)
        if Base.Cartesian.@nall($A, k -> kinds[k] == _K_SCALAR)
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
    @inbounds state.descriptors[regs.stack_top] = Int64(_K_SCALAR)
    @inbounds state.scalar_vals[regs.stack_top] = value
    return (regs, true)
end

"""Run an operator over pool slots: recycle the freed argument slots,
allocate the destination (slot 1 at the root), and dispatch the kernel.
Mirrors `@return_on_nonfinite_array`/`_val`: operands (slots and constant
scalars) are validated at consumption when `early_exit` is set; outputs are
never checked here, so a non-finite *root* result still returns ok=true, as
in the generic evaluator."""
@generated function _run_op_kernel!(
    state::PlanState{T},
    regs::PlanRegisters,
    op_idx::UInt8,
    kinds::NTuple{A,UInt8},
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
            $A, k -> if kinds[k] == _K_SLOT
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
        @inbounds descriptors[stack_top] = Int64(_K_SLOT) | (Int64(slot) << 2)
        dest_offset = _slotoff(slot, nrows)
        is_scalar = Base.Cartesian.@ntuple($A, k -> kinds[k] == _K_SCALAR)
        offsets = Base.Cartesian.@ntuple(
            $A, k -> kinds[k] == _K_SCALAR ? 0 : _slotoff(idxs[k], nrows)
        )
        regs = PlanRegisters(stack_top, num_free, next_slot)
        args_valid =
            !early_exit || Base.Cartesian.@nall($A, k -> if is_scalar[k]
                is_valid(scalar_args[k])
            else
                _valid_slot(pool, offsets[k], nrows)
            end)
        args_valid || return (regs, false)
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
        return (regs, true)
    end
end

"""Copy each used feature column of `cX` into its permanent pool slot.
Slot layout in the pool: 1 = output; 2 .. 1+num_features = materialized
features (ascending feature order); intermediates after that."""
function _materialize_features!(
    pool::Matrix{T}, cX::Matrix{T}, feature_mask::UInt64, nrows::Int
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
        remaining &= remaining - 1
    end
    return nothing
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
    root_kind = UInt8(descriptors[1] & 3)
    root_slot = Int32(descriptors[1] >> 2)
    if root_kind == _K_SCALAR
        value = scalar_vals[1]
        @inbounds @simd for j in 1:nrows
            pool[j] = value
        end
    elseif !isone(root_slot)
        root_offset = _slotoff(root_slot, nrows)
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
    nodes = getfield(arena, :nodes)
    num_nodes = length(nodes)
    nrows = size(cX, 2)
    num_features = count_ones(feature_mask)

    _materialize_features!(pool, cX, feature_mask, nrows)

    # Per-call descriptor state (tiny; the pool itself is caller-owned):
    descriptors = Vector{Int64}(undef, max_stack + num_slots)
    scalar_vals = Vector{T}(undef, max_stack)
    state = PlanState(pool, descriptors, scalar_vals, max_stack, nrows)
    regs = PlanRegisters(0, 0, Int32(1 + num_features))
    output = @view(pool[1, :])

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
