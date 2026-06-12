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
struct ArenaEntry{T,D}
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
struct Arena{T,D} <: AbstractVector{ArenaEntry{T,D}}
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
    arena = getfield(tree, :arena)
    return arena.compact[] && getfield(tree, :idx) == length(arena.nodes)
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
    push!(arena, ArenaEntry{T,D}(val, children, feature, degree, op, constant))
    return Int32(length(arena))
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
    return Nullable{ArenaNode{T,D}}(child_idx == 0, child)
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
    child_idx == 0 && throw(UndefRefError())
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
    return _push_node!(arena, UInt8(degree), false, zero(T), UInt16(0), tree.op, idxs)
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
        ok_plan, n_slots, max_stack, fmask = _plan_scratch(getfield(tree, :arena))
        # +1 for the output slot; capacity is the buffer's row count
        if ok_plan && n_slots + 1 <= size(buffer.array, 1)
            return _arena_eval(
                getfield(tree, :arena),
                cX,
                operators,
                eval_options.early_exit,
                n_slots,
                max_stack,
                fmask,
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
    fmask = UInt64(0)
    scalar_mask = UInt64(0)
    perm_mask = UInt64(0)
    stack_top = 0
    max_stack = 0
    live = 0
    num_free = 0
    max_int_slots = 0
    @inbounds for i in eachindex(nodes)
        entry = nodes[i]
        degree = entry.degree
        if iszero(degree)
            stack_top >= 64 && return (false, 0, 0, UInt64(0))
            stack_top += 1
            max_stack = max(max_stack, stack_top)
            scalar_mask = (scalar_mask << 1) | (entry.constant ? 1 : 0)
            perm_mask <<= 1
            if !entry.constant
                feature = entry.feature
                (1 <= feature <= 64) || return (false, 0, 0, UInt64(0))
                fmask |= UInt64(1) << (feature - 1)
                perm_mask |= 1
            end
        else
            window = (UInt64(1) << degree) - 1
            all_scalar = (scalar_mask & window) == window
            n_free_args = count_ones(~perm_mask & ~scalar_mask & window)
            scalar_mask >>= (degree - 1)
            perm_mask >>= (degree - 1)
            stack_top -= degree - 1
            if all_scalar
                scalar_mask |= 1
                perm_mask &= ~UInt64(1)
            else
                num_free += n_free_args
                if num_free > 0
                    num_free -= 1
                else
                    live += 1
                    max_int_slots = max(max_int_slots, live)
                end
                scalar_mask &= ~UInt64(1)
                perm_mask &= ~UInt64(1)
            end
        end
    end
    n_slots = count_ones(fmask) + max_int_slots
    # A well-formed postfix tree collapses the stack to exactly the root;
    # anything else (e.g. orphaned roots) must fail closed.
    return (stack_top == 1, n_slots, max_stack, fmask)
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
struct _PlanState{T}
    pool::Matrix{T}
    descriptors::Vector{Int64}
    scalar_vals::Vector{T}
    free_base::Int
    nrows::Int
end

"""The evaluator's register-like counters: descriptor stack top, free-list
length, and high-water slot. Threaded through `_push_leaf!`/`_exec_op!`."""
struct _PlanRegs
    stack_top::Int
    num_free::Int
    next_slot::Int32
end

@inline function _push_leaf!(
    state::_PlanState{T}, regs::_PlanRegs, entry::ArenaEntry{T}, fmask::UInt64
) where {T}
    stack_top = regs.stack_top + 1
    @inbounds if entry.constant
        state.descriptors[stack_top] = Int64(_K_SCALAR)
        state.scalar_vals[stack_top] = entry.val
    else
        fslot = count_ones(fmask & ((UInt64(1) << (entry.feature - 1)) - 1)) + 2
        state.descriptors[stack_top] = Int64(_K_PSLOT) | (Int64(fslot) << 2)
    end
    return _PlanRegs(stack_top, regs.num_free, regs.next_slot)
end

"""Execute one operator node of runtime degree `d` (dispatched to a
compile-time arity with `Base.Cartesian.@nif`): gather the top `degree` operand
descriptors, fold if all are scalars, otherwise free recyclable argument
slots, allocate the destination (slot 1 when at the root), and run the
kernel. Returns `(regs, ok)`."""
@generated function _exec_op!(
    state::_PlanState{T},
    regs::_PlanRegs,
    op_idx::UInt8,
    degree::UInt8,
    is_root::Bool,
    early_exit::Bool,
    operators::O,
    ::Val{D},
) where {T,O<:OperatorEnum,D}
    quote
        (; pool, descriptors, scalar_vals, free_base, nrows) = state
        (; stack_top, num_free, next_slot) = regs
        return Base.Cartesian.@nif(
            $D,
            A -> A == degree,  # COV_EXCL_LINE
            A -> @inbounds begin
                kinds = Base.Cartesian.@ntuple(
                    A, k -> UInt8(descriptors[stack_top - A + k] & 3)
                )
                idxs = Base.Cartesian.@ntuple(
                    A, k -> Int32(descriptors[stack_top - A + k] >> 2)
                )
                scalar_args = Base.Cartesian.@ntuple(
                    A,
                    k -> if kinds[k] == _K_SCALAR
                        scalar_vals[stack_top - A + k]
                    else
                        zero(T)
                    end
                )
                stack_top -= A - 1
                if Base.Cartesian.@nall(A, k -> kinds[k] == _K_SCALAR)
                    # Mirrors `dispatch_constant_tree`: leaf values and fold
                    # results are validated unconditionally (not gated on
                    # `early_exit`). Folded args are valid by induction, so
                    # the arg check only screens constant leaves.
                    if Base.Cartesian.@nall(A, k -> is_valid(scalar_args[k]))
                        value = _scalar_degn(Val(A), op_idx, scalar_args, operators)
                        ok = is_valid(value)
                        if ok
                            descriptors[stack_top] = Int64(_K_SCALAR)
                            scalar_vals[stack_top] = value
                        end
                        (_PlanRegs(stack_top, num_free, next_slot), ok)
                    else
                        (_PlanRegs(stack_top, num_free, next_slot), false)
                    end
                else
                    # free recyclable argument slots first; the destination
                    # may then reuse one (kernels are alias-safe: reads and
                    # writes of the same slot are at the same element index)
                    Base.Cartesian.@nexprs(
                        A, k -> if kinds[k] == _K_SLOT
                            num_free += 1
                            descriptors[free_base + num_free] = Int64(idxs[k])
                        end
                    )
                    if is_root
                        slot = Int32(1)
                    elseif num_free > 0
                        slot = Int32(descriptors[free_base + num_free])
                        num_free -= 1
                    else
                        next_slot += Int32(1)
                        slot = next_slot
                    end
                    descriptors[stack_top] = Int64(_K_SLOT) | (Int64(slot) << 2)
                    dest_offset = _slotoff(slot, nrows)
                    is_scalar = Base.Cartesian.@ntuple(A, k -> kinds[k] == _K_SCALAR)
                    offsets = Base.Cartesian.@ntuple(
                        A, k -> kinds[k] == _K_SCALAR ? 0 : _slotoff(idxs[k], nrows)
                    )
                    # Mirrors `@return_on_nonfinite_array`/`_val`: operands
                    # (slots and constant scalars) are validated at consumption
                    # when `early_exit` is set; outputs are never checked here,
                    # so a non-finite *root* result still returns ok=true, as
                    # in the generic evaluator.
                    if early_exit &&
                        !Base.Cartesian.@nall(
                        A, k -> if is_scalar[k]
                            is_valid(scalar_args[k])
                        else
                            _valid_slot(pool, offsets[k], nrows)
                        end,
                    )
                        (_PlanRegs(stack_top, num_free, next_slot), false)
                    else
                        _dispatch_degn!(
                            Val(A),
                            pool,
                            dest_offset,
                            op_idx,
                            is_scalar,
                            scalar_args,
                            offsets,
                            nrows,
                            operators,
                        )
                        (_PlanRegs(stack_top, num_free, next_slot), true)
                    end
                end
            end
        )
    end
end

function _arena_eval(
    arena::Arena{T,D},
    cX::Matrix{T},
    operators::OperatorEnum,
    ::Val{early_exit},
    n_slots::Int,
    max_stack::Int,
    fmask::UInt64,
    pool::Matrix{T},
) where {T,D,early_exit}
    nodes = getfield(arena, :nodes)
    num_nodes = length(nodes)
    nrows = size(cX, 2)

    # Slot layout in the pool: 1 = output; 2 .. 1+n_perm = materialized
    # features; intermediates after that. Slots are addressed by offset; the
    # output view is constructed once, at return.
    n_perm = count_ones(fmask)
    let slot = 1
        remaining = fmask
        while remaining != 0
            feature = trailing_zeros(remaining) + 1
            slot += 1
            offset = (slot - 1) * nrows
            @inbounds @simd for j in 1:nrows
                pool[offset + j] = cX[feature, j]
            end
            remaining &= remaining - 1
        end
    end

    # Per-call descriptor state (tiny; the pool itself is caller-owned):
    descriptors = Vector{Int64}(undef, max_stack + n_slots)
    scalar_vals = Vector{T}(undef, max_stack)
    state = _PlanState(pool, descriptors, scalar_vals, max_stack, nrows)
    regs = _PlanRegs(0, 0, Int32(1 + n_perm))
    out() = @inbounds @view(pool[1, :])

    @inbounds for i in 1:num_nodes
        entry = nodes[i]
        if iszero(entry.degree)
            regs = _push_leaf!(state, regs, entry, fmask)
        else
            is_root = i == num_nodes
            regs, ok = _exec_op!(
                state, regs, entry.op, entry.degree, is_root, early_exit, operators, Val(D)
            )
            ok || return ResultOk(out(), false)
        end
    end

    # Root never went through a kernel (bare leaf or fully folded scalar), or
    # an op-root wrote into a non-output slot via in-place deg1 reuse. A bare
    # leaf root is never validity-checked (`deg0_eval` semantics); a folded
    # scalar root is already valid by induction.
    kroot = UInt8(descriptors[1] & 3)
    if kroot == _K_SCALAR
        value = scalar_vals[1]
        @inbounds @simd for j in 1:nrows
            pool[j] = value
        end
    elseif Int32(descriptors[1] >> 2) != Int32(1)
        soff = _slotoff(Int32(descriptors[1] >> 2), nrows)
        @inbounds @simd for j in 1:nrows
            pool[j] = pool[soff + j]
        end
    end
    # Move the result from chunk 1 (linear 1:nrows) into row 1, so the
    # returned view has the same type as the generic buffered evaluator's
    # `@view(buffer.array[i, :])` (keeping `eval_tree_array` type stable).
    # Chunk and row overlap in memory; iterating downward is safe: when
    # reading chunk index j, every already-written row position (j''-1)*B+1
    # with j'' > j exceeds j for buffer_rows >= 2, and for buffer_rows == 1 chunk and row
    # coincide elementwise.
    buffer_rows = size(pool, 1)
    if buffer_rows > 1
        @inbounds for j in nrows:-1:1
            pool[1, j] = pool[j]
        end
    end
    return ResultOk(out(), true)
end

end
