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
import ..EvaluateModule: _eval_tree_array, EvalOptions, ArrayBuffer

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

Base.size(a::Arena) = size(getfield(a, :nodes))
Base.IndexStyle(::Type{<:Arena}) = IndexLinear()
Base.@propagate_inbounds Base.getindex(a::Arena, i::Integer) = getfield(a, :nodes)[i]
Base.@propagate_inbounds function Base.setindex!(
    a::Arena{T,D}, e::ArenaEntry{T,D}, i::Integer
) where {T,D}
    nodes = getfield(a, :nodes)
    old = nodes[i]
    if e.degree != old.degree || e.children != old.children
        a.compact[] = false
    end
    nodes[i] = e
    return a
end
function Base.push!(a::Arena{T,D}, e::ArenaEntry{T,D}) where {T,D}
    push!(getfield(a, :nodes), e)
    return a
end
Base.sizehint!(a::Arena, n::Integer) = (sizehint!(getfield(a, :nodes), n); a)

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
        return get_child(n, UInt8(1))
    elseif k === :r
        return get_child(n, UInt8(2))
    else
        return getfield(n, k)
    end
end

@inline function Base.setproperty!(n::ArenaNode{T,D}, k::Symbol, v) where {T,D}
    a = n.arena
    i = n.idx
    e = @inbounds a[i]
    if k === :degree
        @inbounds a[i] = _replace(e; degree=UInt8(v))
        return v
    elseif k === :constant
        @inbounds a[i] = _replace(e; constant=Bool(v))
        return v
    elseif k === :val
        @inbounds a[i] = _replace(e; val=convert(T, v))
        return v
    elseif k === :feature
        @inbounds a[i] = _replace(e; feature=UInt16(v))
        return v
    elseif k === :op
        @inbounds a[i] = _replace(e; op=UInt8(v))
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
        $(Expr(:meta, :inline))
        children = @inbounds getfield(n, :arena).nodes[getfield(n, :idx)].children
        return Base.Cartesian.@ntuple($D, j -> _nullable_child(n, children[j]))
    end
end

@inline function get_child(n::ArenaNode{T,D}, i::Integer) where {T,D}
    # Avoid routing through getproperty here: the :l/:r property branches call
    # get_child, and the resulting inference cycle widens property access.
    a = getfield(n, :arena)
    c = @inbounds getfield(a, :nodes)[Int(getfield(n, :idx))].children[i]
    c == 0 && throw(UndefRefError())
    return ArenaNode{T,D}(a, c)
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
    e = @inbounds a[n.idx]
    if @inbounds(e.children[i]) != idx
        @inbounds a[n.idx] = _replace(e; children=Base.setindex(e.children, idx, i))
    end
    return ArenaNode{T,D}(a, idx)
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
    e = @inbounds a[n.idx]
    @inbounds a[n.idx] = _replace(e; children=idxs)
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

function Base.any(f::F, tree::ArenaNode{T,D}) where {F<:Function,T,D}
    return _arena_any(f, getfield(tree, :arena), getfield(tree, :idx))
end
function _arena_any(f::F, a::Arena{T,D}, idx::Int32) where {F<:Function,T,D}
    e = @inbounds getfield(a, :nodes)[Int(idx)]
    @inline(f(ArenaNode{T,D}(a, idx))) && return true
    @inbounds for j in 1:Int(e.degree)
        _arena_any(f, a, e.children[j]) && return true
    end
    return false
end

function is_constant(tree::ArenaNode)
    return _is_constant(getfield(getfield(tree, :arena), :nodes), getfield(tree, :idx))
end
function _is_constant(nodes::Vector{ArenaEntry{T,D}}, idx::Int32) where {T,D}
    e = @inbounds nodes[Int(idx)]
    e.degree == 0x00 && return e.constant
    @inbounds for j in 1:Int(e.degree)
        _is_constant(nodes, e.children[j]) || return false
    end
    return true
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
    f_leaf::F1, f_branch::F2, op::G, a::Arena{T,D}, idx::Int32
) where {F1<:Function,F2<:Function,G<:Function,T,D}
    quote
        e = @inbounds getfield(a, :nodes)[Int(idx)]
        d = e.degree
        if d == 0x00
            return f_leaf(ArenaNode{T,D}(a, idx))
        end
        branch = f_branch(ArenaNode{T,D}(a, idx))
        children = e.children
        return Base.Cartesian.@nif(
            $D,
            i -> i == Int(d),  # COV_EXCL_LINE
            i -> Base.Cartesian.@ncall(
                i,
                op,
                branch,
                j -> _arena_mapreduce(f_leaf, f_branch, op, a, children[j])
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
    a = tree.arena
    if is_compact_root(tree)
        nodes = a.nodes
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
    refs = filter_map(is_node_constant, node -> node.idx, tree, Int32)
    vals = T[@inbounds(a[Int(i)].val) for i in refs]
    return vals, refs
end

function set_scalar_constants!(
    tree::ArenaNode{T}, constants, refs::AbstractVector{Int32}
) where {T<:Number}
    a = tree.arena
    @inbounds for j in eachindex(refs, constants)
        i = Int(refs[j])
        a[i] = _replace(a[i]; val=constants[j]::T)
    end
    return nothing
end

################################################################################
# Plan-style buffered evaluation
################################################################################

"""Plan-style postfix evaluation into a caller-provided `ArrayBuffer`,
mirroring the compile-and-execute evaluator of symbolic_regression.rs
(`compile.rs` + `evaluate.rs`): its `EvalContext` is caller-owned, and
`EvalOptions.buffer` is the DynamicExpressions equivalent. This fast path
therefore engages *only* when the caller passes a buffer -- repeated-eval
workloads such as constant optimization -- and never caches state of its own.

Stack slots are descriptors (constant scalar / scratch slot), not arrays:
- the buffer is treated as a flat pool of contiguous slots of `n_rows`
  values each (the generic evaluator hands out strided matrix rows; the
  plan evaluator slices the same memory contiguously so kernels vectorize);
- slot 1 is the output: the root instruction writes straight into it;
- each used feature is materialized once into a permanent slot (the one
  strided read of column-major `cX`), then read contiguously at every use --
  strictly fewer copies than the generic evaluator's copy per leaf;
- intermediate slots are register-allocated with a free list, so the live
  set stays ~tree depth;
- constant subtrees fold in a scalar lane (one flop per node).

Falls back to the generic recursive evaluator when there is no buffer, the
buffer is too small or mismatched, the arena is not compact, `D != 2`, depth
or feature count exceeds 64, or turbo is requested.
"""
function _eval_tree_array(
    tree::ArenaNode{T,D},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    eval_options::EvalOptions,
)::ResultOk where {T<:Number,D}
    buffer = eval_options.buffer
    if D == 2 &&
        buffer isa ArrayBuffer{Matrix{T}} &&
        cX isa Matrix{T} &&
        size(buffer.array, 2) == size(cX, 2) &&
        is_compact_root(tree) &&
        eval_options.turbo isa Val{false}
        ok_plan, n_slots, sp_max, fmask = _plan_scratch(getfield(tree, :arena))
        # +1 for the output slot; capacity is the buffer's row count
        if ok_plan && n_slots + 1 <= size(buffer.array, 1)
            return _arena_eval(
                getfield(tree, :arena),
                cX,
                operators,
                eval_options.early_exit,
                n_slots,
                sp_max,
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
function _plan_scratch(a::Arena{T,D}) where {T,D}
    nodes = getfield(a, :nodes)
    fmask = UInt64(0)
    scalar_mask = UInt64(0)
    perm_mask = UInt64(0)
    sp = 0
    sp_max = 0
    live = 0
    nfree = 0
    max_int_slots = 0
    @inbounds for i in eachindex(nodes)
        e = nodes[i]
        d = e.degree
        if d == 0x00
            sp >= 64 && return (false, 0, 0, UInt64(0))
            sp += 1
            sp_max = max(sp_max, sp)
            scalar_mask = (scalar_mask << 1) | (e.constant ? 1 : 0)
            perm_mask <<= 1
            if !e.constant
                f = Int(e.feature)
                (1 <= f <= 64) || return (false, 0, 0, UInt64(0))
                fmask |= UInt64(1) << (f - 1)
                perm_mask |= 1
            end
        elseif d == 0x01
            if scalar_mask & 1 == 0
                if perm_mask & 1 == 1
                    # permanent operand: result needs a fresh intermediate
                    if nfree > 0
                        nfree -= 1
                    else
                        live += 1
                        max_int_slots = max(max_int_slots, live)
                    end
                    perm_mask &= ~UInt64(1)
                end
                # recyclable operand: reused in place
            end
        else
            both_scalar = (scalar_mask & 3) == 3
            n_free_args = count_ones(~perm_mask & ~scalar_mask & 3)
            scalar_mask >>= 1
            perm_mask >>= 1
            sp -= 1
            if both_scalar
                scalar_mask |= 1
                perm_mask &= ~UInt64(1)
            else
                nfree += n_free_args
                if nfree > 0
                    nfree -= 1
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
    return (true, n_slots, sp_max, fmask)
end

@generated function _scalar_deg1(
    op_idx::UInt8, x::T, operators::O
) where {T,O<:OperatorEnum}
    nops = length(O.parameters[1].parameters[1].parameters)
    return quote
        Base.Cartesian.@nif(
            $nops,
            i -> i == Int(op_idx),  # COV_EXCL_LINE
            i -> operators.unaops[i](x)::T,
        )
    end
end
@generated function _scalar_deg2(
    op_idx::UInt8, x::T, y::T, operators::O
) where {T,O<:OperatorEnum}
    nops = length(O.parameters[1].parameters[2].parameters)
    return quote
        Base.Cartesian.@nif(
            $nops,
            i -> i == Int(op_idx),  # COV_EXCL_LINE
            i -> operators.binops[i](x, y)::T,
        )
    end
end

"""Offset-based kernels over the raw pool: views/broadcasts box `SubArray`
wrappers at the dispatch boundaries (~0.5KB/op), so every kernel works on
`(pool, offset)` pairs with explicit `@simd` loops. The destination is always
a pool slot too (slot 1 is the output), so offsets are uniform."""
@inline function _kern1!(pool::Matrix{T}, doff::Int, soff::Int, op::F, n::Int) where {T,F}
    @inbounds @simd for j in 1:n
        pool[doff + j] = op(pool[soff + j])
    end
    return nothing
end
@inline function _kern2_vv!(
    pool::Matrix{T}, doff::Int, a::Int, b::Int, op::F, n::Int
) where {T,F}
    @inbounds @simd for j in 1:n
        pool[doff + j] = op(pool[a + j], pool[b + j])
    end
    return nothing
end
@inline function _kern2_sv!(
    pool::Matrix{T}, doff::Int, s::T, b::Int, op::F, n::Int
) where {T,F}
    @inbounds @simd for j in 1:n
        pool[doff + j] = op(s, pool[b + j])
    end
    return nothing
end
@inline function _kern2_vs!(
    pool::Matrix{T}, doff::Int, a::Int, s::T, op::F, n::Int
) where {T,F}
    @inbounds @simd for j in 1:n
        pool[doff + j] = op(pool[a + j], s)
    end
    return nothing
end
"""`is_valid_array` over a pool slot without constructing a view."""
@inline function _valid_slot(pool::Matrix{T}, off::Int, n::Int) where {T}
    s = zero(T)
    @inbounds @simd for j in 1:n
        s += pool[off + j]
    end
    return is_valid(s)
end
@inline _slotoff(s::Int32, nrows::Int) = (Int(s) - 1) * nrows
"""Contiguous view of pool slot `s` (linear indexing); used only for the
returned output, never inside kernels."""
@inline function _slotview(pool::Matrix{T}, s::Int32, nrows::Int) where {T}
    off = _slotoff(s, nrows)
    return @inbounds view(pool, (off + 1):(off + nrows))
end

@inline function _deg2_combos!(
    pool::Matrix{T},
    doff::Int,
    op::F,
    k1::UInt8,
    i1::Int32,
    s1::T,
    k2::UInt8,
    i2::Int32,
    s2::T,
    nrows::Int,
) where {F,T}
    if k1 == _K_SCALAR
        _kern2_sv!(pool, doff, s1, _slotoff(i2, nrows), op, nrows)
    elseif k2 == _K_SCALAR
        _kern2_vs!(pool, doff, _slotoff(i1, nrows), s2, op, nrows)
    else
        _kern2_vv!(pool, doff, _slotoff(i1, nrows), _slotoff(i2, nrows), op, nrows)
    end
    return nothing
end

@generated function _dispatch_deg1!(
    pool::Matrix{T}, doff::Int, op_idx::UInt8, src::Int32, nrows::Int, operators::O
) where {T,O<:OperatorEnum}
    nops = length(O.parameters[1].parameters[1].parameters)
    quote
        Base.Cartesian.@nif(
            $nops,
            i -> i == Int(op_idx),  # COV_EXCL_LINE
            i -> _kern1!(pool, doff, _slotoff(src, nrows), operators.unaops[i], nrows),
        )
        return nothing
    end
end
@generated function _dispatch_deg2!(
    pool::Matrix{T},
    doff::Int,
    op_idx::UInt8,
    k1::UInt8,
    i1::Int32,
    s1::T,
    k2::UInt8,
    i2::Int32,
    s2::T,
    nrows::Int,
    operators::O,
) where {T,O<:OperatorEnum}
    nops = length(O.parameters[1].parameters[2].parameters)
    quote
        Base.Cartesian.@nif(
            $nops,
            i -> i == Int(op_idx),  # COV_EXCL_LINE
            i -> _deg2_combos!(
                pool, doff, operators.binops[i], k1, i1, s1, k2, i2, s2, nrows
            ),
        )
        return nothing
    end
end

function _arena_eval(
    a::Arena{T,D},
    cX::Matrix{T},
    operators::OperatorEnum,
    ::Val{early_exit},
    n_slots::Int,
    sp_max::Int,
    fmask::UInt64,
    pool::Matrix{T},
) where {T,D,early_exit}
    nodes = getfield(a, :nodes)
    n = length(nodes)
    nrows = size(cX, 2)

    # Slot layout in the pool: 1 = output; 2 .. 1+n_perm = materialized
    # features; intermediates after that. Slots are addressed by offset; the
    # output view is constructed once, at return.
    n_perm = count_ones(fmask)
    let slot = 1
        rem = fmask
        while rem != 0
            f = trailing_zeros(rem) + 1
            slot += 1
            off = (slot - 1) * nrows
            @inbounds @simd for j in 1:nrows
                pool[off + j] = cX[f, j]
            end
            rem &= rem - 1
        end
    end

    # Per-call descriptor state (tiny; the pool itself is caller-owned):
    desc = Vector{Int64}(undef, sp_max + n_slots)
    svals = Vector{T}(undef, sp_max)
    fbase = sp_max
    sp = 0
    nfree = 0
    next_slot = Int32(1 + n_perm)
    out() = @inbounds @view(pool[1, :])

    @inbounds for i in 1:n
        e = nodes[i]
        d = e.degree
        is_root = i == n
        if d == 0x00
            sp += 1
            if e.constant
                desc[sp] = Int64(_K_SCALAR)
                svals[sp] = e.val
            else
                f = Int(e.feature)
                fslot = count_ones(fmask & ((UInt64(1) << (f - 1)) - 1)) + 2
                desc[sp] = Int64(_K_PSLOT) | (Int64(fslot) << 2)
            end
        elseif d == 0x01
            dtop = desc[sp]
            k = UInt8(dtop & 3)
            if k == _K_SCALAR
                v = _scalar_deg1(e.op, svals[sp], operators)
                is_valid(v) || return ResultOk(out(), false)
                svals[sp] = v
            else
                srci = Int32(dtop >> 2)
                if is_root
                    s = Int32(1)
                    desc[sp] = Int64(_K_SLOT) | (Int64(s) << 2)
                elseif k == _K_SLOT
                    s = srci
                else
                    if nfree > 0
                        s = Int32(desc[fbase + nfree])
                        nfree -= 1
                    else
                        next_slot += Int32(1)
                        s = next_slot
                    end
                    desc[sp] = Int64(_K_SLOT) | (Int64(s) << 2)
                end
                doff = _slotoff(s, nrows)
                _dispatch_deg1!(pool, doff, e.op, srci, nrows, operators)
                if early_exit && !_valid_slot(pool, doff, nrows)
                    return ResultOk(out(), false)
                end
            end
        else
            d1 = desc[sp - 1]
            d2 = desc[sp]
            k1 = UInt8(d1 & 3)
            k2 = UInt8(d2 & 3)
            i1 = Int32(d1 >> 2)
            i2 = Int32(d2 >> 2)
            s1 = k1 == _K_SCALAR ? svals[sp - 1] : zero(T)
            s2 = k2 == _K_SCALAR ? svals[sp] : zero(T)
            sp -= 1
            if k1 == _K_SCALAR && k2 == _K_SCALAR
                v = _scalar_deg2(e.op, s1, s2, operators)
                is_valid(v) || return ResultOk(out(), false)
                desc[sp] = Int64(_K_SCALAR)
                svals[sp] = v
            else
                # free recyclable argument slots first; the destination may
                # then reuse one (kernels are alias-safe for exact overlap)
                if k1 == _K_SLOT
                    nfree += 1
                    desc[fbase + nfree] = Int64(i1)
                end
                if k2 == _K_SLOT
                    nfree += 1
                    desc[fbase + nfree] = Int64(i2)
                end
                if is_root
                    s = Int32(1)
                else
                    if nfree > 0
                        s = Int32(desc[fbase + nfree])
                        nfree -= 1
                    else
                        next_slot += Int32(1)
                        s = next_slot
                    end
                end
                desc[sp] = Int64(_K_SLOT) | (Int64(s) << 2)
                doff = _slotoff(s, nrows)
                _dispatch_deg2!(pool, doff, e.op, k1, i1, s1, k2, i2, s2, nrows, operators)
                if early_exit && !_valid_slot(pool, doff, nrows)
                    return ResultOk(out(), false)
                end
            end
        end
    end

    # Root never went through a kernel (bare leaf or fully folded scalar), or
    # an op-root wrote into a non-output slot via in-place deg1 reuse.
    kroot = UInt8(desc[1] & 3)
    if kroot == _K_SCALAR
        v = svals[1]
        is_valid(v) || return ResultOk(out(), false)
        @inbounds @simd for j in 1:nrows
            pool[j] = v
        end
    elseif Int32(desc[1] >> 2) != Int32(1)
        soff = _slotoff(Int32(desc[1] >> 2), nrows)
        @inbounds @simd for j in 1:nrows
            pool[j] = pool[soff + j]
        end
    end
    # Move the result from chunk 1 (linear 1:nrows) into row 1, so the
    # returned view has the same type as the generic buffered evaluator's
    # `@view(buffer.array[i, :])` (keeping `eval_tree_array` type stable).
    # Chunk and row overlap in memory; iterating downward is safe: when
    # reading chunk index j, every already-written row position (j''-1)*B+1
    # with j'' > j exceeds j for B >= 2, and for B == 1 chunk and row
    # coincide elementwise.
    B = size(pool, 1)
    if B > 1
        @inbounds for j in nrows:-1:1
            pool[1, j] = pool[j]
        end
    end
    return ResultOk(out(), true)
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
