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
import ..NodeUtilsModule: get_scalar_constants, set_scalar_constants!, is_node_constant
import ..NodePreallocationModule: allocate_container, copy_into!
import ..ValueInterfaceModule: get_number_type, is_valid, is_valid_array
import ..OperatorEnumModule: OperatorEnum
import ..EvaluateModule: _eval_tree_array, EvalOptions

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
    c = @inbounds n.arena.nodes[n.idx].children[i]
    c == 0 && throw(UndefRefError())
    return ArenaNode{T,D}(n.arena, c)
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
# Iterative postfix evaluation
################################################################################

"""Iterative postfix evaluation over the flat entry array.

For a compact arena the entries are already in children-before-parent order,
so evaluation is a single left-to-right pass with a value stack: no recursion
and no per-node facade traversal. Stack slots are tagged scalar-or-buffer;
constant subtrees stay in the scalar lane (one flop per node) until they meet
a vector operand, which reproduces the generic evaluator's constant-folding
"speed hack" without its per-call `is_constant` traversals. Buffers are
recycled through a free list, so the number of allocated buffers is the
maximum number of simultaneously live vector operands (~tree depth), not the
node count.

Applies for `D == 2`, numeric `T`, and default options; anything else falls
back to the generic recursive evaluator.
"""
function _eval_tree_array(
    tree::ArenaNode{T,D},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    eval_options::EvalOptions,
)::ResultOk where {T<:Number,D}
    if !(
        D == 2 &&
        cX isa Matrix{T} &&
        is_compact_root(tree) &&
        eval_options.turbo isa Val{false} &&
        eval_options.buffer === nothing
    )
        return invoke(
            _eval_tree_array,
            Tuple{AbstractExpressionNode{T,D},AbstractMatrix{T},OperatorEnum,EvalOptions},
            tree,
            cX,
            operators,
            eval_options,
        )
    end
    return _arena_eval(getfield(tree, :arena), cX, operators, eval_options.early_exit)
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
@generated function _kern_deg1!(
    dest::AbstractVector{T}, op_idx::UInt8, x, operators::O
) where {T,O<:OperatorEnum}
    nops = length(O.parameters[1].parameters[1].parameters)
    quote
        Base.Cartesian.@nif(
            $nops,
            i -> i == Int(op_idx),  # COV_EXCL_LINE
            i -> (dest.=operators.unaops[i].(x); nothing),
        )
        return nothing
    end
end
@generated function _kern_deg2!(
    dest::AbstractVector{T}, op_idx::UInt8, x, y, operators::O
) where {T,O<:OperatorEnum}
    nops = length(O.parameters[1].parameters[2].parameters)
    quote
        Base.Cartesian.@nif(
            $nops,
            i -> i == Int(op_idx),  # COV_EXCL_LINE
            i -> (dest.=operators.binops[i].(x, y); nothing),
        )
        return nothing
    end
end

"""Per-task reusable evaluation workspace. Stored in task-local storage, so
concurrent evaluations from different tasks never share state. Buffers in
`free` persist across calls; the returned output array always escapes to the
caller, so steady-state evaluation allocates exactly one array per call."""
mutable struct ArenaEvalWorkspace{T}
    tags::Vector{Bool}
    svals::Vector{T}
    bufs::Vector{Vector{T}}
    free::Vector{Vector{T}}
end
function ArenaEvalWorkspace{T}() where {T}
    return ArenaEvalWorkspace{T}(Bool[], T[], Vector{T}[], Vector{T}[])
end

@inline function _eval_workspace(::Type{T}) where {T}
    tls = task_local_storage()
    ws = get!(() -> ArenaEvalWorkspace{T}(), tls, (:__de_arena_eval_workspace, T))
    return ws::ArenaEvalWorkspace{T}
end

@inline function _acquire!(ws::ArenaEvalWorkspace{T}, nrows::Int) where {T}
    isempty(ws.free) && return Vector{T}(undef, nrows)
    buf = pop!(ws.free)
    length(buf) == nrows || resize!(buf, nrows)
    return buf
end

"""Move all remaining stack buffers (except the escaping `out`) to the free
list for reuse by the next call."""
@inline function _release_except!(ws::ArenaEvalWorkspace, out)
    for b in ws.bufs
        b === out || push!(ws.free, b)
    end
    empty!(ws.bufs)
    return nothing
end

function _arena_eval(
    a::Arena{T,D}, cX::Matrix{T}, operators::OperatorEnum, ::Val{early_exit}
)::ResultOk{Vector{T}} where {T,D,early_exit}
    nodes = getfield(a, :nodes)
    n = length(nodes)
    nrows = size(cX, 2)
    rows = 1:nrows

    ws = _eval_workspace(T)
    tags = ws.tags
    svals = ws.svals
    bufs = ws.bufs
    empty!(tags)
    empty!(svals)
    empty!(bufs)

    @inbounds for i in 1:n
        e = nodes[i]
        d = e.degree
        if d == 0x00
            if e.constant
                push!(tags, true)
                push!(svals, e.val)
            else
                buf = _acquire!(ws, nrows)
                feat = Int(e.feature)
                @simd for j in rows
                    buf[j] = cX[feat, j]
                end
                push!(tags, false)
                push!(bufs, buf)
            end
        elseif d == 0x01
            if tags[end]
                v = _scalar_deg1(e.op, svals[end], operators)
                if !is_valid(v)
                    out = _acquire!(ws, nrows)
                    _release_except!(ws, out)
                    return ResultOk(out, false)
                end
                svals[end] = v
            else
                buf = bufs[end]
                _kern_deg1!(buf, e.op, buf, operators)
                if early_exit && !is_valid_array(buf)
                    _release_except!(ws, buf)
                    return ResultOk(buf, false)
                end
            end
        else
            rscal = pop!(tags)
            lscal = tags[end]
            if lscal & rscal
                v = _scalar_deg2(e.op, svals[end - 1], svals[end], operators)
                if !is_valid(v)
                    out = _acquire!(ws, nrows)
                    _release_except!(ws, out)
                    return ResultOk(out, false)
                end
                pop!(svals)
                svals[end] = v
            else
                if lscal  # scalar op buffer
                    buf = bufs[end]
                    _kern_deg2!(buf, e.op, pop!(svals), buf, operators)
                elseif rscal  # buffer op scalar
                    buf = bufs[end]
                    _kern_deg2!(buf, e.op, buf, pop!(svals), operators)
                else  # buffer op buffer
                    r = pop!(bufs)
                    buf = bufs[end]
                    _kern_deg2!(buf, e.op, buf, r, operators)
                    push!(ws.free, r)
                end
                tags[end] = false
                if early_exit && !is_valid_array(buf)
                    _release_except!(ws, buf)
                    return ResultOk(buf, false)
                end
            end
        end
    end

    if tags[end]
        out = _acquire!(ws, nrows)
        fill!(out, svals[end])
        _release_except!(ws, out)
        return ResultOk(out, true)
    end
    out = bufs[end]
    _release_except!(ws, out)
    return ResultOk(out, true)
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
