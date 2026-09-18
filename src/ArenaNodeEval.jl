module ArenaNodeEvalModule

using ..UtilsModule: ResultOk
import ..NodeModule: AbstractExpressionNode
using ..ValueInterfaceModule: is_valid
using ..NodeUtilsModule: is_constant
import ..OperatorEnumModule: OperatorEnum
import ..EvaluateModule: _eval_tree_array, EvalContext, ArrayBuffer, get_nops, next_index!
import ..ArenaNodeModule:
    ArenaNode, Arena, ArenaEntry, get_arena, get_index, is_compact_root

################################################################################
# Plan-style buffered evaluation
################################################################################

# Plan-style postfix evaluation into reserved rows of the caller-provided
# EvalContext.buffer. The first reserved row is the output; used features and
# recyclable intermediates occupy the remaining rows. The buffer index advances
# past the entire reservation so earlier results remain valid until reset.
# Without a sufficient buffer or for constant trees the generic evaluator is
# used. Other fallbacks: non-compact arena, non-isbits T (the branchless
# kernels issue dead loads from unwritten slots), depth or feature count over
# 64, turbo, or use_fused=Val(false) (callers may overload deg1_eval etc.,
# which this path bypasses).
function _eval_tree_array(
    tree::ArenaNode{T,D},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    eval_context::EvalContext,
)::ResultOk where {T<:Number,D}
    buffer = eval_context.buffer
    if buffer isa ArrayBuffer{Matrix{T}} &&
        isbitstype(T) &&
        cX isa Matrix{T} &&
        size(buffer.array, 2) == size(cX, 2) &&
        is_compact_root(tree) &&
        eval_context.turbo isa Val{false} &&
        eval_context.use_fused isa Val{true} &&
        !is_constant(tree)
        ok_plan, num_slots, max_stack, feature_mask = _plan_scratch(get_arena(tree))
        # +1 for the output slot; reserve the whole range so earlier results
        # remain valid until the caller explicitly resets the buffer.
        available_slots = size(buffer.array, 1) - buffer.index[]
        if ok_plan && num_slots + 1 <= available_slots
            output_slot = next_index!(buffer)
            buffer.index[] += num_slots
            return _arena_eval(
                get_arena(tree),
                cX,
                operators,
                eval_context.early_exit,
                num_slots,
                max_stack,
                feature_mask,
                buffer.array,
                output_slot,
            )
        end
    end
    return invoke(
        _eval_tree_array,
        Tuple{AbstractExpressionNode{T,D},AbstractMatrix{T},OperatorEnum,EvalContext},
        tree,
        cX,
        operators,
        eval_context,
    )
end

# Pool slot of materialized `feature`: slot 1 is the output; features fill
# slots 2, 3, ... in ascending feature order.
function _feature_slot(feature_mask::UInt64, feature::Integer)
    return count_ones(feature_mask & (_feature_bit(feature) - 1)) + 2
end

# Descriptor kinds for evaluation stack slots:
const _K_SCALAR = 0x00  # folded constant; value lives in the scalar lane
const _K_PSLOT = 0x01   # permanent slot (output or a materialized feature)
const _K_SLOT = 0x02    # recyclable slot (an intermediate)

# The planner (`_plan_scratch`) and the executor (`_push_leaf!`/`_exec_op!`)
# walk the same postfix program, so they must make identical kind and
# recycling decisions: the executor trusts the planner's slot counts under
# `@inbounds`. These three functions are the single source of that policy.

# Leaves: constants fold into the scalar lane, features live in permanent slots.
_leaf_kind(entry::ArenaEntry) = entry.constant ? _K_SCALAR : _K_PSLOT

# Operators: an all-scalar application constant-folds; anything else lands in
# a recyclable intermediate slot.
_op_result_kind(all_args_scalar::Bool) = all_args_scalar ? _K_SCALAR : _K_SLOT

# Whether consuming an operand of this kind frees its slot for reuse.
_is_recyclable(kind::UInt8) = kind == _K_SLOT

# A stack descriptor is an Int64 packing a kind (low 2 bits) with a slot
# index; scalar descriptors carry no slot (their value lives in the scalar
# lane). `_feature_bit` is the feature's position in the `feature_mask`
# bitset of used features.
_pack_descriptor(kind::UInt8, slot::Integer=0) = Int64(kind) | (Int64(slot) << 2)
_descriptor_kind(descriptor::Int64) = UInt8(descriptor & 3)
_descriptor_slot(descriptor::Int64) = Int32(descriptor >> 2)
_feature_bit(feature::Integer) = UInt64(1) << (feature - 1)

# Alloc-free stack of descriptor kinds for the planner: two bitmask lanes
# (bit 1 = top) mark `_K_SCALAR`/`_K_PSLOT`; neither lane set = `_K_SLOT`.
# Capacity is 64 entries.
struct KindStack
    scalar::UInt64
    permanent::UInt64
end

function _push_kind(kinds::KindStack, kind::UInt8)
    return KindStack(
        (kinds.scalar << 1) | (kind == _K_SCALAR),
        (kinds.permanent << 1) | (kind == _K_PSLOT),
    )
end
function _pop_kinds(kinds::KindStack, count::UInt8)
    return KindStack(kinds.scalar >> count, kinds.permanent >> count)
end
function _args_all_scalar(kinds::KindStack, degree::UInt8)
    arity_mask = (UInt64(1) << degree) - 1
    return (kinds.scalar & arity_mask) == arity_mask
end
function _count_recyclable_args(kinds::KindStack, degree::UInt8)
    arity_mask = (UInt64(1) << degree) - 1
    return count_ones(~kinds.scalar & ~kinds.permanent & arity_mask)
end

# Alloc-free pre-pass: record used features and simulate the descriptor stack
# to count slots. Makes the same kind/recycling decisions as the executor (the
# shared policy functions above), so the counts are exact. Trees deeper than
# 64 or features beyond 64 report failure and take the generic path.
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
            if kind == _K_PSLOT
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

# `args` is `Tuple{T,Vararg{T}}` (not `NTuple{A,T}`) so that `T` stays bound
# for empty tuples (Aqua's unbound-args check); the arity comes from the
# tuple type itself.
@generated function _scalar_degn(
    op_idx::UInt8, args::Tuple{T,Vararg{T}}, operators::O
) where {T,O<:OperatorEnum}
    A = length(args.parameters)
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

# Branchless arity-generic kernel: each operand selects per element between
# its scalar value and its pool row (scalar operands use the output row as a
# harmless address for the unselected branch), so an arity-A operator needs
# one kernel rather than 2^A variants.
@generated function _kern_n!(
    pool::Matrix{T},
    dest_slot::Int,
    op::F,
    is_scalar::NTuple{A,Bool},
    scalar_args::NTuple{A,T},
    offsets::NTuple{A,Int},
    num_rows::Int,
) where {T,F,A}
    quote
        @inbounds @simd for j in 1:num_rows
            pool[dest_slot, j] = Base.Cartesian.@ncall(
                $A, op, k -> ifelse(is_scalar[k], scalar_args[k], pool[offsets[k], j])
            )
        end
        return nothing
    end
end
# Validity check over a pool row without constructing a view.
function _valid_slot(pool::Matrix{T}, slot::Int, num_rows::Int) where {T}
    total = zero(T)
    @inbounds @simd for j in 1:num_rows
        total += pool[slot, j]
    end
    return is_valid(total)
end

@generated function _dispatch_degn!(
    ::Val{A},
    pool::Matrix{T},
    dest_slot::Int,
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
                dest_slot,
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

# Loop-invariant evaluation state.
struct PlanState{T}
    pool::Matrix{T}
    descriptors::Vector{Int64}
    scalar_vals::Vector{T}
    free_base::Int
    nrows::Int
    base_slot::Int
end

# Descriptor stack top, free-list length, and high-water slot, threaded
# through `_push_leaf!`/`_exec_op!`.
struct PlanRegisters
    stack_top::Int
    num_free::Int
    next_slot::Int32
end

function _push_leaf!(
    state::PlanState{T}, regs::PlanRegisters, entry::ArenaEntry{T}, feature_mask::UInt64
) where {T}
    stack_top = regs.stack_top + 1
    @inbounds if _leaf_kind(entry) == _K_SCALAR
        state.descriptors[stack_top] = _pack_descriptor(_K_SCALAR)
        state.scalar_vals[stack_top] = entry.val
    else
        feature_slot = _feature_slot(feature_mask, entry.feature)
        state.descriptors[stack_top] = _pack_descriptor(_K_PSLOT, feature_slot)
    end
    return PlanRegisters(stack_top, regs.num_free, regs.next_slot)
end

# Dispatch the runtime degree to a compile-time arity, then fold (all-scalar
# operands) or run the array kernel. Returns `(regs, ok)`.
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

# Pop the top `A` operand descriptors; constant-fold if every operand is a
# scalar, otherwise run the kernel.
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
                $A, k -> if kinds[k] == _K_SCALAR
                    scalar_vals[stack_top - $A + k]
                else
                    zero(T)
                end
            )
        end
        regs = PlanRegisters(stack_top - ($A - 1), num_free, next_slot)
        all_args_scalar = Base.Cartesian.@nall($A, k -> kinds[k] == _K_SCALAR)
        if _op_result_kind(all_args_scalar) == _K_SCALAR
            return _fold_constant_args!(
                state, regs, op_idx, scalar_args, early_exit, operators
            )
        end
        return _run_op_kernel!(
            state, regs, op_idx, kinds, idxs, scalar_args, is_root, early_exit, operators
        )
    end
end

# Constant-fold an all-scalar operator at the (already popped) stack top.
# With early exit enabled, validate operands and the folded result to match
# the generic evaluator; otherwise retain the operator's invalid result.
function _fold_constant_args!(
    state::PlanState{T},
    regs::PlanRegisters,
    op_idx::UInt8,
    scalar_args::NTuple{A,T},
    early_exit::Bool,
    operators::OperatorEnum,
) where {A,T}
    early_exit && !all(is_valid, scalar_args) && return (regs, false)
    value = _scalar_degn(op_idx, scalar_args, operators)
    early_exit && !is_valid(value) && return (regs, false)
    @inbounds state.descriptors[regs.stack_top] = _pack_descriptor(_K_SCALAR)
    @inbounds state.scalar_vals[regs.stack_top] = value
    return (regs, true)
end

# Recycle the freed argument slots, allocate the destination (slot 1 at the
# root), and dispatch the kernel. `early_exit` validation mirrors the generic
# evaluator at lower cost: scalar operands are checked at consumption (O(1));
# slot operands are covered by checking each kernel output at production
# (every non-root intermediate is consumed exactly once, so this rejects the
# same trees as per-consumption checks); features are validated once at
# materialization; the root output is never checked, as in the generic
# evaluator.
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
        (; pool, descriptors, free_base, nrows, base_slot) = state
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
        @inbounds descriptors[stack_top] = _pack_descriptor(_K_SLOT, slot)
        dest_slot = base_slot + slot - 1
        is_scalar = Base.Cartesian.@ntuple($A, k -> kinds[k] == _K_SCALAR)
        offsets = Base.Cartesian.@ntuple(
            $A, k -> kinds[k] == _K_SCALAR ? base_slot : base_slot + idxs[k] - 1
        )
        regs = PlanRegisters(stack_top, num_free, next_slot)
        scalars_valid =
            !early_exit ||
            Base.Cartesian.@nall($A, k -> !is_scalar[k] || is_valid(scalar_args[k]))
        scalars_valid || return (regs, false)
        _dispatch_degn!(
            Val($A),
            pool,
            dest_slot,
            op_idx,
            is_scalar,
            scalar_args,
            offsets,
            nrows,
            operators,
        )
        if early_exit && !is_root && !_valid_slot(pool, dest_slot, nrows)
            return (regs, false)
        end
        return (regs, true)
    end
end

# Copy each used feature column into its pinned pool slot (layout: 1 =
# output; 2 .. 1+num_features = features, ascending). Validity is checked
# once per feature here, replacing per-consumption checks: every
# materialized feature is consumed by some operator -- except in a
# single-leaf tree, where `check_validity` is passed as false to match
# `deg0_eval`, which never validates a bare leaf.
function _materialize_features!(
    pool::Matrix{T},
    cX::Matrix{T},
    feature_mask::UInt64,
    nrows::Int,
    check_validity::Bool,
    base_slot::Int,
) where {T}
    slot = 1
    remaining = feature_mask
    while !iszero(remaining)
        feature = trailing_zeros(remaining) + 1
        slot += 1
        pool_slot = base_slot + slot - 1
        @inbounds @simd for j in 1:nrows
            pool[pool_slot, j] = cX[feature, j]
        end
        # Separate validity pass over the just-written row keeps the copy
        # loop independent from validation.
        check_validity && !_valid_slot(pool, pool_slot, nrows) && return false
        remaining &= remaining - 1
    end
    return true
end

# Land the result in the caller-owned output row.
function _write_root_to_output!(
    pool::Matrix{T},
    descriptors::Vector{Int64},
    scalar_vals::Vector{T},
    nrows::Int,
    output_slot::Int,
    base_slot::Int,
) where {T}
    # Root never went through a kernel (bare leaf or fully folded scalar), or
    # an op-root wrote into a non-output slot via in-place deg1 reuse. A bare
    # leaf root is never validity-checked (deg0_eval semantics); a folded
    # scalar root is already valid by induction.
    root_kind = _descriptor_kind(descriptors[1])
    root_slot = _descriptor_slot(descriptors[1])
    if root_kind == _K_SCALAR
        value = scalar_vals[1]
        @inbounds @simd for j in 1:nrows
            pool[output_slot, j] = value
        end
    elseif !isone(root_slot)
        root_row = base_slot + root_slot - 1
        @inbounds @simd for j in 1:nrows
            pool[output_slot, j] = pool[root_row, j]
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
    output_slot::Int,
) where {T,D,early_exit}
    nodes = arena.nodes
    num_nodes = length(nodes)
    nrows = size(cX, 2)
    num_features = count_ones(feature_mask)
    base_slot = output_slot
    output = @view(pool[output_slot, :])

    check_features = early_exit && num_nodes > 1
    if !_materialize_features!(pool, cX, feature_mask, nrows, check_features, base_slot)
        return ResultOk(output, false)
    end

    # Per-call descriptor state (tiny; the pool itself is caller-owned):
    descriptors = Vector{Int64}(undef, max_stack + num_slots)
    scalar_vals = Vector{T}(undef, max_stack)
    state = PlanState(pool, descriptors, scalar_vals, max_stack, nrows, base_slot)
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

    _write_root_to_output!(
        pool, descriptors, scalar_vals, nrows, output_slot, base_slot
    )
    return ResultOk(output, true)
end

end
