module OperatorEnumConstructionModule

using DispatchDoctor: @unstable

import ..OperatorEnumModule: AbstractOperatorEnum, OperatorEnum, GenericOperatorEnum
import ..NodeModule: Node, GraphNode, AbstractExpressionNode, constructorof
import ..StringsModule: string_tree
import ..EvaluateModule: eval_tree_array, OPERATOR_LIMIT_BEFORE_SLOWDOWN
import ..EvaluateDerivativeModule: eval_grad_tree_array, _zygote_gradient
import ..EvaluationHelpersModule: _grad_evaluator

"""Used to set a default value for `operators` for ease of use."""
@enum AvailableOperatorTypes::UInt8 begin
    IsNothing
    IsOperatorEnum
    IsGenericOperatorEnum
end

# These constants are purely for convenience. Internal code
# should make use of `Node`, `string_tree`, `eval_tree_array`,
# and `eval_grad_tree_array` directly.

const LATEST_OPERATORS = Ref{Union{Nothing,AbstractOperatorEnum}}(nothing)
const LATEST_OPERATORS_TYPE = Ref{AvailableOperatorTypes}(IsNothing)
const OP_FIELDTYPE = fieldtype(Node{Float64}, :op)
const LATEST_OPERATOR_MAPPING = Dict{Int,Dict{Any,OP_FIELDTYPE}}()
const ALREADY_DEFINED_OPERATORS = (;
    operator_enum=Dict{DataType,Dict{Int,Dict{Any,Bool}}}(),
    generic_operator_enum=Dict{DataType,Dict{Int,Dict{Any,Bool}}}(),
)
const LATEST_VARIABLE_NAMES = Ref{Vector{String}}(String[])
const LATEST_LOCK = Threads.SpinLock()

function Base.show(io::IO, tree::AbstractExpressionNode)
    latest_operators_type = LATEST_OPERATORS_TYPE.x
    kwargs = (variable_names=LATEST_VARIABLE_NAMES.x,)
    if latest_operators_type == IsNothing
        return print(io, string_tree(tree; kwargs...))
    elseif latest_operators_type == IsOperatorEnum
        latest_operators = LATEST_OPERATORS.x::OperatorEnum
        return print(io, string_tree(tree, latest_operators; kwargs...))
    else
        latest_operators = LATEST_OPERATORS.x::GenericOperatorEnum
        return print(io, string_tree(tree, latest_operators; kwargs...))
    end
end
@unstable function (tree::AbstractExpressionNode)(X; kws...)
    Base.depwarn(
        "The `tree(X; kws...)` syntax is deprecated. Use `tree(X, operators; kws...)` instead.",
        :AbstractExpressionNode,
    )
    latest_operators_type = LATEST_OPERATORS_TYPE.x

    latest_operators_type == IsNothing &&
        error("Please use the `tree(X, operators; kws...)` syntax instead.")

    if latest_operators_type == IsOperatorEnum
        latest_operators = LATEST_OPERATORS.x::OperatorEnum
        return tree(X, latest_operators; kws...)
    else
        latest_operators = LATEST_OPERATORS.x::GenericOperatorEnum
        return tree(X, latest_operators; kws...)
    end
end

@unstable function _grad_evaluator(tree::AbstractExpressionNode, X; kws...)
    Base.depwarn(
        "The `tree'(X; kws...)` syntax is deprecated. Use `tree'(X, operators; kws...)` instead.",
        :AbstractExpressionNode,
    )
    latest_operators_type = LATEST_OPERATORS_TYPE.x
    # return _grad_evaluator(tree, X, $operators; kws...)
    latest_operators_type == IsNothing &&
        error("Please use the `tree'(X, operators; kws...)` syntax instead.")
    latest_operators_type == IsGenericOperatorEnum &&
        error("Gradients are not implemented for `GenericOperatorEnum`.")

    latest_operators = LATEST_OPERATORS.x::OperatorEnum
    return _grad_evaluator(tree, X, latest_operators; kws...)
end

function set_default_variable_names!(variable_names::Vector{String})
    return LATEST_VARIABLE_NAMES.x = copy(variable_names)
end

Base.@deprecate create_evaluation_helpers!(operators) set_default_operators!(operators)

function set_default_operators!(operators::OperatorEnum)
    LATEST_OPERATORS.x = operators
    return LATEST_OPERATORS_TYPE.x = IsOperatorEnum
end
function set_default_operators!(operators::GenericOperatorEnum)
    LATEST_OPERATORS.x = operators
    return LATEST_OPERATORS_TYPE.x = IsGenericOperatorEnum
end

@unstable function lookup_op(@nospecialize(f), ::Val{degree}) where {degree}
    mapping = get!(Dict{Any,OP_FIELDTYPE}, LATEST_OPERATOR_MAPPING, degree)
    if !haskey(mapping, f)
        error(
            "Convenience constructor for operator `$(f)` for degree $(degree) is out-of-date. " *
            "Please create an `OperatorEnum` (or `GenericOperatorEnum`) containing " *
            "the operator `$(f)` which will define the `$(f)` -> `Int` mapping.",
        )
    end
    return mapping[f]
end

@unstable function _unpack_broadcast_function(f)
    if f isa Broadcast.BroadcastFunction
        return Symbol(f.f), :(Broadcast.BroadcastFunction($(f.f)))
    else
        return Symbol(f), Symbol(f)
    end
end

function _validate_no_ambiguous_broadcasts(operators::AbstractOperatorEnum)
    for op_tuple in operators.ops, op in op_tuple
        if op isa Broadcast.BroadcastFunction &&
            any(other_tuple -> op.f in other_tuple, operators.ops)
            throw(
                ArgumentError(
                    "Usage of both broadcasted and unbroadcasted operator `$(op.f)` is ambiguous",
                ),
            )
        end
    end
    return nothing
end

function empty_all_globals!(; force=true)
    if force || islocked(LATEST_LOCK)
        lock(LATEST_LOCK) do
            LATEST_OPERATORS.x = nothing
            LATEST_OPERATORS_TYPE.x = IsNothing
            foreach(empty!, values(LATEST_OPERATOR_MAPPING))
            LATEST_VARIABLE_NAMES.x = String[]
        end
    end
    return nothing
end

function _extend_unary_operator(
    f_inside::Symbol, f_outside::Symbol, type_requirements, internal
)
    quote
        @gensym _constructorof _AbstractExpressionNode
        quote
            if $$internal
                import ..NodeModule.constructorof as $_constructorof
                import ..NodeModule.AbstractExpressionNode as $_AbstractExpressionNode
            else
                using DynamicExpressions:
                    constructorof as $_constructorof,
                    AbstractExpressionNode as $_AbstractExpressionNode
            end

            function $($f_outside)(
                l::N
            ) where {T<:$($type_requirements),N<:$_AbstractExpressionNode{T}}
                return if (l.degree == 0 && l.constant)
                    $_constructorof(N)(T; val=$($f_inside)(l.val))
                else
                    latest_op_idx = $($lookup_op)($($f_inside), Val(1))
                    $_constructorof(N)(; op=latest_op_idx, children=(l,))
                end
            end
        end
    end
end

function _extend_binary_operator(
    f_inside::Symbol, f_outside::Symbol, type_requirements, build_converters, internal
)
    quote
        @gensym _constructorof _AbstractExpressionNode
        quote
            if $$internal
                import ..NodeModule.constructorof as $_constructorof
                import ..NodeModule.AbstractExpressionNode as $_AbstractExpressionNode
            else
                using DynamicExpressions:
                    constructorof as $_constructorof,
                    AbstractExpressionNode as $_AbstractExpressionNode
            end

            function $($f_outside)(
                l::N, r::N
            ) where {T<:$($type_requirements),N<:$_AbstractExpressionNode{T}}
                if (l.degree == 0 && l.constant && r.degree == 0 && r.constant)
                    $_constructorof(N)(T; val=$($f_inside)(l.val, r.val))
                else
                    latest_op_idx = $($lookup_op)($($f_inside), Val(2))
                    $_constructorof(N)(; op=latest_op_idx, children=(l, r))
                end
            end
            function $($f_outside)(
                l::N, r::T
            ) where {T<:$($type_requirements),N<:$_AbstractExpressionNode{T}}
                if l.degree == 0 && l.constant
                    $_constructorof(N)(T; val=$($f_inside)(l.val, r))
                else
                    latest_op_idx = $($lookup_op)($($f_inside), Val(2))
                    $_constructorof(N)(;
                        op=latest_op_idx, children=(l, $_constructorof(N)(T; val=r))
                    )
                end
            end
            function $($f_outside)(
                l::T, r::N
            ) where {T<:$($type_requirements),N<:$_AbstractExpressionNode{T}}
                if r.degree == 0 && r.constant
                    $_constructorof(N)(T; val=$($f_inside)(l, r.val))
                else
                    latest_op_idx = $($lookup_op)($($f_inside), Val(2))
                    $_constructorof(N)(;
                        op=latest_op_idx, children=($_constructorof(N)(T; val=l), r)
                    )
                end
            end
            if $($build_converters)
                # Converters:
                function $($f_outside)(
                    l::$_AbstractExpressionNode{T1}, r::$_AbstractExpressionNode{T2}
                ) where {T1<:$($type_requirements),T2<:$($type_requirements)}
                    if l isa GraphNode || r isa GraphNode
                        error(
                            "Refusing to promote `GraphNode` as it would break the graph structure. " *
                            "Please convert to a common type first.",
                        )
                    end
                    return $($f_outside)(promote(l, r)...)
                end

                function $($f_outside)(
                    l::$_AbstractExpressionNode{T1}, r::T2
                ) where {T1<:$($type_requirements),T2<:$($type_requirements)}
                    return $($f_outside)(l, convert(T1, r))
                end
                function $($f_outside)(
                    l::T1, r::$_AbstractExpressionNode{T2}
                ) where {T1<:$($type_requirements),T2<:$($type_requirements)}
                    return $($f_outside)(convert(T2, l), r)
                end
            end
        end
    end
end

function _extend_nary_operator(
    degree::Symbol, f_inside::Symbol, f_outside::Symbol, type_requirements, internal
)
    quote
        @gensym _constructorof _AbstractExpressionNode
        arg_syms = [$(Symbol)("arg", i) for i in 1:($(degree))]
        args = [Expr(:(::), arg_syms[i], :N) for i in 1:($(degree))]
        quote
            if $$internal
                import ..NodeModule.constructorof as $_constructorof
                import ..NodeModule.AbstractExpressionNode as $_AbstractExpressionNode
            else
                using DynamicExpressions:
                    constructorof as $_constructorof,
                    AbstractExpressionNode as $_AbstractExpressionNode
            end

            function $($f_outside)(
                $(args...)
            ) where {T<:$($type_requirements),N<:$_AbstractExpressionNode{T}}
                if all(c -> c.degree == 0 && c.constant, ($(arg_syms...),))
                    $_constructorof(N)(
                        T; val=$($f_inside)(map(c -> c.val, ($(arg_syms...),))...)
                    )
                else
                    latest_op_idx = $($lookup_op)($($f_inside), Val($($degree)))
                    $_constructorof(N)(; op=latest_op_idx, children=($(arg_syms...),))
                end
            end
        end
    end
end

function _extend_operators(operators, skip_user_operators, kws, __module__::Module)
    if !all(x -> first(x.args) ∈ (:empty_old_operators, :internal, :on_type), kws)
        error(
            "You passed the keywords $(kws), but only `empty_old_operators`, `internal`, `on_type` are supported.",
        )
    end

    empty_old_operators_idx = findfirst(
        x -> hasproperty(x, :args) && first(x.args) == :empty_old_operators, kws
    )
    internal_idx = findfirst(x -> hasproperty(x, :args) && first(x.args) == :internal, kws)
    on_type_idx = findfirst(x -> hasproperty(x, :args) && first(x.args) == :on_type, kws)

    empty_old_operators = if empty_old_operators_idx !== nothing
        @assert kws[empty_old_operators_idx].head == :(=)
        kws[empty_old_operators_idx].args[2]
    else
        true
    end

    on_type = if on_type_idx !== nothing
        @assert kws[on_type_idx].head == :(=)
        kws[on_type_idx].args[2]
    else
        nothing
    end

    internal = if internal_idx !== nothing
        @assert kws[internal_idx].head == :(=)
        kws[internal_idx].args[2]::Bool
    else
        false
    end

    @gensym f_inside f_outside skip type_requirements build_converters op_exists degree op_tuple
    unary_ex = _extend_unary_operator(f_inside, f_outside, type_requirements, internal)
    binary_ex = _extend_binary_operator(
        f_inside, f_outside, type_requirements, build_converters, internal
    )
    nary_ex = _extend_nary_operator(
        degree, f_inside, f_outside, type_requirements, internal
    )

    #! format: off
    return quote
        local $type_requirements, $build_converters, $op_exists
        $(_validate_no_ambiguous_broadcasts)($operators)
        # Base.@lock($LATEST_LOCK) do
        lock($LATEST_LOCK)
        try
            if isa($operators, $OperatorEnum)
                $type_requirements = $(on_type == nothing ? Number : on_type)
                $build_converters = $(on_type == nothing)
                $op_exists = get!(Dict{Int,Dict{Any,Bool}},$(ALREADY_DEFINED_OPERATORS).operator_enum, $type_requirements)
            else
                $type_requirements = $(on_type == nothing ? Any : on_type)
                $build_converters = false
                $op_exists = get!(Dict{Int,Dict{Any,Bool}},$(ALREADY_DEFINED_OPERATORS).generic_operator_enum, $type_requirements)
            end
            if $(empty_old_operators)
                foreach(empty!, values($(LATEST_OPERATOR_MAPPING)))
            end
            for ($degree, $op_tuple) in enumerate($(operators).ops), (op, func) in enumerate($op_tuple)
                local ($f_outside, $f_inside) = $(_unpack_broadcast_function)(func)
                local $skip = false
                if isdefined(Base, $f_outside)
                    $f_outside = :(Base.$($f_outside))
                elseif $(skip_user_operators)
                    $skip = true
                else
                    $f_outside = :($($__module__).$($f_outside))
                end
                get!(Dict{Any,$(OP_FIELDTYPE)}, $(LATEST_OPERATOR_MAPPING), $degree)[func] = op
                $skip && continue
                let __d = get!(Dict{Any,Bool}, $op_exists, $degree)
                    if !haskey(__d, func)
                        $degree == 1 && eval($unary_ex)
                        $degree == 2 && eval($binary_ex)
                        $degree > 2 && eval($nary_ex)
                        __d[func] = true
                    end
                end
            end
        finally
            unlock($LATEST_LOCK)
        end
    end
    #! format: on
end

"""
    @extend_operators operators [kws...]

Extends all operators defined in this operator enum to work on the
`Node` type. While by default this is already done for operators defined
in `Base` when you create an enum and pass `define_helper_functions=true`,
this does not apply to the user-defined operators. Thus, to do so, you must
apply this macro to the operator enum in the same module you have the operators
defined.
"""
macro extend_operators(operators, kws...)
    ex = _extend_operators(operators, false, kws, __module__)
    expected_type = AbstractOperatorEnum
    return esc(
        quote
            if !isa($(operators), $expected_type)
                error("You must pass an operator enum to `@extend_operators`.")
            end
            $ex
        end,
    )
end

"""
    @extend_operators_base operators [kws...]

Similar to `@extend_operators`, but only extends operators already
defined in `Base`.
`kws` can include `empty_old_operators` which is default `true`,
and `internal` which is default `false`.
"""
macro extend_operators_base(operators, kws...)
    ex = _extend_operators(operators, true, kws, __module__)
    expected_type = AbstractOperatorEnum
    return esc(
        quote
            if !isa($(operators), $expected_type)
                error("You must pass an operator enum to `@extend_operators_base`.")
            end
            $ex
        end,
    )
end

function _pairs_to_ops_tuple(
    @nospecialize(pair::Pair{Int,<:Tuple}), @nospecialize(pairs::Pair{Int,<:Tuple}...)
)
    all_pairs = Pair{Int,Tuple}[pair, pairs...]

    # Find the maximum degree to determine tuple length
    max_degree = maximum(first, all_pairs)

    # Create array of empty tuples with the right length
    ops_array = Any[() for _ in 1:max_degree]

    # Fill in the operators at their respective degrees
    for (degree, operators) in all_pairs
        degree < 1 && throw(ArgumentError("Degree must be ≥ 1, got $degree"))
        ops_array[degree] = operators
    end

    return Tuple(ops_array)
end

"""
    OperatorEnum(pairs::Pair{Int,<:Tuple}...; define_helper_functions::Bool=true, empty_old_operators::Bool=true)

Construct an `OperatorEnum` object, defining the possible expressions. This will also
redefine operators for `AbstractExpressionNode` types, as well as `show`, `print`, and
`(::AbstractExpressionNode)(X)`. It will automatically compute derivatives with `Zygote.jl`.

# Arguments

- `pairs::Pair{Int,<:Tuple}...`: A variable number of `degree => operators` pairs, where each
    `degree` is an integer and `operators` is a tuple of functions.

# Keyword Arguments

- `define_helper_functions::Bool=true`: Whether to define helper functions for creating
    and evaluating node types. Turn this off when doing precompilation. Note that these
    are *not* needed for the package to work; they are purely for convenience.
- `empty_old_operators::Bool=true`: Whether to clear the old operators.

# Examples

```julia
# Simple case - just degree-1 and degree-2 operators
OperatorEnum(1 => (sin, cos, exp), 2 => (+, -, *, /))

# Just degree-2 operators
OperatorEnum(2 => (+, -, *, /))

# Just degree-1 operators
OperatorEnum(1 => (sin, cos, exp, log))

# Advanced: Adding ternary operators
OperatorEnum(1 => (sin, cos), 2 => (+, -, *, /), 3 => (fma,))

# Advanced: direct tuple-of-tuples construction (type stable approach)
OperatorEnum(((sin, cos), (+, -, *, /), (fma,)))
```
"""
@unstable function OperatorEnum(
    @nospecialize(pair::Pair{Int,<:Tuple}),
    @nospecialize(pairs::Pair{Int,<:Tuple}...);
    define_helper_functions::Bool=true,
    empty_old_operators::Bool=true,
    # Deprecated:
    enable_autodiff=nothing,
)
    all_pairs = Any[pair, pairs...]
    !isnothing(enable_autodiff) && Base.depwarn(
        "The option `enable_autodiff` has been deprecated. " *
        "Differential operators are now automatically computed within the gradient call.",
        :OperatorEnum,
    )
    for (op, s) in map(p -> (p.second, string("degree", p.first)), all_pairs)
        if length(op) > OPERATOR_LIMIT_BEFORE_SLOWDOWN
            @warn(
                "You have passed over $(OPERATOR_LIMIT_BEFORE_SLOWDOWN) $(s) operators. " *
                    "To prevent long compilation times, some optimizations will be disabled. " *
                    "If this presents an issue, please open an issue on https://github.com/SymbolicML/DynamicExpressions.jl"
            )
            break
        end
    end

    if define_helper_functions && any(
        op_set -> any(op -> op isa Broadcast.BroadcastFunction, op_set),
        map(last, all_pairs),
    )
        # TODO: Fix issue with defining operators on a `BroadcastFunction`
        # and then on a regular function
        @warn "Using `BroadcastFunction` in an `OperatorEnum` is not yet stable"
    end

    operators = OperatorEnum(_pairs_to_ops_tuple(pair, pairs...))
    if define_helper_functions
        @extend_operators_base operators empty_old_operators = empty_old_operators
        set_default_operators!(operators)
    end
    return operators
end

"""
    GenericOperatorEnum(pairs::Pair{Int,<:Tuple}...; options...)

Construct a `GenericOperatorEnum` object, defining possible expressions.
Unlike `OperatorEnum`, this enum one will work arbitrary operators and data types.
This will also redefine operators for `AbstractExpressionNode` types, as well as `show`, `print`,
and `(::AbstractExpressionNode)(X)`.

# Arguments

- `pairs::Pair{Int,<:Tuple}...`: A variable number of `degree => operators` pairs, where each
    `degree` is an integer and `operators` is a tuple of functions.

# Keyword Arguments

- `define_helper_functions::Bool=true`: Whether to define helper functions for creating
    and evaluating node types. Turn this off when doing precompilation. Note that these
    are *not* needed for the package to work; they are purely for convenience.
- `empty_old_operators::Bool=true`: Whether to clear the old operators.

# Examples

```julia
# For vector operations
vec_add(x, y) = x .+ y
vec_square(x) = x .* x
GenericOperatorEnum(1 => (vec_square,), 2 => (vec_add,))

# For string operations
GenericOperatorEnum(1 => (reverse,), 2 => (*,))
```
"""
function GenericOperatorEnum(
    @nospecialize(pair::Pair{Int,<:Tuple}),
    @nospecialize(pairs::Pair{Int,<:Tuple}...);
    define_helper_functions::Bool=true,
    empty_old_operators::Bool=true,
)
    operators = GenericOperatorEnum(_pairs_to_ops_tuple(pair, pairs...))
    if define_helper_functions
        @extend_operators_base operators empty_old_operators = empty_old_operators
        set_default_operators!(operators)
    end
    return operators
end

# Make compatible with vector of operators too
for OP_ENUM in (:OperatorEnum, :GenericOperatorEnum)
    @eval function $OP_ENUM(
        @nospecialize(pair::Pair{Int,<:Vector}),
        @nospecialize(pairs::Pair{Int,<:Vector}...);
        kws...,
    )
        return $OP_ENUM(
            map(p -> Pair(p.first, Tuple(p.second)), (pair, pairs...))...; kws...
        )
    end
end

# Predefine the most common operators so the errors
# are more informative
function _overload_common_operators()
    # Overload the operators in batches (so that we don't hit the warning
    # about too many operators)
    operators = OperatorEnum((
        (sin, cos, tan, exp, log, log1p, log2, log10, sqrt, cbrt, abs, sinh),
        (+, -, *, /, ^, max, min, mod),
    ))
    @extend_operators(operators, empty_old_operators = false, internal = true)
    operators = OperatorEnum((
        (cosh, tanh, atan, asinh, acosh, round, sign, floor, ceil), ()
    ))
    @extend_operators(operators, empty_old_operators = true, internal = true)

    foreach(empty!, values(LATEST_OPERATOR_MAPPING))
    return nothing
end
_overload_common_operators()

end
