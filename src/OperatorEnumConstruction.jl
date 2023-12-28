module OperatorEnumConstructionModule

import ..OperatorEnumModule: AbstractOperatorEnum, OperatorEnum, GenericOperatorEnum
import ..EquationModule: string_tree, Node, GraphNode, AbstractExpressionNode, constructorof
import ..EvaluateEquationModule: eval_tree_array
import ..EvaluateEquationDerivativeModule: eval_grad_tree_array, _zygote_gradient
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
const LATEST_UNARY_OPERATOR_MAPPING = Dict{Function,fieldtype(Node{Float64}, :op)}()
const LATEST_BINARY_OPERATOR_MAPPING = Dict{Function,fieldtype(Node{Float64}, :op)}()
const ALREADY_DEFINED_UNARY_OPERATORS = (;
    operator_enum=Dict{Function,Bool}(), generic_operator_enum=Dict{Function,Bool}()
)
const ALREADY_DEFINED_BINARY_OPERATORS = (;
    operator_enum=Dict{Function,Bool}(), generic_operator_enum=Dict{Function,Bool}()
)
const LATEST_VARIABLE_NAMES = Ref{Vector{String}}(String[])

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
function (tree::AbstractExpressionNode)(X; kws...)
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

function _grad_evaluator(tree::AbstractExpressionNode, X; kws...)
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

Base.@deprecate create_evaluation_helpers! set_default_operators!

function set_default_operators!(operators::OperatorEnum)
    LATEST_OPERATORS.x = operators
    return LATEST_OPERATORS_TYPE.x = IsOperatorEnum
end
function set_default_operators!(operators::GenericOperatorEnum)
    LATEST_OPERATORS.x = operators
    return LATEST_OPERATORS_TYPE.x = IsGenericOperatorEnum
end

function lookup_op(@nospecialize(f), ::Val{degree}) where {degree}
    mapping = degree == 1 ? LATEST_UNARY_OPERATOR_MAPPING : LATEST_BINARY_OPERATOR_MAPPING
    if !haskey(mapping, f)
        error(
            "Convenience constructor for operator `$(f)` is out-of-date. " *
            "Please create an `OperatorEnum` (or `GenericOperatorEnum`) containing " *
            "the operator `$(f)` which will define the `$(f)` -> `Int` mapping.",
        )
    end
    return mapping[f]
end

function _extend_unary_operator(f::Symbol, type_requirements, internal)
    quote
        @gensym _constructorof _AbstractExpressionNode
        quote
            if $$internal
                import ..EquationModule.constructorof as $_constructorof
                import ..EquationModule.AbstractExpressionNode as $_AbstractExpressionNode
            else
                using DynamicExpressions:
                    constructorof as $_constructorof,
                    AbstractExpressionNode as $_AbstractExpressionNode
            end

            function $($f)(
                l::N
            ) where {T<:$($type_requirements),N<:$_AbstractExpressionNode{T}}
                return if (l.degree == 0 && l.constant)
                    $_constructorof(N)(T; val=$($f)(l.val::T))
                else
                    latest_op_idx = $($lookup_op)($($f), Val(1))
                    $_constructorof(N)(latest_op_idx, l)
                end
            end
        end
    end
end

function _extend_binary_operator(f::Symbol, type_requirements, build_converters, internal)
    quote
        @gensym _constructorof _AbstractExpressionNode
        quote
            if $$internal
                import ..EquationModule.constructorof as $_constructorof
                import ..EquationModule.AbstractExpressionNode as $_AbstractExpressionNode
            else
                using DynamicExpressions:
                    constructorof as $_constructorof,
                    AbstractExpressionNode as $_AbstractExpressionNode
            end

            function $($f)(
                l::N, r::N
            ) where {T<:$($type_requirements),N<:$_AbstractExpressionNode{T}}
                if (l.degree == 0 && l.constant && r.degree == 0 && r.constant)
                    $_constructorof(N)(T; val=$($f)(l.val::T, r.val::T))
                else
                    latest_op_idx = $($lookup_op)($($f), Val(2))
                    $_constructorof(N)(latest_op_idx, l, r)
                end
            end
            function $($f)(
                l::N, r::T
            ) where {T<:$($type_requirements),N<:$_AbstractExpressionNode{T}}
                if l.degree == 0 && l.constant
                    $_constructorof(N)(T; val=$($f)(l.val::T, r))
                else
                    latest_op_idx = $($lookup_op)($($f), Val(2))
                    $_constructorof(N)(latest_op_idx, l, $_constructorof(N)(T; val=r))
                end
            end
            function $($f)(
                l::T, r::N
            ) where {T<:$($type_requirements),N<:$_AbstractExpressionNode{T}}
                if r.degree == 0 && r.constant
                    $_constructorof(N)(T; val=$($f)(l, r.val::T))
                else
                    latest_op_idx = $($lookup_op)($($f), Val(2))
                    $_constructorof(N)(latest_op_idx, $_constructorof(N)(T; val=l), r)
                end
            end
            if $($build_converters)
                # Converters:
                function $($f)(l::$_AbstractExpressionNode, r::$_AbstractExpressionNode)
                    if l isa GraphNode || r isa GraphNode
                        error(
                            "Refusing to promote `GraphNode` as it would break the graph structure. " *
                            "Please convert to a common type first.",
                        )
                    end
                    return $($f)(promote(l, r)...)
                end
                function $($f)(
                    l::$_AbstractExpressionNode{T1}, r::T2
                ) where {T1<:$($type_requirements),T2<:$($type_requirements)}
                    return $($f)(l, convert(T1, r))
                end
                function $($f)(
                    l::T1, r::$_AbstractExpressionNode{T2}
                ) where {T1<:$($type_requirements),T2<:$($type_requirements)}
                    return $($f)(convert(T2, l), r)
                end
            end
        end
    end
end

function _extend_operators(operators, skip_user_operators, kws, __module__::Module)
    if !all(x -> first(x.args) ∈ (:empty_old_operators, :internal), kws)
        error(
            "You passed the keywords $(kws), but only `empty_old_operators`, `internal` are supported.",
        )
    end

    empty_old_operators_idx = findfirst(x -> first(x.args) == :empty_old_operators, kws)
    internal_idx = findfirst(x -> first(x.args) == :internal, kws)

    empty_old_operators = if empty_old_operators_idx !== nothing
        @assert kws[empty_old_operators_idx].head == :(=)
        kws[empty_old_operators_idx].args[2]
    else
        true
    end

    internal = if internal_idx !== nothing
        @assert kws[internal_idx].head == :(=)
        kws[internal_idx].args[2]::Bool
    else
        false
    end

    @gensym f skip type_requirements build_converters binary_exists unary_exists
    binary_ex = _extend_binary_operator(f, type_requirements, build_converters, internal)
    unary_ex = _extend_unary_operator(f, type_requirements, internal)
    return quote
        local $type_requirements
        local $build_converters
        local $binary_exists
        local $unary_exists
        if isa($operators, $OperatorEnum)
            $type_requirements = Number
            $build_converters = true
            $binary_exists = $(ALREADY_DEFINED_BINARY_OPERATORS).operator_enum
            $unary_exists = $(ALREADY_DEFINED_UNARY_OPERATORS).operator_enum
        else
            $type_requirements = Any
            $build_converters = false
            $binary_exists = $(ALREADY_DEFINED_BINARY_OPERATORS).generic_operator_enum
            $unary_exists = $(ALREADY_DEFINED_UNARY_OPERATORS).generic_operator_enum
        end
        if $(empty_old_operators)
            # Trigger errors if operators are not yet defined:
            empty!($(LATEST_BINARY_OPERATOR_MAPPING))
            empty!($(LATEST_UNARY_OPERATOR_MAPPING))
        end
        for (op, func) in enumerate($(operators).binops)
            local $f = Symbol(func)
            local $skip = false
            if isdefined(Base, $f)
                $f = :(Base.$($f))
            elseif $(skip_user_operators)
                $skip = true
            else
                $f = :($($__module__).$($f))
            end
            $(LATEST_BINARY_OPERATOR_MAPPING)[func] = op
            $skip && continue
            # Avoid redefining methods:
            if !haskey($unary_exists, func)
                eval($binary_ex)
                $(unary_exists)[func] = true
            end
        end
        for (op, func) in enumerate($(operators).unaops)
            local $f = Symbol(func)
            local $skip = false
            if isdefined(Base, $f)
                $f = :(Base.$($f))
            elseif $(skip_user_operators)
                $skip = true
            else
                $f = :($($__module__).$($f))
            end
            $(LATEST_UNARY_OPERATOR_MAPPING)[func] = op
            $skip && continue
            # Avoid redefining methods:
            if !haskey($binary_exists, func)
                eval($unary_ex)
                $(binary_exists)[func] = true
            end
        end
    end
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

"""
    OperatorEnum(; binary_operators=[], unary_operators=[],
                   define_helper_functions::Bool=true,
                   empty_old_operators::Bool=true)

Construct an `OperatorEnum` object, defining the possible expressions. This will also
redefine operators for `AbstractExpressionNode` types, as well as `show`, `print`, and
`(::AbstractExpressionNode)(X)`. It will automatically compute derivatives with `Zygote.jl`.

# Arguments
- `binary_operators::Vector{Function}`: A vector of functions, each of which is a binary
  operator.
- `unary_operators::Vector{Function}`: A vector of functions, each of which is a unary
  operator.
- `define_helper_functions::Bool=true`: Whether to define helper functions for creating
   and evaluating node types. Turn this off when doing precompilation. Note that these
   are *not* needed for the package to work; they are purely for convenience.
- `empty_old_operators::Bool=true`: Whether to clear the old operators.
"""
function OperatorEnum(;
    binary_operators=Function[],
    unary_operators=Function[],
    define_helper_functions::Bool=true,
    empty_old_operators::Bool=true,
    # Deprecated:
    enable_autodiff=nothing,
)
    @assert length(binary_operators) > 0 || length(unary_operators) > 0
    enable_autodiff !== nothing && Base.depwarn(
        "The option `enable_autodiff` has been deprecated. " *
        "Differential operators are now automatically computed within the gradient call.",
        :OperatorEnum,
    )

    operators = OperatorEnum(Tuple(binary_operators), Tuple(unary_operators))

    if define_helper_functions
        @extend_operators_base operators empty_old_operators = empty_old_operators
        set_default_operators!(operators)
    end

    return operators
end

"""
    GenericOperatorEnum(; binary_operators=[], unary_operators=[],
                          define_helper_functions::Bool=true, empty_old_operators::Bool=true)

Construct a `GenericOperatorEnum` object, defining possible expressions.
Unlike `OperatorEnum`, this enum one will work arbitrary operators and data types.
This will also redefine operators for `AbstractExpressionNode` types, as well as `show`, `print`,
and `(::AbstractExpressionNode)(X)`.

# Arguments
- `binary_operators::Vector{Function}`: A vector of functions, each of which is a binary
  operator.
- `unary_operators::Vector{Function}`: A vector of functions, each of which is a unary
  operator.
- `define_helper_functions::Bool=true`: Whether to define helper functions for creating
   and evaluating node types. Turn this off when doing precompilation. Note that these
   are *not* needed for the package to work; they are purely for convenience.
- `empty_old_operators::Bool=true`: Whether to clear the old operators.
"""
function GenericOperatorEnum(;
    binary_operators=Function[],
    unary_operators=Function[],
    define_helper_functions::Bool=true,
    empty_old_operators::Bool=true,
)
    @assert length(binary_operators) > 0 || length(unary_operators) > 0

    operators = GenericOperatorEnum(Tuple(binary_operators), Tuple(unary_operators))

    if define_helper_functions
        @extend_operators_base operators empty_old_operators = empty_old_operators
        set_default_operators!(operators)
    end

    return operators
end

# Predefine the most common operators so the errors
# are more informative
function _overload_common_operators()
    #! format: off
    operators = OperatorEnum(
        (+, -, *, /, ^, max, min, mod),
        (
            sin, cos, tan, exp, log, log1p, log2, log10, sqrt, cbrt, abs, sinh,
            cosh, tanh, atan, asinh, acosh, round, sign, floor, ceil,
        ),
    )
    #! format: on
    @extend_operators(operators, empty_old_operators = false, internal = true)
    empty!(LATEST_UNARY_OPERATOR_MAPPING)
    empty!(LATEST_BINARY_OPERATOR_MAPPING)
    return nothing
end
_overload_common_operators()

end
