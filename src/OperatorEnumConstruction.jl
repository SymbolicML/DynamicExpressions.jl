module OperatorEnumConstructionModule

import Zygote: gradient
import ..UtilsModule: max_ops
import ..OperatorEnumModule: AbstractOperatorEnum, OperatorEnum, GenericOperatorEnum
import ..EquationModule: string_tree, Node
import ..EvaluateEquationModule: eval_tree_array
import ..EvaluateEquationDerivativeModule: eval_grad_tree_array

function create_evaluation_helpers!(operators::OperatorEnum)
    @eval begin
        Base.print(io::IO, tree::Node) = print(io, string_tree(tree, $operators))
        Base.show(io::IO, tree::Node) = print(io, string_tree(tree, $operators))
        function (tree::Node)(X; kws...)
            out, did_finish = eval_tree_array(tree, X, $operators; kws...)
            if !did_finish
                out .= convert(eltype(out), NaN)
            end
            return out
        end
        # Gradients:
        function Base.adjoint(tree::Node{T}) where {T}
            return (X; kws...) -> begin
                _, grad, did_complete = eval_grad_tree_array(
                    tree, X, $operators; variable=true, kws...
                )
                !did_complete && (grad .= T(NaN))
                grad
            end
        end
    end
end

function create_evaluation_helpers!(operators::GenericOperatorEnum)
    @eval begin
        Base.print(io::IO, tree::Node) = print(io, string_tree(tree, $operators))
        Base.show(io::IO, tree::Node) = print(io, string_tree(tree, $operators))

        function (tree::Node)(X; kws...)
            out, did_finish = eval_tree_array(tree, X, $operators; kws...)
            if !did_finish
                return nothing
            end
            return out
        end
        function Base.adjoint(::Node{T}) where {T}
            return _ -> begin
                error("Gradients are not implemented for `GenericOperatorEnum`.")
            end
        end
    end
end

function _extend_unary_operator(f::Symbol, op, type_requirements)
    quote
        quote
            function $($f)(l::Node{T})::Node{T} where {T<:$($type_requirements)}
                return if (l.degree == 0 && l.constant)
                    Node(T; val=$($f)(l.val::T))
                else
                    Node($($op), l)
                end
            end
        end
    end
end

function _extend_binary_operator(f::Symbol, op, type_requirements, build_converters)
    quote
        quote
            function $($f)(l::Node{T}, r::Node{T}) where {T<:$($type_requirements)}
                if (l.degree == 0 && l.constant && r.degree == 0 && r.constant)
                    Node(T; val=$($f)(l.val::T, r.val::T))
                else
                    Node($($op), l, r)
                end
            end
            function $($f)(l::Node{T}, r::T) where {T<:$($type_requirements)}
                if l.degree == 0 && l.constant
                    Node(T; val=$($f)(l.val::T, r))
                else
                    Node($($op), l, Node(T; val=r))
                end
            end
            function $($f)(l::T, r::Node{T}) where {T<:$($type_requirements)}
                if r.degree == 0 && r.constant
                    Node(T; val=$($f)(l, r.val::T))
                else
                    Node($($op), Node(T; val=l), r)
                end
            end
            if $($build_converters)
                # Converters:
                function $($f)(
                    l::Node{T1}, r::Node{T2}
                ) where {T1<:$($type_requirements),T2<:$($type_requirements)}
                    T = promote_type(T1, T2)
                    l = convert(Node{T}, l)
                    r = convert(Node{T}, r)
                    return $($f)(l, r)
                end
                function $($f)(
                    l::Node{T1}, r::T2
                ) where {T1<:$($type_requirements),T2<:$($type_requirements)}
                    T = promote_type(T1, T2)
                    l = convert(Node{T}, l)
                    r = convert(T, r)
                    return $($f)(l, r)
                end
                function $($f)(
                    l::T1, r::Node{T2}
                ) where {T1<:$($type_requirements),T2<:$($type_requirements)}
                    T = promote_type(T1, T2)
                    l = convert(T, l)
                    r = convert(Node{T}, r)
                    return $($f)(l, r)
                end
            end
        end
    end
end

function _extend_operators(operators, skip_user_operators, __module__::Module)
    binary_ex = _extend_binary_operator(:f, :op, :type_requirements, :build_converters)
    unary_ex = _extend_unary_operator(:f, :op, :type_requirements)
    return quote
        local type_requirements
        local build_converters
        if isa($operators, OperatorEnum)
            type_requirements = Real
            build_converters = true
        else
            type_requirements = Any
            build_converters = false
        end
        for (op, f) in enumerate(map(Symbol, $(operators).binops))
            if isdefined(Base, f)
                f = :(Base.$(f))
            elseif $(skip_user_operators)
                continue
            else
                f = :($($__module__).$(f))
            end
            eval($binary_ex)
        end
        for (op, f) in enumerate(map(Symbol, $(operators).unaops))
            if isdefined(Base, f)
                f = :(Base.$(f))
            elseif $(skip_user_operators)
                continue
            else
                f = :($($__module__).$(f))
            end
            eval($unary_ex)
        end
    end
end

"""
    @extend_operators operators

Extends all operators defined in this operator enum to work on the
`Node` type. While by default this is already done for operators defined
in `Base` when you create an enum and pass `define_helper_functions=true`,
this does not apply to the user-defined operators. Thus, to do so, you must
apply this macro to the operator enum in the same module you have the operators
defined.
"""
macro extend_operators(operators)
    ex = _extend_operators(esc(operators), false, __module__)
    expected_type = AbstractOperatorEnum
    quote
        if !isa($(esc(operators)), $expected_type)
            error("You must pass an operator enum to `@extend_operators`.")
        end
        $ex
    end
end

"""
    @extend_operators_base operators

Similar to `@extend_operators`, but only extends operators already
defined in `Base`.
"""
macro extend_operators_base(operators)
    ex = _extend_operators(esc(operators), true, __module__)
    expected_type = AbstractOperatorEnum
    quote
        if !isa($(esc(operators)), $expected_type)
            error("You must pass an operator enum to `@extend_operators_base`.")
        end
        $ex
    end
end

"""
    OperatorEnum(; binary_operators=[], unary_operators=[], enable_autodiff::Bool=false, define_helper_functions::Bool=true)

Construct an `OperatorEnum` object, defining the possible expressions. This will also
redefine operators for `Node` types, as well as `show`, `print`, and `(::Node)(X)`.
It will automatically compute derivatives with `Zygote.jl`.

# Arguments
- `binary_operators::Vector{Function}`: A vector of functions, each of which is a binary
  operator.
- `unary_operators::Vector{Function}`: A vector of functions, each of which is a unary
  operator.
- `enable_autodiff::Bool=false`: Whether to enable automatic differentiation.
- `define_helper_functions::Bool=true`: Whether to define helper functions for creating
   and evaluating node types. Turn this off when doing precompilation. Note that these
   are *not* needed for the package to work; they are purely for convenience.
"""
function OperatorEnum(;
    binary_operators=[],
    unary_operators=[],
    enable_autodiff::Bool=false,
    define_helper_functions::Bool=true,
)
    @assert length(binary_operators) > 0 || length(unary_operators) > 0
    @assert length(binary_operators) <= max_ops && length(unary_operators) <= max_ops
    binary_operators = Tuple(binary_operators)
    unary_operators = Tuple(unary_operators)

    if enable_autodiff
        diff_binary_operators = Any[]
        diff_unary_operators = Any[]

        test_inputs = map(x -> convert(Float32, x), LinRange(-100, 100, 99))
        # Create grid over [-100, 100]^2:
        test_inputs_xy = reduce(
            hcat, reduce(hcat, ([[[x, y] for x in test_inputs] for y in test_inputs]))
        )
        for op in binary_operators
            diff_op(x, y) = gradient(op, x, y)

            test_output = diff_op.(test_inputs_xy[1, :], test_inputs_xy[2, :])
            gradient_exists = all((x) -> x !== nothing, Iterators.flatten(test_output))
            if gradient_exists
                push!(diff_binary_operators, diff_op)
            else
                if verbosity > 0
                    @warn "Automatic differentiation has been turned off, since operator $(op) does not have well-defined gradients."
                end
                enable_autodiff = false
                break
            end
        end

        for op in unary_operators
            diff_op(x) = gradient(op, x)[1]
            test_output = diff_op.(test_inputs)
            gradient_exists = all((x) -> x !== nothing, test_output)
            if gradient_exists
                push!(diff_unary_operators, diff_op)
            else
                if verbosity > 0
                    @warn "Automatic differentiation has been turned off, since operator $(op) does not have well-defined gradients."
                end
                enable_autodiff = false
                break
            end
        end
        diff_binary_operators = Tuple(diff_binary_operators)
        diff_unary_operators = Tuple(diff_unary_operators)
    end

    if !enable_autodiff
        diff_binary_operators = nothing
        diff_unary_operators = nothing
    end

    operators = OperatorEnum(
        binary_operators, unary_operators, diff_binary_operators, diff_unary_operators
    )

    if define_helper_functions
        @extend_operators_base operators
        create_evaluation_helpers!(operators)
    end

    return operators
end

"""
    GenericOperatorEnum(; binary_operators=[], unary_operators=[], define_helper_functions::Bool=true)

Construct a `GenericOperatorEnum` object, defining possible expressions.
Unlike `OperatorEnum`, this enum one will work arbitrary operators and data types.
This will also redefine operators for `Node` types, as well as `show`, `print`,
and `(::Node)(X)`.

# Arguments
- `binary_operators::Vector{Function}`: A vector of functions, each of which is a binary
  operator on real scalars.
- `unary_operators::Vector{Function}`: A vector of functions, each of which is a unary
  operator on real scalars.
- `define_helper_functions::Bool=true`: Whether to define helper functions for creating
   and evaluating node types. Turn this off when doing precompilation. Note that these
   are *not* needed for the package to work; they are purely for convenience.
"""
function GenericOperatorEnum(;
    binary_operators=[], unary_operators=[], define_helper_functions::Bool=true
)
    binary_operators = Tuple(binary_operators)
    unary_operators = Tuple(unary_operators)

    @assert length(binary_operators) > 0 || length(unary_operators) > 0
    @assert length(binary_operators) <= max_ops && length(unary_operators) <= max_ops

    operators = GenericOperatorEnum(binary_operators, unary_operators)

    if define_helper_functions
        @extend_operators_base operators
        create_evaluation_helpers!(operators)
    end

    return operators
end

end
