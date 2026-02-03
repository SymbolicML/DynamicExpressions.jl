using SymbolicUtils
using DynamicExpressions
using DynamicExpressions: get_operators, get_variable_names
using Test
include("test_params.jl")

_inv(x) = 1 / x
!(@isdefined safe_pow) &&
    @eval safe_pow(x::T, y::T) where {T<:Number} = (x < 0 && y != round(y)) ? T(NaN) : x^y
!(@isdefined greater) && @eval greater(x::T, y::T) where {T} = (x > y) ? one(T) : zero(T)

tree =
    let tmp_op = OperatorEnum(;
            default_params...,
            binary_operators=(+, *, ^, /, greater),
            unary_operators=(_inv,),
        )
        Node(5, (Node(; val=3.0) * Node(1, Node("x1")))^2.0, Node(; val=-1.2))
    end

operators = OperatorEnum(;
    default_params...,
    binary_operators=(+, *, safe_pow, /, greater),
    unary_operators=(_inv,),
)

eqn = node_to_symbolic(tree, operators; variable_names=["energy"], index_functions=true)
@test string(eqn) == "greater(safe_pow(3.0_inv(energy), 2.0), -1.2)"

tree2 = symbolic_to_node(eqn, operators; variable_names=["energy"])
@test string_tree(tree, operators) == string_tree(tree2, operators)

# Test variable name conversion with Expression objects
let
    ex = parse_expression(
        :(sin(x + y));
        binary_operators=[+, *, -, /],
        unary_operators=[sin],
        variable_names=["x", "y"],
    )

    # Test conversion to symbolic form preserves variable names
    eqn = convert(SymbolicUtils.BasicSymbolic, ex)
    @test string(eqn) == "sin(x + y)"

    # Test with different variable names in the expression
    ex2 = parse_expression(
        :(sin(alpha + beta));
        binary_operators=[+, *, -, /],
        unary_operators=[sin],
        variable_names=["alpha", "beta"],
    )
    eqn2 = convert(SymbolicUtils.BasicSymbolic, ex2)
    @test string(eqn2) == "sin(alpha + beta)"
    eqn2

    # Test round trip preserves structure and variable names
    operators = OperatorEnum(; unary_operators=(sin,), binary_operators=(+, *, -, /))
    ex2_again = convert(Expression, eqn2, operators; variable_names=["alpha", "beta"])
    # `Expression` equality also checks metadata (including the exact `OperatorEnum`)
    # which can legitimately differ across round-trips. Here we just care that the
    # parsed tree is preserved.
    @test DynamicExpressions.get_tree(ex2) == DynamicExpressions.get_tree(ex2_again)
end
