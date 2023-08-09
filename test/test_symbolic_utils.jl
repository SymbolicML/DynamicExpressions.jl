using SymbolicUtils
using DynamicExpressions
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
