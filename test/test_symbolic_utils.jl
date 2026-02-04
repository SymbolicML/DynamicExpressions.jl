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
    # SymbolicUtils may reorder commutative operations (e.g. alpha + beta vs beta + alpha),
    # so exact tree equality is not a stable invariant. Instead, compare by evaluation.
    X = Float32[
        -2 -1 0 1 2
        0.1 0.2 0.3 0.4 0.5
    ]
    y1, ok1 = DynamicExpressions.eval_tree_array(
        DynamicExpressions.get_tree(ex2), X, operators
    )
    y2, ok2 = DynamicExpressions.eval_tree_array(
        DynamicExpressions.get_tree(ex2_again), X, operators
    )
    @test ok1 && ok2
    @test isapprox(y1, y2; rtol=0, atol=1.0f-6)
end

@testset "SymbolicUtils extension edge cases" begin
    @syms x y z

    operators_no_pow = OperatorEnum(;
        unary_operators=(sin, exp), binary_operators=(+, *, /)
    )
    operators_with_pow = OperatorEnum(;
        unary_operators=(sin, exp), binary_operators=(+, *, /, ^)
    )

    function eval_expr(expr, operators, variable_names, X)
        node = if expr isa DynamicExpressions.AbstractExpressionNode
            expr
        else
            convert(Node, expr, operators; variable_names)
        end
        yval, ok = DynamicExpressions.eval_tree_array(node, X, operators)
        @test ok
        return yval
    end

    X1 = reshape([2.0], 1, 1)
    X2 = reshape([2.0, 3.0], 2, 1)
    X3 = reshape([2.0, 3.0, 4.0], 3, 1)

    expr_pow1 = SymbolicUtils.term(^, x, 1)
    @test eval_expr(expr_pow1, operators_no_pow, ["x"], X1) == [2.0]

    expr_pow0 = SymbolicUtils.term(^, x, 0)
    @test eval_expr(expr_pow0, operators_no_pow, ["x"], X1) == [1.0]

    expr_pow_neg1 = SymbolicUtils.term(^, x, -1)
    @test isapprox(
        eval_expr(expr_pow_neg1, operators_no_pow, ["x"], X1)[1],
        0.5;
        rtol=0,
        atol=1.0e-12,
    )

    expr_pow3 = SymbolicUtils.term(^, x, 3)
    @test eval_expr(expr_pow3, operators_no_pow, ["x"], X1) == [8.0]

    expr_pow_neg3 = SymbolicUtils.term(^, x, -3)
    @test isapprox(
        eval_expr(expr_pow_neg3, operators_no_pow, ["x"], X1)[1],
        0.125;
        rtol=0,
        atol=1.0e-12,
    )

    expr_unary = SymbolicUtils.term(sin, SymbolicUtils.term(^, x, 2))
    @test isapprox(
        eval_expr(expr_unary, operators_no_pow, ["x"], X1)[1],
        sin(4.0);
        rtol=0,
        atol=1.0e-12,
    )

    expr_mul_binary = SymbolicUtils.term(*, SymbolicUtils.term(^, x, 2), y)
    @test eval_expr(expr_mul_binary, operators_no_pow, ["x", "y"], X2) == [12.0]

    expr_mul_nary = SymbolicUtils.term(*, x, y, z)
    @test eval_expr(expr_mul_nary, operators_no_pow, ["x", "y", "z"], X3) == [24.0]

    expr_pow_sym = SymbolicUtils.term(^, x, y)
    @test eval_expr(expr_pow_sym, operators_with_pow, ["x", "y"], X2) == [8.0]

    # Cover the `findoperation` failure branch: n-ary + with an OperatorEnum lacking +
    operators_missing_plus = OperatorEnum(; unary_operators=(), binary_operators=(*, /))
    expr_nary_plus = SymbolicUtils.term(+, x, y, z)
    @test_throws ErrorException convert(
        Node, expr_nary_plus, operators_missing_plus; variable_names=["x", "y", "z"]
    )

    myop(x) = x + 1
    operators_custom = OperatorEnum(; unary_operators=(myop,), binary_operators=(+, *, /))
    expr_custom = parse_expression(
        :(myop(x)); unary_operators=[myop], binary_operators=[+, *, /], variable_names=["x"]
    )
    @test_throws ErrorException node_to_symbolic(
        expr_custom, operators_custom; index_functions=false
    )
    eqn_custom = node_to_symbolic(expr_custom, operators_custom; index_functions=true)
    expr_custom_rt = symbolic_to_node(eqn_custom, operators_custom; variable_names=["x"])
    @test eval_expr(expr_custom_rt, operators_custom, ["x"], X1) == [3.0]
end
