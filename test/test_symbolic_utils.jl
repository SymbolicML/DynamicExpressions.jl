using SymbolicUtils
using DynamicExpressions
using DynamicExpressions: get_operators, get_variable_names
using Test
include("test_params.jl")

# Test basic conversion with supported operators only
# (Custom operators are not supported in SymbolicUtils v4+)
operators = OperatorEnum(;
    default_params...,
    binary_operators=(+, *, -, /),
    unary_operators=(sin, cos, exp),
)

# Build tree: sin(3.0 * x1) + 2.0
x1_node = Node(; feature=1)
tree = Node(1, Node(; val=3.0) * x1_node) + Node(; val=2.0)

eqn = node_to_symbolic(tree, operators; variable_names=["energy"])
@test occursin("sin", string(eqn))
@test occursin("energy", string(eqn))
@test occursin("3", string(eqn))
@test occursin("2", string(eqn))

tree2 = symbolic_to_node(eqn, operators; variable_names=["energy"])
# SymbolicUtils v4 may reorder commutative operations, so compare by evaluation
X = [1.5;;]  # Test input
result1, _ = eval_tree_array(tree, X, operators)
result2, _ = eval_tree_array(tree2, X, operators)
@test isapprox(result1, result2)

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
    @test ex2 == ex2_again
end
