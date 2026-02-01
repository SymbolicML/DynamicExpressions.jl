using SymbolicUtils
using DynamicExpressions
using DynamicExpressions: get_operators, get_variable_names
using Test
include("test_params.jl")

custom_unary_test(x) = x + 1
custom_binary_test(x, y) = x - 2y

# Test basic conversion with supported operators only
# (Custom operators require `index_functions=true` for round-tripping)
operators = OperatorEnum(;
    default_params..., binary_operators=(+, *, -, /), unary_operators=(sin, cos, exp)
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

    # SymbolicUtils may reorder commutative operations (e.g., alpha + beta vs beta + alpha),
    # so compare via simplified SymbolicUtils expressions.
    eqn2_again = convert(SymbolicUtils.BasicSymbolic, ex2_again)
    @test string(SymbolicUtils.simplify(eqn2)) == string(SymbolicUtils.simplify(eqn2_again))
end

# Test `index_functions=true` supports round-tripping custom operators
let
    operators_custom = OperatorEnum(;
        binary_operators=(custom_binary_test,), unary_operators=(custom_unary_test,)
    )

    x1 = Node(; feature=1)
    # Build a tree that *uses the custom operators* without relying on helper-method injection.
    tree = Node(1, Node(1, x1), Node(; val=2.0))

    @test_throws ErrorException node_to_symbolic(tree, operators_custom)

    eqn = node_to_symbolic(tree, operators_custom; index_functions=true)
    tree2 = symbolic_to_node(eqn, operators_custom)

    X = [1.5;;]
    result1, _ = eval_tree_array(tree, X, operators_custom)
    result2, _ = eval_tree_array(tree2, X, operators_custom)
    @test isapprox(result1, result2)
end
