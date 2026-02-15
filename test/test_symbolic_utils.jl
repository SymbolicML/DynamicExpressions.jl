using SymbolicUtils
using DynamicExpressions
using DynamicExpressions: get_operators, get_variable_names
using Test
include("test_params.jl")

# Test basic conversion with supported operators only
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
    @test occursin("x", string(eqn))
    @test occursin("y", string(eqn))
    @test occursin("sin", string(eqn))

    # Test with different variable names in the expression.
    # Use a non-symmetric expression so we can detect any variable swapping,
    # while still allowing commutative re-ordering inside SymbolicUtils.
    ex2 = parse_expression(
        :(sin(alpha + 2 * beta));
        binary_operators=[+, *, -, /],
        unary_operators=[sin],
        variable_names=["alpha", "beta"],
    )
    eqn2 = convert(SymbolicUtils.BasicSymbolic, ex2)
    @test occursin("alpha", string(eqn2))
    @test occursin("beta", string(eqn2))
    @test occursin("2", string(eqn2))

    # Test round trip preserves semantics and variable names.
    # SymbolicUtils v4 may reorder commutative operations, so don't require exact `==`.
    operators = OperatorEnum(; unary_operators=(sin,), binary_operators=(+, *, -, /))
    ex2_again = convert(Expression, eqn2, operators; variable_names=["alpha", "beta"])

    X = rand(Float64, 2, 10) .+ 1
    y1, _ = eval_tree_array(ex2, X)
    y2, _ = eval_tree_array(ex2_again, X)
    @test y1 ≈ y2
end

# Test custom operator round-trip via index_functions
let
    myop(x, y) = x + 2y
    operators = OperatorEnum(; binary_operators=(+, *, -, /, myop), unary_operators=(sin,))

    x1 = Node(; feature=1)
    x2 = Node(; feature=2)
    tree = Node(; op=5, l=x1, r=x2) # myop(x1, x2)

    eqn = node_to_symbolic(tree, operators; index_functions=true)
    tree2 = symbolic_to_node(eqn, operators)

    X = rand(Float64, 2, 50) .+ 1
    y1, ok1 = eval_tree_array(tree, X, operators)
    y2, ok2 = eval_tree_array(tree2, X, operators)
    @test ok1 && ok2
    @test y1 ≈ y2
end
