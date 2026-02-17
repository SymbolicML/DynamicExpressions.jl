using SymbolicUtils
using StaticArrays
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

tree2 = symbolic_to_node(eqn, operators; variable_names=["energy"])
# SymbolicUtils v4 may reorder commutative operations, so compare by evaluation
X = reshape([1.5, -0.2, 2.0], 1, :)
expected = sin.(3.0 .* X[1, :]) .+ 2.0

result1, ok1 = eval_tree_array(tree, X, operators)
result2, ok2 = eval_tree_array(tree2, X, operators)
@test ok1 && ok2
@test isapprox(result1, result2)
@test isapprox(result2, expected; rtol=0, atol=1.0e-12)

# Test variable name conversion with Expression objects
let
    ex = parse_expression(
        :(sin(x + y));
        binary_operators=[+, *, -, /],
        unary_operators=[sin],
        variable_names=["x", "y"],
    )

    # Test conversion to symbolic form round-trips by evaluation.
    eqn = convert(SymbolicUtils.BasicSymbolic, ex)
    operators_roundtrip = OperatorEnum(;
        unary_operators=(sin,), binary_operators=(+, *, -, /)
    )
    ex_again = convert(Expression, eqn, operators_roundtrip; variable_names=["x", "y"])

    X = rand(Float64, 2, 10) .+ 1
    y1, ok1 = eval_tree_array(ex, X)
    y2, ok2 = eval_tree_array(ex_again, X)
    @test ok1 && ok2
    @test y1 ≈ y2

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

    # Test round trip preserves semantics and variable names.
    # SymbolicUtils v4 may reorder commutative operations, so don't require exact `==`.
    operators = OperatorEnum(; unary_operators=(sin,), binary_operators=(+, *, -, /))
    ex2_again = convert(Expression, eqn2, operators; variable_names=["alpha", "beta"])

    X = rand(Float64, 2, 10) .+ 1
    y1, _ = eval_tree_array(ex2, X)
    y2, _ = eval_tree_array(ex2_again, X)
    @test y1 ≈ y2
end

# Const scalar-container unwrapping (SymbolicUtils v4 Const can wrap scalar containers)
let
    operators = OperatorEnum(; default_params..., binary_operators=(+, *, -, /), unary_operators=())
    expr = SymbolicUtils.BasicSymbolicImpl.Const{SymbolicUtils.SymReal}(SVector(1.0))

    node = convert(Node, expr, operators)

    X = rand(Float64, 1, 10) .+ 1
    y, ok = eval_tree_array(node, X, operators)
    @test ok
    @test all(y .== 1.0f0)
end

# Operators excluding `^`: preserve semantics for `x*x` and integer powers
let
    operators = OperatorEnum(; default_params..., binary_operators=(+, *, -, /), unary_operators=())

    ex = parse_expression(
        :(x * x);
        binary_operators=[+, *, -, /],
        unary_operators=Function[],
        variable_names=["x"],
    )
    eqn = convert(SymbolicUtils.BasicSymbolic, ex)
    ex_again = convert(Expression, eqn, operators; variable_names=["x"])

    X = rand(Float64, 1, 50) .+ 0.1
    y1, ok1 = eval_tree_array(ex, X)
    y2, ok2 = eval_tree_array(ex_again, X)
    @test ok1 && ok2
    @test y1 ≈ y2

    x = SymbolicUtils.Sym{SymbolicUtils.SymReal}(:x1; type=Number)
    for n in (0, 1, 2)
        expr = SymbolicUtils.term(^, x, n)
        node = convert(Node, expr, operators)

        # SU -> Node -> SU -> Node round-trip should not require `^` in the operator set
        eqn_rt = node_to_symbolic(node, operators)
        node_rt = if eqn_rt isa Number
            convert(Node, eqn_rt, operators)
        else
            symbolic_to_node(eqn_rt, operators)
        end

        X = rand(Float64, 1, 40) .+ 0.1
        y, ok = eval_tree_array(node_rt, X, operators)
        @test ok
        @test y ≈ Float32.(X[1, :] .^ n)
    end

    x2 = SymbolicUtils.Sym{SymbolicUtils.SymReal}(:x2; type=Number)
    x3 = SymbolicUtils.Sym{SymbolicUtils.SymReal}(:x3; type=Number)
    expr = SymbolicUtils.term(*, x, x2, x3)
    node = convert(Node, expr, operators)

    X = rand(Float64, 3, 40) .+ 0.1
    y, ok = eval_tree_array(node, X, operators)
    @test ok
    @test y ≈ Float32.(X[1, :] .* X[2, :] .* X[3, :])
end

# Test custom operator round-trip via index_functions
let
    myop(x, y) = x + 2y
    operators = OperatorEnum(; binary_operators=(+, *, -, /, myop), unary_operators=(sin,))

    x1 = Node(; feature=1)
    x2 = Node(; feature=2)
    tree = Node(; op=5, l=x1, r=x2) # myop(x1, x2)

    @test_throws ErrorException node_to_symbolic(tree, operators; index_functions=false)

    eqn = node_to_symbolic(tree, operators; index_functions=true)
    tree2 = symbolic_to_node(eqn, operators)

    X = rand(Float64, 2, 50) .+ 1
    y1, ok1 = eval_tree_array(tree, X, operators)
    y2, ok2 = eval_tree_array(tree2, X, operators)
    @test ok1 && ok2
    @test y1 ≈ y2
end
