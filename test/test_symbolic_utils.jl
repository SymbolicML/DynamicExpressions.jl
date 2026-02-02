using SymbolicUtils
using DynamicExpressions
using DynamicExpressions: get_operators, get_variable_names
using Test
include("test_params.jl")

custom_binary_test(x, y) = x - 2y

# Test conversion round-trip with custom operators using `index_functions=true`.
operators = OperatorEnum(;
    default_params..., binary_operators=(custom_binary_test,), unary_operators=(custom_cos,)
)

x1_node = Node(; feature=1)
tree = Node(1, Node(1, x1_node), Node(; val=2.0))

@test_throws ErrorException node_to_symbolic(tree, operators; variable_names=["energy"])

eqn = node_to_symbolic(tree, operators; variable_names=["energy"], index_functions=true)
@test occursin("energy", string(eqn))
@test occursin("custom_cos", string(eqn))

tree2 = symbolic_to_node(eqn, operators; variable_names=["energy"])
X = [1.5;;]  # Test input
result1, _ = eval_tree_array(tree, X, operators)
result2, _ = eval_tree_array(tree2, X, operators)
@test isapprox(result1, result2)

# Test default variable naming (x1, x2, ...) round-trips without providing variable_names
let
    operators = OperatorEnum(; unary_operators=(sin,), binary_operators=(+, *, -, /))
    tree = Node(; feature=1)
    eqn = convert(SymbolicUtils.BasicSymbolic, tree, operators)
    tree2 = convert(Node, eqn, operators)
    @test tree2.degree == 0 && !tree2.constant && tree2.feature == 1
end

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

# Cover SymbolicUtils v4 conversion edge-cases:
let
    operators = OperatorEnum(; unary_operators=(sin,), binary_operators=(+, *, -, /, ^))

    # `convert_to_function(::BasicSymbolic, ...)` should no-op on non-function symbols.
    x1 = SymbolicUtils.Sym{SymbolicUtils.SymReal}(:x1; type=Number)

    # Access the extension module in a way that works under SafeTestsets (isolated test modules).
    ext = Base.get_extension(DynamicExpressions, :DynamicExpressionsSymbolicUtilsExt)
    @test ext !== nothing
    @test ext.convert_to_function(x1, operators) === x1

    # A 3-argument function symbol should throw (only unary/binary supported).
    ftype3 = Base.unwrap_unionall(SymbolicUtils.FnType{Tuple{Number,Number,Number},Number})
    f3 = SymbolicUtils.Sym{SymbolicUtils.SymReal}(:f3; type=ftype3)
    x2 = SymbolicUtils.Sym{SymbolicUtils.SymReal}(:x2; type=Number)
    x3 = SymbolicUtils.Sym{SymbolicUtils.SymReal}(:x3; type=Number)
    @test_throws AssertionError ext.convert_to_function(f3, operators)
    @test_throws Exception convert(Node, f3(x1, x2, x3), operators)

    # `multiply_powers`: cover negative integer powers (SymbolicUtils v4 Const wrapping).
    expr_powm2 = SymbolicUtils.term(^, x1, -2)
    tree_powm2 = convert(Node, expr_powm2, operators)

    X = [1.5;;]
    rm2, _ = eval_tree_array(tree_powm2, X, operators)
    @test only(rm2) ≈ 1.5^-2
end
