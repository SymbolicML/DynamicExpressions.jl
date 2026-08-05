using DynamicExpressions
using DynamicExpressions: EvalContext
using DynamicExpressions.ExtensionInterfaceModule:
    _is_extension_loaded, _zygote_gradient, bumper_eval_tree_array
using DispatchDoctor: allow_unstable
using Test

# Before defining OperatorEnum, calling the implicit (deprecated)
# syntax should fail:
tree = Node{Float64}(; feature=1)

@test_throws ErrorException allow_unstable(() -> tree([1.0; 2.0;;]))
@test_throws "Please use the " allow_unstable(() -> tree([1.0; 2.0;;]))
@test_throws ErrorException allow_unstable(() -> tree'([1.0; 2.0;;]))
@test_throws "Please use the " allow_unstable(() -> tree'([1.0; 2.0;;]))

# Initial strings are still somewhat useful
@test string(tree) == "x1"
@test string(Node(1, tree)) == "unary_operator[1](x1)"
@test string(Node(1, tree, tree)) == "binary_operator[1](x1, x1)"

# Before loading extensions, should fail with helpful message:
operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[cos, sin])
x1, x2 = Node{Float64}(; feature=1), Node{Float64}(; feature=2)
tree = cos(2.1 * x1) + sin(x2)

@test_throws(
    "Please load the `SymbolicUtils` package to use `node_to_symbolic`.",
    node_to_symbolic(tree, operators)
)
@test_throws(
    "Please load the `SymbolicUtils` package to use `symbolic_to_node`.",
    symbolic_to_node(tree, operators)
)

@test_throws("Please load the Zygote.jl package.", allow_unstable(() -> tree'(ones(2, 10))))

@test_throws(
    "Please load the Bumper.jl package",
    allow_unstable(
        () -> tree(ones(2, 10), operators; eval_context=EvalContext(; bumper=Val(true)))
    )
)

@test_throws(
    "Please load the LoopVectorization.jl package",
    allow_unstable(
        () -> tree(ones(2, 10), operators; eval_context=EvalContext(; turbo=Val(true)))
    )
)

# Loaded extensions should use normal dispatch for unsupported arguments instead of claiming
# that the dependency is missing.
using SymbolicUtils

@test _is_extension_loaded(Val(:SymbolicUtils))
@test !_is_extension_loaded(Val(:Zygote))
@test !_is_extension_loaded(Val(:Bumper))
@test_throws "Please load the Zygote.jl package." _zygote_gradient(nothing)
@test_throws "Please load the Bumper.jl package" bumper_eval_tree_array(nothing)

symbolic_x1 = node_to_symbolic(x1, operators)
@test string(symbolic_x1) == "x1"
@test string(symbolic_to_node(symbolic_x1, operators)) == "x1"
@test_throws MethodError node_to_symbolic(nothing)
@test_throws MethodError symbolic_to_node(nothing)

using Zygote

@test _is_extension_loaded(Val(:SymbolicUtils))
@test _is_extension_loaded(Val(:Zygote))
@test !_is_extension_loaded(Val(:Bumper))
@test_throws "Please load the Bumper.jl package" bumper_eval_tree_array(nothing)

@test only(_zygote_gradient(sin, Val(1))(1.0)) ≈ cos(1.0)
@test_throws MethodError _zygote_gradient(nothing)

using Bumper

@test _is_extension_loaded(Val(:SymbolicUtils))
@test _is_extension_loaded(Val(:Zygote))
@test _is_extension_loaded(Val(:Bumper))

bumper_result, bumper_ok =
    tree(ones(2, 10), operators; eval_options=EvalContext(; bumper=Val(true)))
@test bumper_ok
@test bumper_result ≈ fill(cos(2.1) + sin(1.0), 10)
@test_throws MethodError bumper_eval_tree_array(nothing)
