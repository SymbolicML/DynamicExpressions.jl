using DynamicExpressions
using DynamicExpressions: EvalOptions
using DispatchDoctor: allow_unstable
using Test

# Before defining OperatorEnum, calling the implicit (deprecated)
# syntax should fail:
tree = Node{Float64}(; feature=1)

if VERSION >= v"1.8"
    @test_throws ErrorException allow_unstable(() -> tree([1.0; 2.0;;]))
    @test_throws "Please use the " allow_unstable(() -> tree([1.0; 2.0;;]))
    @test_throws ErrorException allow_unstable(() -> tree'([1.0; 2.0;;]))
    @test_throws "Please use the " allow_unstable(() -> tree'([1.0; 2.0;;]))
end

# Initial strings are still somewhat useful
@test string(tree) == "x1"
@test string(Node(1, tree)) == "unary_operator[1](x1)"
@test string(Node(1, tree, tree)) == "binary_operator[1](x1, x1)"

# Before loading extensions, should fail with helpful message:
operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[cos, sin])
x1, x2 = Node{Float64}(; feature=1), Node{Float64}(; feature=2)
tree = cos(2.1 * x1) + sin(x2)

if VERSION >= v"1.9"
    @test_throws(
        "Please load the `SymbolicUtils` package to use `node_to_symbolic`.",
        node_to_symbolic(tree, operators)
    )
    @test_throws(
        "Please load the `SymbolicUtils` package to use `symbolic_to_node`.",
        symbolic_to_node(tree, operators)
    )

    @test_throws(
        "Please load the Zygote.jl package.", allow_unstable(() -> tree'(ones(2, 10)))
    )

    @test_throws(
        "Please load the Bumper.jl package",
        allow_unstable(
            () -> tree(ones(2, 10), operators; eval_options=EvalOptions(; bumper=Val(true)))
        )
    )

    @test_throws(
        "Please load the LoopVectorization.jl package",
        allow_unstable(
            () -> tree(ones(2, 10), operators; eval_options=EvalOptions(; turbo=Val(true)))
        )
    )
end
