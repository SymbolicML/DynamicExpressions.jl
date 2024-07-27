@testitem "Expression Initialization" begin
    using DynamicExpressions
    using DynamicExpressions: get_tree, get_operators, get_variable_names

    tree = Node(Float64; feature=1)
    operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])
    variable_names = ["x"]

    expr = Expression(tree; operators, variable_names)

    @test get_tree(expr) === tree
    @test get_operators(expr, nothing) === operators
    @test get_variable_names(expr, nothing) === variable_names

    copy_operators = OperatorEnum(; binary_operators=[+])
    copy_variable_names = ["y"]

    @test get_operators(expr, copy_operators) === copy_operators
    @test get_variable_names(expr, copy_variable_names) === copy_variable_names

    @inferred copy_node(expr)
    @test copy(expr) == expr
    @test hash(copy(expr)) == hash(expr)

    expr2 = Expression(Node(; op=1, l=tree); operators, variable_names)
    @test copy_node(expr2) != expr
    @test hash(copy(expr2)) != hash(expr)

    @test propertynames(expr.metadata) == (:operators, :variable_names)

    @test count_nodes(expr2) == 2

    @test tree_mapreduce(_ -> 2, +, expr2) == 4
    @test tree_mapreduce(_ -> 2, _ -> 3, +, expr2) == 5
    @test count_depth(expr2) == 2
end

@testitem "Expression interface" begin
    using DynamicExpressions
    using DynamicExpressions: ExpressionInterface
    using Interfaces: test
    tree = Node(Float64; feature=1)
    operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])
    variable_names = ["x"]
    expr = Expression(tree; operators, variable_names)
    @test test(ExpressionInterface, Expression, [expr])
end

@testitem "Expression evaluation" begin
    using DynamicExpressions
    using Zygote

    ex = @parse_expression(
        sin(2.0 * x1 + exp(x2 + 5.0)),
        operators = OperatorEnum(;
            binary_operators=[+, -, *, /], unary_operators=[sin, cos, exp]
        ),
        variable_names = [:x1, :x2],
    )

    X = rand(Float64, 2, 10) .+ 1
    expected = @. sin(2.0 * X[1, :] + exp(X[2, :] + 5.0))
    expected_dy_dx1 = @. 2.0 * cos(2.0 * X[1, :] + exp(X[2, :] + 5.0))

    if VERSION >= v"1.9"
        @test_nowarn begin
            result = ex(X)
            @test result ≈ expected
            result, _ = eval_tree_array(ex, X)
            @test result ≈ expected
            result_grad = ex'(X)
            @test result_grad[1, :] ≈ expected_dy_dx1
            _, result_grad, _ = eval_grad_tree_array(ex, X; variable=Val(true))
            @test result_grad[1, :] ≈ expected_dy_dx1
        end
    end
end

@testitem "Can also get derivatives of expression itself" begin
    using DynamicExpressions
    using Zygote: Zygote
    using DifferentiationInterface: AutoZygote, gradient

    ex = @parse_expression(x1 + 1.5, binary_operators = [+], variable_names = ["x1"])
    d_ex = gradient(AutoZygote(), ex) do ex
        sum(ex(ones(1, 5)))
    end
    @test d_ex isa NamedTuple
    @test extract_gradient(d_ex, ex) ≈ [5.0]
end

@testitem "Expression simplification" begin
    using DynamicExpressions

    ex = @parse_expression(
        sin(2.0 + 1.0 + c),
        operators = OperatorEnum(;
            binary_operators=[+, -, *, /], unary_operators=[sin, cos, exp]
        ),
        variable_names = [:c],
    )
    out = simplify_tree!(ex)
    @test typeof(out) === typeof(ex)
    @test string_tree(ex) == "sin(3.0 + c)"

    # ((const + var) + const) => (const + var)
    ex = @parse_expression(
        (2.0 + x) + 3.0,
        operators = OperatorEnum(; binary_operators=[+, -]),
        variable_names = [:x],
    )
    out = combine_operators(ex)
    @test typeof(out) === typeof(ex)
    @test string_tree(out) == "x + 5.0"
end

@testitem "Nested repeat operators in Expression parsing" begin
    using DynamicExpressions

    ex = @parse_expression(
        a + b + c + a + b + c,
        variable_names = [:a, :b, :c],
        operators = OperatorEnum(; binary_operators=[+])
    )
    @test sprint((io, ex) -> show(io, MIME"text/plain"(), ex), ex) ==
        "((((a + b) + c) + a) + b) + c"
end

@testitem "Expression utilities" begin
    using DynamicExpressions

    operators = OperatorEnum(;
        binary_operators=[+, -, *, /], unary_operators=[sin, cos, exp]
    )
    variable_names = [:a, :b, :c]
    ex = @parse_expression(
        cos(a * 1.5 - 0.3) * b + exp(0.5 - c * c),
        variable_names = variable_names,
        operators = operators
    )
    @test string(ex) == string(copy(ex))
    @test ex !== copy(ex)
    @test hash(ex) == copy(hash(ex))

    t = ex.tree
    modified_ex = @parse_expression(
        $t + 1.5, variable_names = variable_names, operators = operators
    )
    s1 = sprint((io, ex) -> show(io, MIME"text/plain"(), ex), ex)
    s2 = sprint((io, ex) -> show(io, MIME"text/plain"(), ex), modified_ex)
    @test s2 == "($s1) + 1.5"
    @test hash(ex) != hash(modified_ex)

    variable_names = []
    ex = @parse_expression(
        1.5 + 2.5, operators = OperatorEnum(; binary_operators=[+]), variable_names
    )
    @test has_operators(ex) == true
    ex = @parse_expression(
        1.5, operators = OperatorEnum(; binary_operators=[+]), variable_names
    )
    @test has_operators(ex) == false
    @test count_constants(ex) == 1
    node_index = index_constant_nodes(ex)
    @test node_index isa NodeIndex
    @test node_index.val == 1
    ex = @parse_expression(
        1.5 + 2.5, operators = OperatorEnum(; binary_operators=[+]), variable_names
    )
    node_index = index_constant_nodes(ex)
    @test node_index.l.val == 1
    @test node_index.r.val == 2
    @test get_scalar_constants(ex)[1] == [1.5, 2.5]
    set_scalar_constants!(ex, [3.5, 4.5], get_scalar_constants(ex)[2])
    @test get_scalar_constants(ex)[1] == [3.5, 4.5]
    @test count_constants(ex) == 2
    @test has_constants(ex) == true
    ex = @parse_expression(
        a, operators = OperatorEnum(; binary_operators=[+]), variable_names = [:a]
    )
    @test has_constants(ex) == false
end

@testitem "Expression with_contents" begin
    using DynamicExpressions

    ex = @parse_expression(x1 + 1.5, binary_operators = [+, *], variable_names = ["x1"])
    ex2 = @parse_expression(x1 + 3.0, binary_operators = [+], variable_names = ["x1"])

    t2 = DynamicExpressions.get_contents(ex2)
    ex_modified = DynamicExpressions.with_contents(ex, t2)
    @test DynamicExpressions.get_tree(ex_modified) == t2
end

@testitem "Expression `preserve_sharing`" begin
    using DynamicExpressions

    ex = @parse_expression(x1 + 1.5, binary_operators = [+, *], variable_names = ["x1"])
    ex_graph = @parse_expression(
        x1 + 1.5, binary_operators = [+, *], variable_names = ["x1"], node_type = GraphNode
    )
    @test !DynamicExpressions.preserve_sharing(ex)
    @test DynamicExpressions.preserve_sharing(ex_graph)
end

@testitem "Expression edge cases" begin
    using DynamicExpressions

    operators = OperatorEnum(;
        binary_operators=[+, -, *, /], unary_operators=[sin, cos, exp]
    )
    variable_names = [:a, :b, :c]
    ex = @parse_expression(
        cos(a * 1.5 - 0.3) * b + exp(0.5 - c * c),
        variable_names = variable_names,
        operators = operators
    )
    @test_throws ArgumentError @parse_expression(
        $ex + 1.5, variable_names = variable_names, operators = operators
    )
    if VERSION >= v"1.9"
        @test_throws "Cannot parse an expression as a value in another expression. " @parse_expression(
            $ex + 1.5, variable_names = variable_names, operators = operators
        )
    end
    @eval struct Foo
        x::$(typeof(ex.tree))
    end
    foo = Foo(ex.tree)
    @test_throws ArgumentError @parse_expression(
        $(foo).x + 1.5, variable_names = variable_names, operators = operators
    )
    if VERSION >= v"1.9"
        @test_throws "Unrecognized expression type" @parse_expression(
            $(foo).x + 1.5, variable_names = variable_names, operators = operators
        )
    end
end

@testitem "No operators and variable names" begin
    using DynamicExpressions

    x1 = Node{Float64}(; feature=1)
    expr = Expression(x1; operators=nothing, variable_names=nothing)

    @test sprint(show, expr) == "x1"
    @test copy(expr) == expr
    @test hash(expr) == hash(copy(expr))

    @test sprint(show, get_metadata(expr)) ==
        "Metadata((operators = nothing, variable_names = nothing))"

    cos_x1 = Node{Float64}(; op=1, l=x1)
    expr = Expression(cos_x1; operators=nothing, variable_names=nothing)
    @test sprint(show, expr) == "unary_operator[1](x1)"
    @test copy(expr) == expr
    @test hash(expr) == hash(copy(expr))

    @test sprint(show, get_metadata(expr)) ==
        "Metadata((operators = nothing, variable_names = nothing))"

    @test_throws MethodError expr(rand(Float64, 2, 5))

    # Now, with passing operators explicitly
    X = rand(Float64, 2, 5)
    operators = OperatorEnum(; unary_operators=[cos])
    @test expr(X, operators) ≈ cos.(X[1, :])
end

@testitem "Miscellaneous expression calls" begin
    using DynamicExpressions
    using DynamicExpressions: get_tree, get_operators

    ex = @parse_expression(x1 + 1.5, binary_operators = [+], variable_names = ["x1"])
    @test DynamicExpressions.ExpressionModule.node_type(ex) <: Node

    @test !isempty(ex)

    tree = get_tree(ex)
    @test_throws ArgumentError get_operators(tree, nothing)
end

@testitem "Expression Literate examples" begin
    #literate_begin file="src/examples/expression.md"
    #=
    # `Expression` example

    `Expression` is a fundamental type in DynamicExpressions that represents
    a mathematical expression as a tree structure. It combines an
    `AbstractExpressionNode` (typically a `Node`) with metadata like operators
    and variable names.

    Let's explore how to create and work with `Expression` objects:
    =#
    using DynamicExpressions, Random

    # First, let's define our operators and variable names:

    operators = OperatorEnum(;
        binary_operators=[+, -, *, /], unary_operators=[sin, cos, exp]
    )
    variable_names = ["x", "y"]

    # Now, let's create a simple Expression manually:
    x = Node{Float64}(; feature=1)
    x_expr = Expression(x; operators, variable_names)

    # We can build up more complex expressions using these basic building blocks:
    y = Node{Float64}(; feature=2)
    c = Node{Float64}(; val=2.0)
    complex_node = Node(; op=3, l=x, r=Node(; op=1, l=y, r=c))
    # where the `3` indicates `*` and `1` indicates `+`.
    complex_expr = Expression(complex_node; operators, variable_names)

    #=
    This expression includes its own metadata: the operators and variable names,
    and so there are no scope issues as with raw `AbstractExpressionNode` types
    which depend on the last-used metadata for convenience functions like printing.
    In other words, you can print this expression, or evaluate it, directly:
    =#
    rng = Random.MersenneTwister(0)
    complex_expr(randn(rng, 2, 5))

    #=
    While creating expressions manually is faster, and should be preferred within packages,
    it can be cumbersome for quickly writing more complex expressions.
    DynamicExpressions provides a more convenient way to create expressions using
    the `parse_expression` function, which directly parses a Julia object:
    =#
    parsed_expr = parse_expression(
        :(sin(2.0 * x + exp(y + 5.0))); operators=operators, variable_names=variable_names
    )

    # We can convert an expression into the primitive `AbstractExpressionNode` type
    # with [`get_tree`](@ref):
    tree = get_tree(parsed_expr)
    @test tree isa AbstractExpressionNode  #src

    # Some `AbstractExpression` types may choose to store their expression in
    # a different way than simply saving it as one of the fields. For any expression,
    # you can get the raw contents with [`get_contents`](@ref):
    get_contents(parsed_expr)

    #=
    Similarly, you can get the metadata for an expression with [`get_metadata`](@ref):
    =#
    get_metadata(parsed_expr)

    #=
    These can be used with [`with_contents`](@ref) and [`with_metadata`](@ref) to
    create new expressions based on the original:
    =#
    with_contents(parsed_expr, Node(; op=2, l=get_contents(parsed_expr)))

    #=
    `Expression` objects support various operations defined on regular trees,
    which permits us to overload specific methods with modified behavior.
    For example, we can count the number of nodes, which simply forwards
    to the method as it is defined on [`Node`](@ref):
    =#
    node_count = count_nodes(parsed_expr)
    println("Number of nodes: $node_count")

    #=
    The [`tree_mapreduce`] will by default call [`get_tree`](@ref) to get the tree,
    so it can be used with any expression type that overloads this method.
    For example, we can compute the depth of a tree:
    =#
    tree_mapreduce(
        leaf -> 1, branch -> 1, (parent, child...) -> parent + max(child...), parsed_expr
    )

    #=
    We can also perform more complex operations, like simplification:
    =#
    complex_expr = parse_expression(
        :((2.0 + x) + 3.0); operators=operators, variable_names=["x"]
    )
    simplified_expr = combine_operators(copy(complex_expr))
    println("Original: ", complex_expr)
    println("Simplified: ", simplified_expr)

    #=
    `AbstractExpression` types also have many operators in `Base` defined, which
    will automatically look up the matching index in the stored [`OperatorEnum`](@ref).
    This means we can combine expressions like so:
    =#
    xs = [Expression(Node{Float64}(; feature=i); operators, variable_names) for i in 1:5]

    xs[1] + xs[2]
    # These have the same type – they simply combine their `AbstractExpressionNode` objects
    # and ensure the metadata is the same.
    typeof(xs[1] + xs[2])

    #=
    This gives us an easy way to quickly construct expressions with minimal memory overhead,
    and fast evaluation speed:
    =#
    ex = xs[1] * 2.1 - exp(3 * xs[2])

    # Evaluation:
    X = randn(rng, 5, 2)
    ex(X)

    # Or, if we have loaded Zygote, we can differentiate with respect
    # to the variables:
    using Zygote
    ex'(X)

    # Or the constants of the expression:
    ex'(X; variable=Val(false))

    # Which can be used for optimization.

    #literate_end
end
