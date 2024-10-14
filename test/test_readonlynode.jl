@testitem "ReadOnlyNode construction and access" begin
    using DynamicExpressions
    using DynamicExpressions: ReadOnlyNode

    inner_node = Node{Float64}(; val=42.0)
    readonly_node = ReadOnlyNode(inner_node)

    @test readonly_node isa ReadOnlyNode
    @test getfield(readonly_node, :_inner) === inner_node
    @test readonly_node.degree == inner_node.degree
    @test readonly_node.constant == inner_node.constant
    @test readonly_node.val == inner_node.val
end

@testitem "ReadOnlyNode immutability" begin
    using DynamicExpressions
    using DynamicExpressions: ReadOnlyNode

    inner_node = Node{Float64}(; val=42.0)
    readonly_node = ReadOnlyNode(inner_node)

    @test_throws ErrorException readonly_node.val = 100.0
    @test_throws "Cannot set properties on a ReadOnlyNode" readonly_node.val = 100.0
end

@testitem "ReadOnlyNode - accessing children should return ReadOnlyNode" begin
    using DynamicExpressions
    using DynamicExpressions: ReadOnlyNode

    operators = OperatorEnum(; binary_operators=(+, -, *, /), unary_operators=(sin, exp))
    x1 = Node{Float64}(; feature=1)
    x2 = Node{Float64}(; feature=2)
    tree = 2 * x1 - sin(x2)
    readonly_node = ReadOnlyNode(tree)

    @test typeof(readonly_node.l) === typeof(readonly_node)
end

@testitem "ReadOnlyNode copy" begin
    using DynamicExpressions
    using DynamicExpressions: ReadOnlyNode

    inner_node = Node{Float64}(; val=42.0)
    readonly_node = ReadOnlyNode(inner_node)
    copied_node = copy(readonly_node)

    @test copied_node !== readonly_node
    @test copied_node == readonly_node
end

@testitem "StructuredExpression returns ReadOnlyNode" begin
    using DynamicExpressions
    using DynamicExpressions: ReadOnlyNode
    using DynamicExpressions: StructuredExpression

    operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[-, cos, exp])
    variable_names = ["x", "y"]
    kws = (; operators, variable_names)
    f = parse_expression(:(x * x - cos(2.5f0 * y + -0.5f0)); kws...)
    g = parse_expression(:(exp(-(y * y))); kws...)

    structured_expr = StructuredExpression((; f, g); structure=nt -> nt.f + nt.g, kws...)

    tree = get_tree(structured_expr)
    @test tree isa ReadOnlyNode
    @test string_tree(tree, operators; variable_names) ==
        "((x * x) - cos((2.5 * y) + -0.5)) + exp(-(y * y))"
    @test getfield(tree, :_inner) isa Node
end
