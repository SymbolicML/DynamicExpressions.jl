@testitem "StructuredExpression basics" begin
    using DynamicExpressions

    kws = (;
        binary_operators=[+, -, *, /],
        unary_operators=[-, cos, exp],
        variable_names=["x", "y"],
    )
    f = parse_expression(:(x * x - cos(2.5f0 * y + -0.5f0)); kws...)
    g = parse_expression(:(exp(-(y * y))); kws...)

    shower(ex) = sprint((io, e) -> show(io, MIME"text/plain"(), e), ex)

    f_plus_g = StructuredExpression((; f, g); structure=nt -> nt.f + nt.g)
    f_div_g = StructuredExpression((; f, g); structure=nt -> nt.f / nt.g)
    cos_f = StructuredExpression((; f); structure=nt -> cos(nt.f))
    exp_g = StructuredExpression((; g); structure=nt -> exp(nt.g))

    @test shower(f_plus_g) == "((x * x) - cos((2.5 * y) + -0.5)) + exp(-(y * y))"
    @test shower(f_div_g) == "((x * x) - cos((2.5 * y) + -0.5)) / exp(-(y * y))"
    @test shower(cos_f) == "cos((x * x) - cos((2.5 * y) + -0.5))"
    @test shower(exp_g) == "exp(exp(-(y * y)))"

    # Will also work for evaluation
    @test f_plus_g([1.0f0; 1.0f0;;])[1] ≈ (f + g)([1.0f0; 1.0f0;;])[1]
    @test f_div_g([1.0f0; 1.0f0;;])[1] ≈ (f / g)([1.0f0; 1.0f0;;])[1]
    @test cos_f([1.0f0; 1.0f0;;])[1] ≈ cos(f)([1.0f0; 1.0f0;;])[1]
    @test exp_g([1.0f0; 1.0f0;;])[1] ≈ exp(g)([1.0f0; 1.0f0;;])[1]
end

@testitem "Interface for structured expressions" begin
    using DynamicExpressions
    using DynamicExpressions: ExpressionInterface, NodeInterface
    using Interfaces: test

    kws = (;
        binary_operators=[+, -, *, /],
        unary_operators=[-, cos, exp],
        variable_names=["x", "y"],
    )
    f = parse_expression(:(x * x - cos(2.5f0 * y + -0.5f0)); kws...)
    g = parse_expression(:(exp(-(y * y))); kws...)

    ex = StructuredExpression((; f, g); structure=nt -> nt.f + nt.g)

    @test test(ExpressionInterface, StructuredExpression, [ex])
end

@testitem "Dispatching on a structured expression" begin
    using DynamicExpressions
    using DynamicExpressions: ExpressionInterface, NodeInterface
    using Interfaces: test

    kws = (;
        binary_operators=[+, -, *, /],
        unary_operators=[-, cos, exp],
        variable_names=["x", "y"],
    )
    my_factory(nt) = nt.f + nt.g

    f = parse_expression(:(x * x - cos(2.5f0 * y + -0.5f0)); kws...)
    g = parse_expression(:(exp(-(y * y))); kws...)

    ex = StructuredExpression((; f, g); structure=my_factory)

    h(_) = 1
    h(::StructuredExpression{<:Any,typeof(my_factory)}) = 2

    @test h(ex) == 2
end

@testitem "StructuredExpression Literate examples" begin
    #literate_begin file="src/examples/structured_expression.md"
    #=
    # `StructuredExpression` example

    `StructuredExpression`s allow you to specify a predefined structure for an
    expression that exists outside of the regular `AbstractExpressionNode` objects
    which store expressions as trees.

    Let's look at an example:
    =#
    using DynamicExpressions, Random

    # First, we will create some normal `Expression` objects.

    operators = OperatorEnum(1 => (cos, exp), 2 => (+, -, *, /))
    variable_names = ["x", "y"]
    x = Expression(Node{Float64}(; feature=1); operators, variable_names)
    y = Expression(Node{Float64}(; feature=2); operators, variable_names)
    @test typeof(x) <: AbstractExpression{Float64,<:Node{Float64}}  #src

    typeof(x)
    #=
    Any `AbstractExpression`, such as this `Expression` object, can be composed together
    using standard Julia math operations. For example, let's some complex expressions from these:
    =#
    f = x * x - cos(2.5f0 * y + -0.5f0)
    g = exp(2.0 - y * y)

    @test typeof(f) == typeof(g) == typeof(x)  #src
    f, g
    #=
    We can then create a `StructuredExpression` from these two expressions.
    This is a composite `AbstractExpression` object that composes multiple
    expressions during evaluation.
    =#
    ex = StructuredExpression(
        (; f, g); structure=nt -> nt.f + nt.g, operators, variable_names
    )
    ex
    @test typeof(ex) <: AbstractExpression{Float64,<:Node{Float64}}  #src
    #=
    Note that this is displayed as a single tree, with the `+` operator
    used to combine them. Despite this, the expression is not actually
    *stored* with the `+` operator in an `AbstractExpressionNode`.

    By default, using `get_tree` will evaluate the result of `nt.f + nt.g`.
    This let's us use things like the regular operations available to
    `AbstractExpressionNode`s:
    =#
    length(get_tree(ex))
    @test length(get_tree(ex)) == 17  #src
    #=
    Next, let's try to evaluate this on some random data:
    =#
    rng = Random.MersenneTwister(0)
    X = randn(rng, Float64, 2, 5)
    X
    #=
    Followed by the evaluation. Since we have stored the operators directly
    in the expression object, we do not need to pass the operators explicitly.
    Evaluation of an `AbstractExpression` is set up to forward through
    `get_tree`, so this will work automatically.
    =#
    ex(X)
    #=
    Which we can verify against the individual expressions:
    =#
    f(X) + g(X)
    @test ex(X) ≈ f(X) + g(X) #src

    #literate_end
end
