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

    f_plus_g = StructuredExpression((; f, g), nt -> nt.f + nt.g)
    f_div_g = StructuredExpression((; f, g), nt -> nt.f / nt.g)
    cos_f = StructuredExpression((; f), nt -> cos(nt.f))
    exp_g = StructuredExpression((; g), nt -> exp(nt.g))

    @test shower(f_plus_g) == "((x * x) - cos((2.5 * y) + -0.5)) + exp(-(y * y))"
    @test shower(f_div_g) == "((x * x) - cos((2.5 * y) + -0.5)) / exp(-(y * y))"
    @test shower(cos_f) == "cos((x * x) - cos((2.5 * y) + -0.5))"
    @test shower(exp_g) == "exp(exp(-(y * y)))"

    @static if VERSION >= v"1.7.0"
        # Will also work for evaluation
        @test f_plus_g([1.0f0; 1.0f0;;])[1] ≈ (f + g)([1.0f0; 1.0f0;;])[1]
        @test f_div_g([1.0f0; 1.0f0;;])[1] ≈ (f / g)([1.0f0; 1.0f0;;])[1]
        @test cos_f([1.0f0; 1.0f0;;])[1] ≈ cos(f)([1.0f0; 1.0f0;;])[1]
        @test exp_g([1.0f0; 1.0f0;;])[1] ≈ exp(g)([1.0f0; 1.0f0;;])[1]
    end
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

    ex = StructuredExpression((; f, g), nt -> nt.f + nt.g)

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

    c = [1]
    ex = StructuredExpression((; f, g), my_factory; a=c)

    @test ex.metadata.extra.a[] == 1
    @test ex.metadata.extra.a === c

    # Should copy everything down to the metadata:
    @test copy(ex).metadata.extra.a !== c

    h(_) = 1
    h(::StructuredExpression{<:Any,typeof(my_factory)}) = 2

    @test h(ex) == 2
end
