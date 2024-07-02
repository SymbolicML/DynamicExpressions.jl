@testitem "Math on Expression objects" begin
    using DynamicExpressions
    using Random: MersenneTwister

    kws = (;
        binary_operators=[+, -, *, /],
        unary_operators=[-, cos, exp],  # Use - as a unary operator too
        variable_names=["x", "y"],
    )
    f = parse_expression(:(x * x - cos(2.5f0 * y + -0.5f0)); kws...)
    g = parse_expression(:(exp(-(y * y))); kws...)

    shower(ex) = sprint((io, e) -> show(io, MIME"text/plain"(), e), ex)

    @test shower(f + g) == "((x * x) - cos((2.5 * y) + -0.5)) + exp(-(y * y))"
    @test shower(f / g) == "((x * x) - cos((2.5 * y) + -0.5)) / exp(-(y * y))"
    @test shower(cos(f)) == "cos((x * x) - cos((2.5 * y) + -0.5))"
    @test shower(exp(g)) == "exp(exp(-(y * y)))"

    rng = MersenneTwister(0)
    X = rand(rng, Float32, 2, 100)

    truth_1 = @. cos(exp(-(X[2, :] * X[2, :])))
    @test cos(g)(X) ≈ truth_1

    @static if VERSION >= v"1.7.0"
        # Test with zero inputs
        @test f([0.0f0; 0.0f0;;])[1] ≈ -cos(-0.5f0)
        @test g([0.0f0; 0.0f0;;])[1] ≈ 1.0f0

        # Test with large inputs
        large_x, large_y = 1.0f5, 1.0f5
        @test f([large_x; large_y;;])[1] ≈ large_x^2 - cos(2.5f0 * large_y - 0.5f0)
        @test g([0.0f0; large_y;;])[1] ≈ 0.0f0  # exp(-large_number) should be very close to 0

        # Test with small inputs
        small = 1.0f-5
        @test f([small; small;;])[1] ≈ small^2 - cos(2.5f0 * small - 0.5f0)
        @test g([0.0f0; small;;])[1] ≈ exp(-(small^2))

        # Test with negative inputs
        @test f([-1.0f0; -1.0f0;;])[1] ≈ 1.0f0 - cos(-2.5f0 - 0.5f0)
        @test g([0.0f0; -1.0f0;;])[1] ≈ exp(-1.0f0)

        # Test with NaN and Inf
        @test isnan(f([NaN32; 1.0f0;;])[1])
        @test isnan(g([0.0f0; NaN32;;])[1])

        # Should be equivariant with respect to composition
        @test (f * g)([1.0f0; 1.0f0;;])[1] ≈ f([1.0f0; 1.0f0;;])[1] * g([1.0f0; 1.0f0;;])[1]
        @test (f + g)([1.0f0; 1.0f0;;])[1] ≈ f([1.0f0; 1.0f0;;])[1] + g([1.0f0; 1.0f0;;])[1]
        @test (f - g)([1.0f0; 1.0f0;;])[1] ≈ f([1.0f0; 1.0f0;;])[1] - g([1.0f0; 1.0f0;;])[1]
        @test (f / g)([1.0f0; 1.0f0;;])[1] ≈ f([1.0f0; 1.0f0;;])[1] / g([1.0f0; 1.0f0;;])[1]

        @test (-f)([1.0f0; 1.0f0;;])[1] ≈ -f([1.0f0; 1.0f0;;])[1]
        @test (cos(f))([1.0f0; 1.0f0;;])[1] ≈ cos(f([1.0f0; 1.0f0;;])[1])
        @test (exp(g))([1.0f0; 1.0f0;;])[1] ≈ exp(g([1.0f0; 1.0f0;;])[1])
    end
end
