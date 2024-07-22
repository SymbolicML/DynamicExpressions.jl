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

@testitem "Math with numbers and expressions" begin
    using DynamicExpressions

    ex = parse_expression(:(x + y); binary_operators=[+, *], variable_names=["x", "y"])

    ex2 = 2.0 * ex
    ex3 = ex * 2.0

    # Works for regular numbers too
    ex4 = 2 * ex
    ex5 = ex * 2

    shower(ex) = sprint((io, e) -> show(io, MIME"text/plain"(), e), ex)

    @test ex2 isa typeof(ex)
    @test ex3 isa typeof(ex)
    @test ex4 isa typeof(ex)
    @test ex5 isa typeof(ex)

    @test shower(ex) == "x + y"
    @test shower(ex2) == "2.0 * (x + y)"
    @test shower(ex3) == "(x + y) * 2.0"
    @test shower(ex4) == "2.0 * (x + y)"
    @test shower(ex5) == "(x + y) * 2.0"
end

@testitem "Math with non-numbers and expressions" begin
    using DynamicExpressions

    z = [1.0, 2.0]
    ex = parse_expression(
        :(x + y + $z);
        binary_operators=[+, *],
        variable_names=["x", "y"],
        node_type=Node{typeof(z)},
    )
    shower(ex) = sprint((io, e) -> show(io, MIME"text/plain"(), e), ex)
    @test typeof(ex) <: Expression{typeof(z)}
    @test shower(ex) == "(x + y) + [1.0, 2.0]"

    # Now, let's try to create a new expression with a vector added to this:
    ex2 = ex + [-4.0, 0.0]
    @test typeof(ex2) <: Expression{typeof(z)}
    @test shower(ex2) == "((x + y) + [1.0, 2.0]) + [-4.0, 0.0]"

    # Same on left side:
    @test shower([-4.0, 0.0] + ex) == "[-4.0, 0.0] + ((x + y) + [1.0, 2.0])"

    # Now, let's evaluate this:
    X = Matrix{Vector{Float64}}(undef, 2, 32)
    for i in eachindex(X)
        X[i] = rand(Float64, 2)
    end
    y = ex2(X)
    @test typeof(y) == Vector{Vector{Float64}}
    @test length(y) == 32
    @test length(first(y)) == 2
    true_out = X[1, 1] + X[2, 1] + [1.0, 2.0] - [4.0, 0.0]
    @test y[1] ≈ true_out
end

@testitem "Math with missing operators" begin
    using DynamicExpressions
    using DynamicExpressions.ExpressionAlgebraModule: MissingOperatorError

    ex = parse_expression(:(x + y); binary_operators=[+, *], variable_names=["x", "y"])

    @test_throws MissingOperatorError cos(ex)
    @test_throws MissingOperatorError ex / 1.0f0
    @test_throws MissingOperatorError ex / 1.0
    @test_throws MissingOperatorError ex / 1
    @test_throws MissingOperatorError 1.0f0 / ex
    @test_throws MissingOperatorError 1.0 / ex
    @test_throws MissingOperatorError 1 / ex

    if VERSION >= v"1.8-"
        err = try
            cos(ex)
        catch e
            e
        end
        @test occursin(
            "Operator cos not found in operators for expression type",
            sprint(showerror, err),
        )
    end
end
