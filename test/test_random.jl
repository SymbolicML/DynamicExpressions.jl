using DynamicExpressions
using Random
using Test

operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[cos, sin])
x = GraphNode(Float64; feature=1)
tree = cos(x) + x

num_samples = 10_000
atol = 200

@testset "Basic random sampling" begin
    uniform_samples = let rng = Random.MersenneTwister(0)
        [rand(rng, tree) for _ in 1:num_samples]
    end

    num_plus = count(Base.Fix1(===, tree), uniform_samples)
    num_x = count(Base.Fix1(===, x), uniform_samples)
    num_cos = count(Base.Fix1(===, tree.l), uniform_samples)

    @test isapprox(num_plus, num_samples ÷ 3; atol)
    @test isapprox(num_x, num_samples ÷ 3; atol)
    @test isapprox(num_cos, num_samples ÷ 3; atol)

    # Now, we sample without sharing
    for break_sharing in (true, Val(true))
        broken_sharing_samples = let rng = Random.MersenneTwister(0)
            [rand(rng, NodeSampler(; tree, break_sharing)) for _ in 1:num_samples]
        end
        num_plus = count(Base.Fix1(===, tree), broken_sharing_samples)
        num_x = count(Base.Fix1(===, x), broken_sharing_samples)
        num_cos = count(Base.Fix1(===, tree.l), broken_sharing_samples)

        @test isapprox(num_plus, num_samples ÷ 4; atol)
        @test isapprox(num_x, num_samples ÷ 2; atol)
        @test isapprox(num_cos, num_samples ÷ 4; atol)
    end
end

@testset "Weighted sampling" begin
    function weighting_1(t)
        if t == cos(x)
            75.0
        elseif t == x
            10.0
        else
            15.0
        end
    end
    specific_weighted_samples = let rng = Random.MersenneTwister(0)
        [rand(rng, NodeSampler(; tree, weighting=weighting_1)) for _ in 1:num_samples]
    end
    num_plus = count(Base.Fix1(===, tree), specific_weighted_samples)
    num_x = count(Base.Fix1(===, x), specific_weighted_samples)
    num_cos = count(Base.Fix1(===, tree.l), specific_weighted_samples)

    @test isapprox(num_plus, num_samples * 15 ÷ 100; atol)
    @test isapprox(num_x, num_samples * 10 ÷ 100; atol)
    @test isapprox(num_cos, num_samples * 75 ÷ 100; atol)

    # Now, without sharing
    function weighting_2(t)
        if t == cos(x)
            75.0
        elseif t == x
            10.0  # Will be doubled if we break sharing
        else
            5.0
        end
    end
    for break_sharing in (true, Val(true))
        broken_sharing_weighted_samples = let rng = Random.MersenneTwister(0)
            [
                rand(rng, NodeSampler(; tree, weighting=weighting_2, break_sharing)) for
                _ in 1:num_samples
            ]
        end
        num_plus = count(Base.Fix1(===, tree), broken_sharing_weighted_samples)
        num_x = count(Base.Fix1(===, x), broken_sharing_weighted_samples)
        num_cos = count(Base.Fix1(===, tree.l), broken_sharing_weighted_samples)

        @test isapprox(num_plus, num_samples * 5 ÷ 100; atol)
        @test isapprox(num_x, num_samples * 20 ÷ 100; atol)
        @test isapprox(num_cos, num_samples * 75 ÷ 100; atol)
    end
end
