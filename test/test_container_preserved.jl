using DynamicExpressions
using StaticArrays
using Test

@testset "StaticArrays type preserved" begin
    X = MMatrix{3,10}(randn(3, 10))
    operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[cos, sin])
    x1, x2, x3 = (i -> Node(Float64; feature=i)).(1:3)
    tree = cos(x1 * 5.2 - 0.9) * x3 + x2 * x2 - 2.2
    y = tree(X, operators)
    @test typeof(y) == MVector{10,Float64}

    X .= NaN
    tree = cos(x1 * 5.2 - 0.9) * x3 + x2 * x2 - 2.2
    y = tree(X, operators)
    @test typeof(y) == MVector{10,Float64}
end
