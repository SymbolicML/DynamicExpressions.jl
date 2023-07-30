using DynamicExpressions
using StaticArrays
using Test
using Zygote

@testset "StaticArrays type preserved" begin
    for T in (Float32, Float64)
        X = MMatrix{3,10}(randn(T, 3, 10))
        operators = OperatorEnum(;
            binary_operators=[+, -, *, /], unary_operators=[cos, sin], enable_autodiff=true
        )
        x1, x2, x3 = (i -> Node(; feature=i)).(1:3)
        tree = cos(x1 * 5.2 - 0.9) * x3 + x2 * x2 - 2.2 * x1 + 1.0
        tree = convert(Node{T}, tree)

        y = tree(X, operators)
        @test typeof(y) == MVector{10,T}

        dy = tree'(X, operators)
        @test typeof(dy) == MMatrix{3,10,T,30}

        dy = tree'(X, operators; variable=false)
        @test typeof(dy) == MMatrix{4,10,T,40}

        # Even with NaNs:
        X .= T(NaN)
        y = tree(X, operators)
        @test typeof(y) == MVector{10,T}

        dy = tree'(X, operators)
        @test typeof(dy) == MMatrix{3,10,T,30}

        dy = tree'(X, operators; variable=false)
        @test typeof(dy) == MMatrix{4,10,T,40}
    end
end
