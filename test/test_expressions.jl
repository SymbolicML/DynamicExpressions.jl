using DynamicExpressions
using Test

@testset "Expression Initialization" begin
    let
        tree = Node(Float64; feature=1)
        operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])
        variable_names = ["x"]

        expr = Expression(tree, operators, variable_names)

        @test expr.tree === tree
        @test expr.operators === operators
        @test expr.variable_names == variable_names
        @test expr.display_variable_names == variable_names  # default copy of variable_names
    end
end

@testset "Nested Operations" begin
    let
        operators = OperatorEnum(;
            binary_operators=[+, -, *, /], unary_operators=[sin, cos, exp]
        )
        x1, x2 = Node{Float64}(; feature=1), Node{Float64}(; feature=2)
        tree = sin(2.0 * x1 + exp(x2 + 5.0))

        X = randn(Float64, 2, 10)
        expected = @. sin(2.0 * X[1, :] + exp(X[2, :] + 5.0))

        if VERSION >= v"1.9"
            @test_nowarn begin
                result = tree(X)
                @test result ≈ expected
            end
        end
    end
end
