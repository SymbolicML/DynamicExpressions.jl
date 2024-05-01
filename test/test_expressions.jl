using DynamicExpressions
using DynamicExpressions: get_tree, get_operators, get_variable_names
using Zygote
using Test

@testset "Expression Initialization" begin
    let
        tree = Node(Float64; feature=1)
        operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])
        variable_names = ["x"]

        expr = Expression(tree, operators, variable_names)

        @test get_tree(expr) === tree
        @test get_operators(expr, nothing) === operators
        @test get_variable_names(expr, nothing) === variable_names

        copy_operators = OperatorEnum(; binary_operators=[+])
        copy_variable_names = ["y"]

        @test get_operators(expr, copy_operators) === copy_operators
        @test get_variable_names(expr, copy_variable_names) === copy_variable_names
    end
end

@testset "Evaluation" begin
    let
        ex = @parse_expression(
            sin(2.0 * x1 + exp(x2 + 5.0)),
            operators = OperatorEnum(;
                binary_operators=[+, -, *, /], unary_operators=[sin, cos, exp]
            ),
            variable_names = [:x1, :x2],
        )

        X = rand(Float64, 2, 10) + 1
        expected = @. sin(2.0 * X[1, :] + exp(X[2, :] + 5.0))
        expected_grad = stack(
            (@. 2.0 * cos(2.0 * X[1, :] + exp(X[2, :] + 5.0))),
            (@. cos(2.0 * X[1, :] + exp(X[2, :] + 5.0))),
        )

        if VERSION >= v"1.9"
            @test_nowarn begin
                result = ex(X)
                @test result ≈ expected
                result_grad ≈ ex'(X)
                @test result_grad ≈ expected_grad
            end
        end
    end
end

@testset "Utilities" begin end
