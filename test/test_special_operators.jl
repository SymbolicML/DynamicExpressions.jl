using TestItems: @testitem

@testitem "AssignOperator basic functionality" begin
    using DynamicExpressions
    using DynamicExpressions.SpecialOperatorsModule: AssignOperator
    using DynamicExpressions.EvaluateModule: eval_tree_array
    using Test
    using Random

    # Define operators and variable names
    assign_x2 = AssignOperator(; target_register=2)
    operators = OperatorEnum(;
        binary_operators=[+, -, *, /], unary_operators=[sin, cos, assign_x2]
    )
    variable_names = ["x1", "x2", "x3", "x4", "x5"]

    # Test data
    X = zeros(Float64, 2, 3)
    X[1, :] .= [1.0, 2.0, 3.0]
    X[2, :] .= [0.5, 1.5, 2.5]

    # 1. Basic register assignment - assign constant to register 2,
    #    and then add the return to `x2` (which should now be 3.0!)
    x1 = Expression(Node(; feature=1); operators, variable_names)
    x2 = Expression(Node(; feature=2); operators, variable_names)
    assign_expr = assign_x2(0.0 * x1 + 3.0) + x2

    @test string_tree(assign_expr) == "[x2 =]((0.0 * x1) + 3.0) + x2"

    # We should see that x2 will become 3.0 _before_ adding
    result, completed = eval_tree_array(assign_expr, X)
    @test completed == true
    @test all(==(6.0), result)

    # We should also see that X is not changed by this
    @test X[2, :] == [0.5, 1.5, 2.5]

    # But, with the reverse order, we get the x2 _before_ it was reassigned
    assign_expr_reverse = x2 + assign_x2(0.0 * x1 + 3.0)
    result, completed = eval_tree_array(assign_expr_reverse, X)
    @test completed == true
    @test result == [3.5, 4.5, 5.5]
end
