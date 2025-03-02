using TestItems: @testitem

@testitem "AssignOperator basic functionality" begin
    using DynamicExpressions
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
    @test string_tree(assign_expr_reverse) == "x2 + [x2 =]((0.0 * x1) + 3.0)"
    result, completed = eval_tree_array(assign_expr_reverse, X)
    @test completed == true
    @test result == [3.5, 4.5, 5.5]
end

@testitem "AssignOperator with self-assignment" begin
    using DynamicExpressions
    using Test
    using Random

    assign_x1 = AssignOperator(; target_register=1)
    operators = OperatorEnum(;
        binary_operators=[+, -, *, /], unary_operators=[sin, cos, assign_x1]
    )
    variable_names = ["a", "b", "c"]
    X = rand(Float64, 2, 10)

    x1 = Expression(Node(; feature=1); operators, variable_names)
    x2 = Expression(Node(; feature=2); operators, variable_names)
    x3 = Expression(Node(; feature=3); operators, variable_names)

    expr = assign_x1(assign_x1(x1 * 2) + x1)
    @test string_tree(expr) == "[a =]([a =](a * 2.0) + a)"

    result, completed = eval_tree_array(expr, X)
    @test completed == true
    @test result == X[1, :] .* 4.0
end

@testitem "Simplification disabled with special operators" begin
    using DynamicExpressions
    using Test

    # Create operators with and without special operator
    assign_op = AssignOperator(; target_register=1)
    special_operators = OperatorEnum(;
        binary_operators=[+, -, *, /], unary_operators=[sin, cos, assign_op]
    )
    normal_operators = OperatorEnum(;
        binary_operators=[+, -, *, /], unary_operators=[sin, cos]
    )

    @test DynamicExpressions.SpecialOperatorsModule.any_special_operators(special_operators)
    @test !DynamicExpressions.SpecialOperatorsModule.any_special_operators(normal_operators)

    # Create expressions using the Expression constructor
    const_val = 2.0

    # Simple expression that should simplify: 2.0 + 2.0
    raw_node = Node(; op=1, l=Node(; val=const_val), r=Node(; val=const_val))
    simple_expr = Expression(copy(raw_node); operators=normal_operators)
    simple_expr_special = Expression(copy(raw_node); operators=special_operators)

    @test string_tree(simple_expr) == "2.0 + 2.0"
    @test string_tree(simple_expr_special) == "2.0 + 2.0"

    # Test normal simplification works
    simplified = simplify_tree!(simple_expr)
    @test string_tree(simplified) == "4.0"

    # Test simplification is disabled with special operators
    not_simplified = simplify_tree!(simple_expr_special)
    @test string_tree(not_simplified) == "2.0 + 2.0"
end

@testitem "WhileOperator basic functionality" begin
    using DynamicExpressions
    using Test

    # Define operators
    while_op = WhileOperator(; max_iters=100)
    assign_x2 = AssignOperator(; target_register=2)
    operators = OperatorEnum(;
        binary_operators=[+, -, *, /, while_op],  # While is binary operator
        unary_operators=[assign_x2],
    )
    variable_names = ["x1", "x2", "x3"]

    # Test data - x2 starts at 1.0 for all samples
    X = zeros(Float64, 2, 3)
    X[2, :] .= 1.0  # x2 initial value

    # Build expression: while (3.0 - x2 > 0) do x2 = x2 + 1.0
    x2 = Expression(Node(; feature=2); operators, variable_names)
    expr = while_op(3.0 - x2, assign_x2(x2 + 1.0))

    @test string_tree(expr) == "while(3.0 - x2, [x2 =](x2 + 1.0))"

    result, completed = eval_tree_array(expr, X)
    @test completed == true
    @test all(result .≈ 3.0)  # After 2 iterations, x2 becomes 3.0
    @test X[2, :] == [1.0, 1.0, 1.0]  # Original data unchanged
end

@testitem "Fibonacci sequence with WhileOperator" begin
    using DynamicExpressions
    using Test

    # Define operators
    while_op = WhileOperator(; max_iters=100)
    assign_ops = [AssignOperator(; target_register=i) for i in 1:5]
    operators = OperatorEnum(;
        binary_operators=[+, -, *, /, while_op], unary_operators=assign_ops
    )
    variable_names = ["x1", "x2", "x3", "x4", "x5"]

    # Test data - x2=5 (counter), x3=0 (F(0)), x4=1 (F(1))
    X = zeros(Float64, 5, 4)
    # Set different Fibonacci sequence positions to calculate
    X[2, :] = [3.0, 5.0, 7.0, 10.0]  # Calculate F(3), F(5), F(7), F(10)

    # Initialize all rows with F(0)=0, F(1)=1
    X[3, :] .= 0.0  # x3 = 0.0 (F(0))
    X[4, :] .= 1.0  # x4 = 1.0 (F(1))

    xs = [Expression(Node(; feature=i); operators, variable_names) for i in 1:5]

    # Build expression: 
    condition = xs[2]  # WhileOperator implicitly checks if > 0
    body =
        assign_ops[5](xs[3]) +
        assign_ops[3](xs[4]) +
        assign_ops[4](xs[5] + xs[4]) +
        assign_ops[2](xs[2] - 1.0)
    expr = (while_op(condition, body) * 0.0) + xs[3]

    @test string_tree(expr) ==
        "(while(x2, (([x5 =](x3) + [x3 =](x4)) + [x4 =](x5 + x4)) + [x2 =](x2 - 1.0)) * 0.0) + x3"

    result, completed = eval_tree_array(expr, X)
    @test completed == true

    # Test each Fibonacci number is correctly calculated
    @test result ≈ [2.0, 5.0, 13.0, 55.0]  # F(3)=2, F(5)=5, F(7)=13, F(10)=55
end
