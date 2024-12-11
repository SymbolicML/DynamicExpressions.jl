@testitem "Buffer creation and validation" begin
    using DynamicExpressions
    using Test

    # Test data setup
    X = rand(2, 10)  # 2 features, 10 samples
    operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])
    tree = Node(;
        op=2, l=Node(; op=1, l=Node(Float64; feature=1)), r=Node(Float64; val=2.0)
    )

    # Basic buffer creation - buffer shape should match (num_leafs, num_samples)
    buffer = zeros(5, size(X, 2))  # 5 leaves should be enough for our test tree
    buffer_ref = Ref(0)
    eval_options = EvalOptions(; buffer=buffer, buffer_ref=buffer_ref)
    @test eval_options.buffer.array === buffer
    @test eval_options.buffer.index === buffer_ref

    # Test buffer is not allowed with turbo/bumper
    @test_throws AssertionError EvalOptions(;
        turbo=true, buffer=buffer, buffer_ref=buffer_ref
    )
    @test_throws AssertionError EvalOptions(;
        bumper=true, buffer=buffer, buffer_ref=buffer_ref
    )

    # Basic evaluation should work
    result = eval_tree_array(tree, X, operators; eval_options)
    @test length(result) == 2  # Returns (output, ok)
    @test size(result[1]) == (size(X, 2),)  # Output should match number of samples
end

@testitem "Buffer correctness" begin
    using DynamicExpressions
    using Test

    X = rand(2, 10)
    operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])

    # Test different tree structures
    @testset "Tree type: \$description" for (description, tree) in [
        ("Single feature", Node(Float64; feature=1)),
        ("Constant", Node(Float64; val=1.5)),
        ("Binary op", Node(; op=1, l=Node(Float64; feature=1), r=Node(Float64; val=2.0))),
        ("Unary op", Node(; op=1, l=Node(Float64; feature=1))),
    ]
        # Regular evaluation
        result1, ok1 = eval_tree_array(tree, X, operators)

        # Evaluation with buffer
        buffer = zeros(5, size(X, 2))
        buffer_ref = Ref(0)
        eval_options = EvalOptions(; buffer=buffer, buffer_ref=buffer_ref)
        result2, ok2 = eval_tree_array(tree, X, operators; eval_options)

        # Results should be identical
        @test result1 ≈ result2
        @test ok1 == ok2
    end
end

@testitem "Buffer index management" begin
    using DynamicExpressions
    using Test

    X = rand(2, 10)
    operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])

    # Create a complex tree that will use multiple buffer slots
    tree = Node(;
        op=1,  # +
        l=Node(; op=1, l=Node(Float64; feature=1)),  # sin(x1)
        r=Node(; op=1, l=Node(Float64; feature=2), r=Node(Float64; val=2.0)),  # x2 + 2
    )

    # This tree needs more buffer space due to intermediate computations
    buffer = zeros(10, size(X, 2))
    buffer_ref = Ref(0)
    eval_options = EvalOptions(; buffer=buffer, buffer_ref=buffer_ref)

    # Index should start at 1
    @test buffer_ref[] == 0

    # Evaluate
    result, ok = eval_tree_array(tree, X, operators; eval_options)

    # Index should have advanced
    @test buffer_ref[] == 2

    # Reset and evaluate again
    result2, ok2 = eval_tree_array(tree, X, operators; eval_options)
    # (We expect the index to automatically reset)

    # Results should be identical
    @test result ≈ result2
    @test ok == ok2
    @test buffer_ref[] == 2
end

@testitem "Buffer error handling" begin
    using DynamicExpressions
    using Test

    X = rand(2, 10)
    operators = OperatorEnum(; binary_operators=[+, /, *], unary_operators=[sin])

    # Create a tree that might produce NaN/Inf
    tree = Node(;
        op=2,  # /
        l=Node(Float64; val=1.0),
        r=Node(Float64; val=0.0),  # Division by zero
    )

    buffer = zeros(5, size(X, 2))
    buffer_ref = Ref(0)
    eval_options = EvalOptions(; buffer=buffer, buffer_ref=buffer_ref)

    # Test with early_exit=true
    result1, ok1 = eval_tree_array(tree, X, operators; eval_options)
    @test !ok1
end
