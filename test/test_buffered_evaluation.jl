@testitem "Buffer creation and validation" begin
    using DynamicExpressions
    using DynamicExpressions: ArrayBuffer

    # Test data setup
    X = rand(2, 10)  # 2 features, 10 samples
    operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])
    tree = Node(;
        op=2, l=Node(; op=1, l=Node(Float64; feature=1)), r=Node(Float64; val=2.0)
    )

    # Basic buffer creation - buffer shape should match (num_leafs, num_samples)
    buffer = zeros(5, size(X, 2))  # 5 leaves should be enough for our test tree
    buffer_ref = Ref(0)
    eval_context = EvalContext(; buffer=ArrayBuffer(buffer, buffer_ref))
    @test eval_context.buffer.array === buffer
    @test eval_context.buffer.index === buffer_ref

    copied_buffer = copy(eval_context.buffer)
    @test copied_buffer.array == buffer
    @test copied_buffer.array !== buffer
    @test copied_buffer.index[] == buffer_ref[]
    @test copied_buffer.index !== buffer_ref

    # Test buffer is not allowed with bumper
    @test_throws AssertionError EvalContext(;
        bumper=true, buffer=ArrayBuffer(buffer, buffer_ref)
    )

    # Basic evaluation should work
    result = eval_tree_array(tree, X, operators; eval_context)
    @test length(result) == 2  # Returns (output, ok)
    @test size(result[1]) == (size(X, 2),)  # Output should match number of samples
end

@testitem "Vector buffer growth and copying" begin
    using DynamicExpressions
    using DynamicExpressions: ArrayBuffer

    X = rand(2, 10)
    operators = OperatorEnum(; binary_operators=[+], unary_operators=[sin])
    tree = Node(;
        op=1,
        l=Node(; op=1, l=Node(Float64; feature=1)),
        r=Node(; op=1, l=Node(Float64; feature=2)),
    )
    arrays = Vector{Vector{Float64}}()
    buffer = ArrayBuffer(arrays, Ref(0))
    eval_context = EvalContext(; buffer)

    expected, expected_ok = eval_tree_array(tree, X, operators)
    result, ok = eval_tree_array(tree, X, operators; eval_context)

    @test result == expected
    @test ok == expected_ok
    @test length(arrays) == buffer.index[]

    for n in (3, 20)
        resized_X = rand(2, n)
        local expected, expected_ok = eval_tree_array(tree, resized_X, operators)
        local result, ok = eval_tree_array(tree, resized_X, operators; eval_context)
        @test result == expected
        @test ok == expected_ok
        @test axes(result) == axes(expected)
    end

    constant_result, constant_ok = eval_tree_array(
        Node(Float64; val=1.5), X, operators; eval_context
    )
    @test constant_result == fill(1.5, size(X, 2))
    @test constant_ok

    copied = copy(buffer)
    @test copied.array !== buffer.array
    @test all(a !== b for (a, b) in zip(copied.array, buffer.array))

    # Abstractly-typed buffer storage is not allowed
    @test_throws ArgumentError ArrayBuffer(AbstractVector{Float64}[], Ref(0))
end

@testitem "Buffer correctness" begin
    using DynamicExpressions
    using DynamicExpressions: ArrayBuffer

    X = rand(2, 10)
    operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])

    # Test different tree structures
    for tree in [
        Node(Float64; feature=1),
        Node(Float64; val=1.5),
        Node(; op=1, l=Node(Float64; feature=1), r=Node(Float64; val=2.0)),
        Node(; op=1, l=Node(Float64; feature=1)),
    ]
        # Regular evaluation
        result1, ok1 = eval_tree_array(tree, X, operators)

        # Evaluation with buffer
        buffer = zeros(5, size(X, 2))
        buffer_ref = Ref(0)
        eval_context = EvalContext(; buffer=ArrayBuffer(buffer, buffer_ref))
        result2, ok2 = eval_tree_array(tree, X, operators; eval_context)

        # Results should be identical
        @test result1 ≈ result2
        @test ok1 == ok2
    end
end

@testitem "Caller-owned buffer index management" begin
    using DynamicExpressions
    using DynamicExpressions: ArrayBuffer
    using DynamicExpressions.EvaluateModule: reset_index!

    X1 = reshape(1.0:10.0, 1, :)
    X2 = reshape(11.0:20.0, 1, :)
    operators = OperatorEnum(; unary_operators=[sin])
    tree = Node(; op=1, l=Node(Float64; feature=1))
    arrays = Vector{Vector{Float64}}()
    buffer = ArrayBuffer(arrays, Ref(0))
    eval_context = EvalContext(; buffer)

    result1, ok1 = eval_tree_array(tree, X1, operators; eval_context)
    expected1 = copy(result1)
    result2, ok2 = eval_tree_array(tree, X2, operators; eval_context)

    @test ok1 && ok2
    @test result1 === arrays[1]
    @test result2 === arrays[2]
    @test result1 == expected1

    reset_index!(buffer)
    result3, ok3 = eval_tree_array(tree, X2, operators; eval_context)
    @test ok3
    @test result3 === result1
    @test result3 == result2
end

@testitem "Buffer error handling" begin
    using DynamicExpressions
    using DynamicExpressions: ArrayBuffer

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
    eval_context = EvalContext(; buffer=ArrayBuffer(buffer, buffer_ref))

    # Test with early_exit=true
    result1, ok1 = eval_tree_array(tree, X, operators; eval_context)
    @test !ok1
end

@testitem "Random tree buffer evaluation" begin
    using DynamicExpressions
    using DynamicExpressions: ArrayBuffer
    using Random
    using LoopVectorization
    include("tree_gen_utils.jl")

    # Test setup
    X = rand(2, 10)
    operators = OperatorEnum(;
        binary_operators=[+, -, *, /], unary_operators=[sin, cos, exp]
    )

    for turbo in (false, true), i in 1:100
        # Generate a random tree with varying size (1-10 nodes)
        rng = Random.MersenneTwister(i)
        n_nodes = rand(rng, 1:10)
        tree = gen_random_tree_fixed_size(
            n_nodes, operators, size(X, 1), Float64, Node, rng
        )

        # Regular evaluation
        eval_options_no_buffer = EvalContext(; turbo)
        result1, ok1 = eval_tree_array(
            tree, X, operators; eval_context=eval_options_no_buffer
        )

        # Buffer evaluation
        buffer = Array{Float64}(undef, 2n_nodes, size(X, 2))
        buffer_ref = Ref(0)
        eval_context = EvalContext(; turbo, buffer=ArrayBuffer(buffer, buffer_ref))
        result2, ok2 = eval_tree_array(tree, X, operators; eval_context)

        # Results should be identical
        @test isapprox(result1, result2; atol=1e-10) || (!ok1 && !ok2)
        @test ok1 == ok2
    end
end
