@testitem "Unary feature cache matches uncached evaluation" begin
    using DynamicExpressions
    using DynamicExpressions: ArrayBuffer
    using DynamicExpressions.EvaluateModule: UnaryFeatureCache, reset_index!
    using Random: MersenneTwister

    include("tree_gen_utils.jl")

    rng = MersenneTwister(0)
    X = randn(rng, 3, 32)
    # Non-finite unary outputs (exp overflow) and non-finite feature inputs:
    X[2, 5] = 800.0
    X[3, 7] = NaN

    # Operators must accept non-finite inputs, since early_exit=false keeps evaluating.
    few_unary = OperatorEnum(1 => [tanh, exp, abs], 2 => [+, -, *, /])
    # Over 15 unary operators selects the dispatch path that skips `@nif`.
    many_unary = @test_logs(
        (:warn, r"You have passed over 15 degree1.*"),
        OperatorEnum(1 => [[tanh, exp, abs]; [(x -> x * i) for i in 1:14]], 2 => [+, *])
    )

    for operators in (few_unary, many_unary), early_exit in (true, false)
        reference = EvalContext(; early_exit)
        cache = UnaryFeatureCache(Float64)
        cached = EvalContext(; early_exit, unary_cache=cache)
        buffer = ArrayBuffer(Vector{Float64}[], Ref(0))
        buffered = EvalContext(; early_exit, buffer, unary_cache=UnaryFeatureCache(Float64))
        for _ in 1:300
            tree = gen_random_tree_fixed_size(
                rand(rng, 1:12), operators, 3, Float64, Node, rng
            )
            y, ok = eval_tree_array(tree, X, operators; eval_context=reference)
            # Evaluate twice so the second pass reads entries the first pass stored.
            for _ in 1:2, context in (cached, buffered)
                reset_index!(buffer)
                y_cached, ok_cached = eval_tree_array(
                    tree, X, operators; eval_context=context
                )
                @test ok_cached == ok
                # Outputs are unspecified when evaluation reports failure.
                ok && @test isequal(y_cached, y)
            end
        end
        @test any(!isnothing, cache.entries)
    end
end

@testitem "Unary feature cache reuse and invalidation" begin
    using DynamicExpressions
    using DynamicExpressions.EvaluateModule: UnaryFeatureCache

    calls = Ref(0)
    counted_cos = x -> (calls[] += 1; cos(x))
    operators = OperatorEnum(1 => [counted_cos, exp], 2 => [+])
    x1 = Node(Float64; feature=1)
    tree = Node(; op=1, children=(x1,))
    X = randn(2, 8)
    context = EvalContext(; unary_cache=UnaryFeatureCache(Float64))

    y, ok = eval_tree_array(tree, X, operators; eval_context=context)
    @test ok && y == cos.(X[1, :])
    @test calls[] == 8
    y, ok = eval_tree_array(tree, X, operators; eval_context=context)
    @test ok && y == cos.(X[1, :])
    @test calls[] == 8

    X2 = randn(2, 8)
    y, ok = eval_tree_array(tree, X2, operators; eval_context=context)
    @test ok && y == cos.(X2[1, :])
    @test calls[] == 16

    other_operators = OperatorEnum(1 => [sin], 2 => [+])
    y, ok = eval_tree_array(tree, X2, other_operators; eval_context=context)
    @test ok && y == sin.(X2[1, :])

    # Copies start from an empty cache instead of sharing entries.
    copied = copy(context)
    @test copied.unary_cache isa UnaryFeatureCache{Float64}
    @test copied.unary_cache !== context.unary_cache
    y, ok = eval_tree_array(tree, X, operators; eval_context=copied)
    @test ok && y == cos.(X[1, :])
    @test calls[] == 24

    # Above 16 MiB of entries, the cache stays empty.
    big_X = randn(2, 600_000)
    calls[] = 0
    for _ in 1:2
        local y, ok = eval_tree_array(tree, big_X, operators; eval_context=context)
        @test ok && y == cos.(big_X[1, :])
    end
    @test calls[] == 2 * 600_000

    # Non-finite outputs are not reused under early exit.
    overflow = Node(; op=2, children=(x1,))
    X3 = [800.0 0.0; 1.0 1.0]
    for early_exit in (true, false)
        local context = EvalContext(; early_exit, unary_cache=UnaryFeatureCache(Float64))
        reference = eval_tree_array(
            overflow, X3, operators; eval_context=EvalContext(; early_exit)
        )
        for _ in 1:2
            local y, ok = eval_tree_array(overflow, X3, operators; eval_context=context)
            @test ok == reference[2] == !early_exit
            early_exit || @test isequal(y, reference[1])
        end
    end
end

@testitem "Positional EvalContext constructor" begin
    using DynamicExpressions

    context = EvalContext(Val(false), Val(false), Val(true), nothing, Val(true))
    @test context === EvalContext()
    @test context.unary_cache === nothing
end
