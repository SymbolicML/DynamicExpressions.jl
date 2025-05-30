@testitem "Supposition round-trip consistency" begin
    using Test
    using Random

    using Supposition
    using Supposition: @check, Data
    using DynamicExpressions
    using DynamicExpressions:
        string_tree, parse_expression, eval_tree_array, Node, get_operators, get_tree
    using LoopVectorization: LoopVectorization
    using Bumper: Bumper

    # bring the generator into scope
    include("supposition_utils.jl")

    n_features = 5
    max_layers = 20
    T = Float64
    operators = OperatorEnum(((abs, cos, exp), (+, -, *, /), (fma, clamp, +, max)))

    expr_gen = make_expression_generator(
        T; num_features=n_features, max_layers=max_layers, operators=operators
    )

    result = @check function roundtrip_string(ex=expr_gen)
        tree_str = string_tree(ex)
        ex_parsed = parse_expression(
            Meta.parse(tree_str);
            operators=get_operators(ex),
            variable_names=["x$i" for i in 1:n_features],
            node_type=Node{Float64,3},
        )
        return ex == ex_parsed
    end
    @test something(result.result) isa Supposition.Pass

    input_gen = make_input_matrix_generator(T; n_features)
    args_gen = map(
        (ex, X, turbo, bumper) -> (; ex, X, turbo, bumper),
        expr_gen,
        input_gen,
        Data.Booleans(),
        Data.Booleans(),
    )
    # We only consider expressions that don't have NaN/Inf/etc.
    clean_args_gen = filter(args -> eval_tree_array(args.ex, args.X)[2], args_gen)
    result2 = @check max_examples = 1000 function eval_against_string(args=clean_args_gen)
        (; ex, X, turbo, bumper) = args
        expression_result, ok = eval_tree_array(ex, X; turbo, bumper)
        tree_str = string_tree(ex)
        f = eval(Meta.parse("(x1, x2, x3, x4, x5) -> ($tree_str)"))
        true_result = Float64[Base.invokelatest(f, x...) for x in eachcol(X)]
        return ok && expression_result ≈ true_result
    end
    @test something(result2.result) isa Supposition.Pass
end
