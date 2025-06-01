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

    # Bring the generator into scope
    include("supposition_utils.jl")

    # Test configuration constants
    const N_FEATURES = 5
    const MAX_LAYERS = 20
    const NUMERIC_TYPE = Float64
    const OPERATORS = OperatorEnum(((abs, cos, exp), (+, -, *, /), (fma, clamp, +, max)))
    const VARIABLE_NAMES = ["x$i" for i in 1:N_FEATURES]

    # Create expression generator
    expr_gen = make_expression_generator(
        NUMERIC_TYPE; num_features=N_FEATURES, max_layers=MAX_LAYERS, operators=OPERATORS
    )

    # Test 1: Round-trip string parsing consistency
    result = @check function roundtrip_string(ex=expr_gen)
        tree_str = string_tree(ex)
        ex_parsed = parse_expression(
            Meta.parse(tree_str);
            operators=get_operators(ex),
            variable_names=VARIABLE_NAMES,
            node_type=Node{Float64,3},
        )
        return ex == ex_parsed
    end
    @test something(result.result) isa Supposition.Pass

    # Test 2: Evaluation consistency against string representation
    input_gen = make_input_matrix_generator(NUMERIC_TYPE; n_features=N_FEATURES)

    # Helper function to create clean argument generators
    function clean_args_gen_maker(default_turbo)
        args_gen = map(
            (ex, X, turbo, bumper) -> let
                result, ok = eval_tree_array(ex, X; turbo, bumper)
                (; ex, X, turbo, bumper, result, ok)
            end,
            expr_gen,
            input_gen,
            map(_ -> default_turbo, Data.Booleans()),
            Data.Booleans(),
        )
        # We only consider expressions that don't have NaN/Inf/etc.
        return filter(args -> args.ok, args_gen)
    end

    # Helper function to create turbo evaluation function
    function create_turbo_function(tree_str)
        turbo_expr = "(x1, x2, x3, x4, x5) -> let y = deepcopy(x1); @turbo(@.(y = ($tree_str))); y; end"
        return eval(Meta.parse(turbo_expr))
    end

    # Helper function to create regular evaluation function
    function create_regular_function(tree_str)
        regular_expr = "(x1, x2, x3, x4, x5) -> ($tree_str)"
        return eval(Meta.parse(regular_expr))
    end

    # Helper function to evaluate with turbo
    function evaluate_with_turbo(f, X)
        return Base.invokelatest(f, X[1, :], X[2, :], X[3, :], X[4, :], X[5, :])
    end

    # Helper function to evaluate without turbo
    function evaluate_without_turbo(f, X)
        return Float64[Base.invokelatest(f, x...) for x in eachcol(X)]
    end

    # Helper function to evaluate expression against its string representation
    function _eval_against_string((; ex, X, turbo, bumper, result))
        tree_str = string_tree(ex)
        true_result = if turbo
            # Turbo changes the operators, so we need to use a different function
            f = create_turbo_function(tree_str)
            evaluate_with_turbo(f, X)
        else
            f = create_regular_function(tree_str)
            evaluate_without_turbo(f, X)
        end

        return result ≈ true_result
    end

    # Test evaluation consistency without turbo
    no_turbo_args_gen = clean_args_gen_maker(false)
    result2_noturbo = @check max_examples = 2000 function eval_against_string(
        args=no_turbo_args_gen
    )
        return _eval_against_string(args)
    end
    @test something(result2_noturbo.result) isa Supposition.Pass

    # TODO: We need to run this test manually, as there are too many
    #       examples where turbo evaluation is slightly different.
    # # Test evaluation consistency with turbo (fewer examples due to performance)
    # turbo_args_gen = clean_args_gen_maker(true)
    # counter = Ref(0)
    # result2_turbo = @check max_examples = 50 function eval_against_string(
    #     args=turbo_args_gen
    # )
    #     c = (counter[] += 1)
    #     if c > 50
    #         # Supposition seems to not listen to max_examples sometimes
    #         return true
    #     else
    #         return _eval_against_string(args)
    #     end
    # end
    # @test something(result2_turbo.result) isa Supposition.Pass
end
