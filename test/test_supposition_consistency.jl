@testitem "Supposition round-trip consistency" begin
    using Test
    using Random

    using Supposition
    using Supposition: @check, Data
    using DynamicExpressions
    using DynamicExpressions:
        string_tree, parse_expression, eval_tree_array, Node, get_operators, get_tree

    # bring the generator into scope
    include("supposition_utils.jl")

    n_features = 5
    max_layers = 20
    T = Float64
    operators = OperatorEnum(((abs, cos, exp), (+, -, *, /), (fma, clamp, +, max)))

    expr_gen = make_expression_generator(
        T; num_features=n_features, max_layers=max_layers, operators=operators
    )

    @check function roundtrip_string(ex=expr_gen)
        tree_str = string_tree(ex)
        ex_parsed = parse_expression(
            Meta.parse(tree_str);
            operators=get_operators(ex),
            variable_names=["x$i" for i in 1:n_features],
            node_type=Node{Float64,3},
        )
        return ex == ex_parsed
    end

    input_gen = make_input_matrix_generator(T; n_features)
    @check max_examples = 1024 function eval_against_string(ex=expr_gen, X=input_gen)
        expression_result, ok = eval_tree_array(ex, X)
        !ok && return true  # If the expression is not valid, we can't test it
        tree_str = string_tree(ex)
        f_sym = gensym("f")
        f = eval(Meta.parse("(x1, x2, x3, x4, x5) -> ($tree_str)"))
        true_result = Float64[Base.invokelatest(f, x...) for x in eachcol(X)]
        return expression_result ≈ true_result
    end
end
