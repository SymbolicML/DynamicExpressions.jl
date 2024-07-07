@testitem "Test evaluation with Supposition.jl" begin
    using DynamicExpressions
    using Supposition

    include("supposition_utils.jl")

    operators = OperatorEnum(; binary_operators=(+, -, *), unary_operators=())
    tree_gen = create_tree_gen(;
        operators,
        features=1:5,
        type=Int,
        val_gen=Data.Integers(-1000, 1000),
    )

    example(tree_gen, 1000)

    Supposition.@check function preserves_commutativity_plus(
        a=tree_gen,
        b=tree_gen,
        _X=Data.Vectors(
            Data.Vectors(
                Data.Integers(-1_000, 1_000);
                min_size=5,
                max_size=5,
            );
            min_size=1,
            max_size=32,
        ),
    )
        X = copy(hcat(_X...)')
        a_b = Node(; op=1, l=a, r=b)
        b_a = Node(; op=1, l=b, r=a)

        y_a_b, complete_a_b = eval_tree_array(a_b, X, operators)
        y_b_a, complete_b_a = eval_tree_array(b_a, X, operators)
        both_failed = !complete_a_b && !complete_b_a

        return both_failed || y_a_b ≈ y_b_a
    end
end
