@testitem "Test evaluation with Supposition.jl" begin
    using DynamicExpressions
    using Supposition

    include("supposition_utils.jl")

    operators = OperatorEnum(;
        binary_operators=(+, -, *, max, min), unary_operators=(abs, -)
    )
    tree_gen = create_tree_gen(;
        operators,
        type=Int,
        features=1:5,
        val_gen=Data.Integers(-1_000, 1_000),
        max_layers=20,
    )
    data_gen = map(
        X -> copy(hcat(X...)),
        Data.Vectors(
            Data.Vectors(Data.Integers(-1_000, 1_000); min_size=5, max_size=5);
            min_size=1,
            max_size=32,
        ),
    )

    Supposition.@check function preserves_commutativity(
        a=tree_gen, b=tree_gen, op=Data.SampledFrom([1, 3, 4, 5]), X=data_gen
    )
        a_b = Node(; op, l=a, r=b)
        b_a = Node(; op, l=b, r=a)

        y_a_b, complete_a_b = eval_tree_array(a_b, X, operators)
        y_b_a, complete_b_a = eval_tree_array(b_a, X, operators)
        both_failed = !complete_a_b && !complete_b_a

        return both_failed || y_a_b ≈ y_b_a
    end
    Supposition.@check function preserves_associativity(
        a=tree_gen, b=tree_gen, c=tree_gen, op=Data.SampledFrom([1, 3, 4, 5]), X=data_gen
    )
        a_b_and_c = Node(; op, l=Node(; op, l=a, r=b), r=c)
        a_and_b_c = Node(; op, l=a, r=Node(; op, l=b, r=c))

        y_a_b_and_c, complete_a_b_and_c = eval_tree_array(a_b_and_c, X, operators)
        y_a_and_b_c, complete_a_and_b_c = eval_tree_array(a_and_b_c, X, operators)
        both_failed = !complete_a_b_and_c && !complete_a_and_b_c

        return both_failed || y_a_b_and_c ≈ y_a_and_b_c
    end
    Supposition.@check function preserves_subtract_self(a=tree_gen, X=data_gen)
        a_minus_a = Node(; op=2, l=a, r=a)
        y_a_m_a, complete_a_m_a = eval_tree_array(a_minus_a, X, operators)

        return !complete_a_m_a || y_a_m_a ≈ (0 * y_a_m_a)
    end
    Supposition.@check function preserves_zero_multiplication(a=tree_gen, X=data_gen)
        a_times_zero = Node(; op=3, l=a, r=Node{Int}(; val=0))
        zero_times_a = Node(; op=3, l=Node{Int}(; val=0), r=a)

        y_a_times_zero, complete_a_times_zero = eval_tree_array(a_times_zero, X, operators)
        y_zero_times_a, complete_zero_times_a = eval_tree_array(zero_times_a, X, operators)

        return (!complete_a_times_zero || y_a_times_zero ≈ (0 * y_a_times_zero)) &&
               (!complete_zero_times_a || y_zero_times_a ≈ (0 * y_zero_times_a))
    end
end
