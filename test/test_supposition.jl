@testitem "Test evaluation with Supposition.jl" begin
    using DynamicExpressions
    using Supposition

    function preserves_commutativity(operators, a, b, op, X)
        a_b = Node(; op, l=a, r=b)
        b_a = Node(; op, l=b, r=a)

        y_a_b, complete_a_b = eval_tree_array(a_b, X, operators)
        y_b_a, complete_b_a = eval_tree_array(b_a, X, operators)
        both_failed = !complete_a_b && !complete_b_a

        return both_failed || y_a_b ≈ y_b_a
    end
    function preserves_associativity(operators, a, b, c, op, X)
        a_b_and_c = Node(; op, l=Node(; op, l=a, r=b), r=c)
        a_and_b_c = Node(; op, l=a, r=Node(; op, l=b, r=c))

        y_a_b_and_c, complete_a_b_and_c = eval_tree_array(a_b_and_c, X, operators)
        y_a_and_b_c, complete_a_and_b_c = eval_tree_array(a_and_b_c, X, operators)
        both_failed = !complete_a_b_and_c && !complete_a_and_b_c

        return both_failed || y_a_b_and_c ≈ y_a_and_b_c
    end
    function preserves_subtract_self(operators, a, X)
        op = findfirst(i -> operators.binops[i] == (-), eachindex(operators.binops))::Int
        a_minus_a = Node(; op, l=a, r=a)
        y_a_m_a, complete_a_m_a = eval_tree_array(a_minus_a, X, operators)

        return !complete_a_m_a || y_a_m_a ≈ (0 * y_a_m_a)
    end
    function preserves_zero_multiplication(operators, a, X)
        T = eltype(X)
        op = findfirst(i -> operators.binops[i] == (*), eachindex(operators.binops))::Int
        a_times_zero = Node(; op, l=a, r=Node{T}(; val=0))
        zero_times_a = Node(; op, l=Node{T}(; val=0), r=a)

        y_a_times_zero, complete_a_times_zero = eval_tree_array(a_times_zero, X, operators)
        y_zero_times_a, complete_zero_times_a = eval_tree_array(zero_times_a, X, operators)

        return (!complete_a_times_zero || y_a_times_zero ≈ (0 * y_a_times_zero)) &&
               (!complete_zero_times_a || y_zero_times_a ≈ (0 * y_zero_times_a))
    end

    include("supposition_utils.jl")

    operators = OperatorEnum(;
        binary_operators=(+, -, *, max, min), unary_operators=(abs, -)
    )
    val_gen = Data.Integers(-1_000, 1_000)

    tree_gen = create_tree_gen(; operators, features=1:5, val_gen, max_layers=20)
    data_gen = map(
        X -> copy(hcat(X...)),
        Data.Vectors(
            Data.Vectors(val_gen; min_size=5, max_size=5); min_size=1, max_size=32
        ),
    )
    Supposition.@check preserves_commutativity(
        Data.Just(operators), tree_gen, tree_gen, Data.SampledFrom([1, 3, 4, 5]), data_gen
    )
    Supposition.@check preserves_associativity(
        Data.Just(operators),
        tree_gen,
        tree_gen,
        tree_gen,
        Data.SampledFrom([1, 3, 4, 5]),
        data_gen,
    )
    Supposition.@check preserves_subtract_self(Data.Just(operators), tree_gen, data_gen)
    Supposition.@check preserves_zero_multiplication(
        Data.Just(operators), tree_gen, data_gen
    )
end
