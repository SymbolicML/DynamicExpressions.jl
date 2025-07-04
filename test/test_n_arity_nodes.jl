@testitem "N-ary Node Construction and Properties" tags = [:narity] begin
    using DynamicExpressions
    using Test

    my_unary_op(x) = x
    my_binary_op(x, y) = x
    my_ternary_op(x, y, z) = x

    operators = OperatorEnum(
        1 => (my_unary_op,), 2 => (my_binary_op,), 3 => (my_ternary_op,)
    )

    # Arity 1 (Unary) in a Node{T,3} type (max_degree is 3)
    n_una_leaf = Node{Float64,3}(; feature=1)
    n_una = Node{Float64,3}(; op=1, children=(n_una_leaf,))
    @test n_una.degree == 1
    @test n_una.op == 1
    @test DynamicExpressions.NodeModule.max_degree(n_una) == 3
    @test DynamicExpressions.NodeModule.max_degree(typeof(n_una)) == 3
    @test get_child(n_una, 1) === n_una_leaf
    # Test .l accessor (should work due to @make_accessors Node)
    @test n_una.l === n_una_leaf

    # Arity 2 (Binary) in a Node{T,3} type
    n_bin_leaf1 = Node{Float64,3}(; feature=1)
    n_bin_leaf2 = Node{Float64,3}(; val=2.0)
    n_bin = Node{Float64,3}(; op=1, children=(n_bin_leaf1, n_bin_leaf2))
    @test n_bin.degree == 2
    @test n_bin.op == 1
    @test DynamicExpressions.NodeModule.max_degree(n_bin) == 3
    @test get_child(n_bin, 1) === n_bin_leaf1
    @test get_child(n_bin, 2) === n_bin_leaf2
    @test_throws UndefRefError get_child(n_bin, 3)
    @test DynamicExpressions.NodeModule.get_children(n_bin, Val(2)) ==
        (n_bin_leaf1, n_bin_leaf2)
    # We can also call directly with integers
    @test DynamicExpressions.NodeModule.get_children(n_bin, 2) == (n_bin_leaf1, n_bin_leaf2)
    # .l and .r should work for Node{T,3} due to general @make_accessors Node
    @test n_bin.l === n_bin_leaf1
    @test n_bin.r === n_bin_leaf2

    # Arity 3 (Ternary) in a Node{T,3} type
    n_ter_leaf1 = Node{Float64,3}(; feature=1)
    n_ter_leaf2 = Node{Float64,3}(; feature=2)
    n_ter_leaf3 = Node{Float64,3}(; val=0.5)
    n_ter = Node{Float64,3}(; op=1, children=(n_ter_leaf1, n_ter_leaf2, n_ter_leaf3))
    @test n_ter.degree == 3
    @test n_ter.op == 1
    @test DynamicExpressions.NodeModule.max_degree(n_ter) == 3
    @test get_child(n_ter, 1) === n_ter_leaf1
    @test get_child(n_ter, 2) === n_ter_leaf2
    @test get_child(n_ter, 3) === n_ter_leaf3
    @test DynamicExpressions.NodeModule.get_children(n_ter, Val(3)) ==
        (n_ter_leaf1, n_ter_leaf2, n_ter_leaf3)
    @test n_ter.l === n_ter_leaf1
    @test n_ter.r === n_ter_leaf2

    # Test .l and .r accessors explicitly for Node{T,2} as per diff's specific @make_accessors Node{T,2}
    n2_leaf1_for_l_r = Node{Float64,2}(; feature=1)
    n2_leaf2_for_l_r = Node{Float64,2}(; val=2.0)
    ops_for_d2_accessors = OperatorEnum(2 => (my_binary_op,))
    n2_bin_for_l_r = Node{Float64,2}(; op=1, children=(n2_leaf1_for_l_r, n2_leaf2_for_l_r))
    @test n2_bin_for_l_r.l === n2_leaf1_for_l_r
    @test n2_bin_for_l_r.r === n2_leaf2_for_l_r
    n2_new_leaf_for_l_r = Node{Float64,2}(; feature=3)
    n2_bin_for_l_r.l = n2_new_leaf_for_l_r # Uses setproperty!
    @test get_child(n2_bin_for_l_r, 1) === n2_new_leaf_for_l_r

    n_default_D_leaf1 = Node{Float64}(; feature=1)
    @test DynamicExpressions.NodeModule.max_degree(typeof(n_default_D_leaf1)) == 2

    n_f32_d3_promo = Node{Float32,3}(; val=1.0f0)
    n_f64_d3_promo = Node{Float64,3}(; val=2.0)
    promoted_nodes = promote(n_f32_d3_promo, n_f64_d3_promo)
    @test promoted_nodes[1] isa Node{Float64,3}
    @test promoted_nodes[2] isa Node{Float64,3}

    @test DynamicExpressions.NodeModule.with_max_degree(Node{Float64,2}, Val(3)) ==
        Node{Float64,3}
    @test DynamicExpressions.NodeModule.with_max_degree(Node{Float64,3}, Val(2)) ==
        Node{Float64,2}
    @test DynamicExpressions.NodeModule.with_max_degree(Node{Float64}, Val(4)) ==
        Node{Float64,4}

    @test DynamicExpressions.NodeModule.defines_eltype(Node{Float64,2}) == true
    @test DynamicExpressions.NodeModule.defines_eltype(Node) == false
end

@testitem "Nodes throw UndefRefError when accessing poisoned children" begin
    using DynamicExpressions
    using Test

    n = Node{Float64,2}(; op=1, children=(Node{Float64,2}(; feature=1),))
    @test n.l == Node{Float64,2}(; feature=1)
    @test_throws UndefRefError n.r
    @test get_child(n, 1) == Node{Float64,2}(; feature=1)
    @test_throws UndefRefError get_child(n, 2)
end

@testitem "N-ary OperatorEnum Structure" tags = [:narity] begin
    using DynamicExpressions
    using Test

    my_unary_op(x) = x
    my_binary_op(x, y) = x
    my_ternary_op(x, y, z) = x

    operators_unary_only = OperatorEnum(1 => (my_unary_op,))
    @test length(operators_unary_only) == 1
    @test operators_unary_only.unaops == (my_unary_op,)
    @test operators_unary_only.binops == ()
    @test operators_unary_only[1] == (my_unary_op,)

    operators_binary_only = OperatorEnum(2 => (my_binary_op,))
    @test length(operators_binary_only) == 2
    @test operators_binary_only.unaops == ()
    @test operators_binary_only.binops == (my_binary_op,)
    @test operators_binary_only[2] == (my_binary_op,)

    operators_full = OperatorEnum(
        1 => (my_unary_op,), 2 => (my_binary_op,), 3 => (my_ternary_op,)
    )
    @test length(operators_full) == 3
    @test operators_full.unaops == (my_unary_op,)
    @test operators_full.binops == (my_binary_op,)
    @test operators_full[1] == (my_unary_op,)
    @test operators_full[2] == (my_binary_op,)
    @test operators_full[3] == (my_ternary_op,)

    @test DynamicExpressions.EvaluateModule.get_nops(typeof(operators_full), Val(1)) == 1
    @test DynamicExpressions.EvaluateModule.get_nops(typeof(operators_full), Val(2)) == 1
    @test DynamicExpressions.EvaluateModule.get_nops(
        typeof(operators_unary_only), Val(1)
    ) == 1
    @test DynamicExpressions.EvaluateModule.get_nops(
        typeof(operators_unary_only), Val(2)
    ) == 0
    @test DynamicExpressions.EvaluateModule.get_nops(
        typeof(operators_binary_only), Val(1)
    ) == 0
    @test DynamicExpressions.EvaluateModule.get_nops(
        typeof(operators_binary_only), Val(2)
    ) == 1
end

@testitem "N-ary Evaluation (targeting dispatch_degn_eval)" tags = [:narity] begin
    using DynamicExpressions
    using Test
    using Random

    my_eval_unary_op(x) = sin(x)
    my_eval_binary_op(x, y) = x^2 + y
    my_eval_ternary_op(x, y, z) = x * y - z

    operators_d3 = OperatorEnum(
        1 => (my_eval_unary_op,), 2 => (my_eval_binary_op,), 3 => (my_eval_ternary_op,)
    )

    x1 = Node{Float64,3}(; feature=1)
    x2 = Node{Float64,3}(; feature=2)
    x3 = Node{Float64,3}(; feature=3)
    c1_node = Node{Float64,3}(; val=0.5) # Renamed to avoid conflict

    X = randn(MersenneTwister(0), Float64, 3, 10)

    tree_bin_in_d3 = Node{Float64,3}(; op=1, children=(x1, c1_node))
    expected_bin_in_d3 = my_eval_binary_op.(X[1, :], 0.5)
    output_bin_in_d3, flag_bin_in_d3 = eval_tree_array(tree_bin_in_d3, X, operators_d3)
    @test flag_bin_in_d3
    @test output_bin_in_d3 ≈ expected_bin_in_d3

    tree_ter_in_d3 = Node{Float64,3}(; op=1, children=(x1, x2, c1_node))
    expected_ter_in_d3 = my_eval_ternary_op.(X[1, :], X[2, :], 0.5)
    output_ter_in_d3, flag_ter_in_d3 = eval_tree_array(tree_ter_in_d3, X, operators_d3)
    @test flag_ter_in_d3
    @test output_ter_in_d3 ≈ expected_ter_in_d3

    unary_child = Node{Float64,3}(; op=1, children=(x1,))
    binary_child_for_nest = Node{Float64,3}(; op=1, children=(x3, c1_node))
    tree_nested = Node{Float64,3}(; op=1, children=(unary_child, x2, binary_child_for_nest))
    expected_nested =
        my_eval_ternary_op.(
            my_eval_unary_op.(X[1, :]), X[2, :], my_eval_binary_op.(X[3, :], 0.5)
        )
    output_nested, flag_nested = eval_tree_array(tree_nested, X, operators_d3)
    @test flag_nested
    @test output_nested ≈ expected_nested

    tree_f32_c1 = Node{Float32,3}(; feature=1)
    tree_f32_c2 = Node{Float32,3}(; feature=2)
    tree_f32_c3 = Node{Float32,3}(; val=0.5f0)
    tree_f32 = Node{Float32,3}(; op=1, children=(tree_f32_c1, tree_f32_c2, tree_f32_c3))
    X_f64 = randn(MersenneTwister(1), Float64, 2, 5)

    output_promoted, flag_promoted = eval_tree_array(tree_f32, X_f64, operators_d3)
    @test flag_promoted
    @test eltype(output_promoted) == Float64
    expected_promoted = my_eval_ternary_op.(X_f64[1, :], X_f64[2, :], 0.5)
    @test output_promoted ≈ expected_promoted

    operators_d2 = OperatorEnum(2 => (my_eval_binary_op,))
    x1_d2 = Node{Float64,2}(; feature=1)
    c1_d2_node = Node{Float64,2}(; val=0.5) # Renamed
    tree_binary_d2 = Node{Float64,2}(; op=1, children=(x1_d2, c1_d2_node))
    expected_binary_d2 = my_eval_binary_op.(X[1, :], 0.5)
    output_binary_d2, flag_binary_d2 = eval_tree_array(tree_binary_d2, X, operators_d2)
    @test flag_binary_d2
    @test output_binary_d2 ≈ expected_binary_d2
end

@testitem "N-ary Constant Evaluation (targeting inner_dispatch_degn_eval_constant)" tags = [
    :narity
] begin
    using DynamicExpressions
    using Test

    my_c_unary_op(x) = sin(x)
    my_c_binary_op(x, y) = x^2 + y
    my_c_ternary_op(x, y, z) = x * y - z

    operators = OperatorEnum(
        1 => (my_c_unary_op,), 2 => (my_c_binary_op,), 3 => (my_c_ternary_op,)
    )

    c1_const = Node{Float64,3}(; val=0.5) # Renamed
    c2_const = Node{Float64,3}(; val=1.5) # Renamed
    c3_const = Node{Float64,3}(; val=2.5) # Renamed
    X_dummy = zeros(Float64, 1, 1)

    const_una_child = Node{Float64,3}(; op=1, children=(c1_const,))
    const_bin_child = Node{Float64,3}(; op=1, children=(c1_const, c3_const))
    tree_const_nested = Node{Float64,3}(;
        op=1, children=(const_una_child, c2_const, const_bin_child)
    )
    expected_val_nested = my_c_ternary_op(my_c_unary_op(0.5), 1.5, my_c_binary_op(0.5, 2.5))

    output_const_nested, flag_const_nested = eval_tree_array(
        tree_const_nested, X_dummy, operators
    )
    @test flag_const_nested
    @test all(output_const_nested .≈ expected_val_nested)

    const_eval_res_nested = DynamicExpressions.EvaluateModule.dispatch_constant_tree(
        tree_const_nested, operators
    )
    @test const_eval_res_nested.ok
    @test const_eval_res_nested.x ≈ expected_val_nested
end

@testitem "N-ary ExpressionAlgebra (targeting apply_operator, @declare_expression_operator)" tags = [
    :narity
] begin
    using DynamicExpressions
    using Test
    using Random

    operators_clamp = OperatorEnum(3 => (clamp,))
    DynamicExpressions.@extend_operators operators_clamp

    ex_x1 = Expression(Node{Float64,3}(; feature=1); operators=operators_clamp)
    ex_val_low = Expression(Node{Float64,3}(; val=0.0); operators=operators_clamp)
    ex_val_high = Expression(Node{Float64,3}(; val=1.0); operators=operators_clamp)

    expr_clamp3 = clamp(ex_x1, ex_val_low, ex_val_high)
    @test expr_clamp3.tree.degree == 3
    @test expr_clamp3.tree.op == 1
    X_clamp = [-0.5, 0.5, 1.5]'
    expected_clamp3 = clamp.(X_clamp[1, :], 0.0, 1.0)
    output_clamp3, flag_clamp3 = eval_tree_array(expr_clamp3.tree, X_clamp, operators_clamp)
    @test flag_clamp3
    @test output_clamp3 ≈ expected_clamp3

    operators_plus_chain = OperatorEnum(2 => (+,), 3 => (+,))
    DynamicExpressions.@extend_operators operators_plus_chain
    ex_p_x1 = Expression(Node{Float64,3}(; feature=1); operators=operators_plus_chain)
    ex_p_x2 = Expression(Node{Float64,3}(; feature=2); operators=operators_plus_chain)
    ex_p_x3 = Expression(Node{Float64,3}(; feature=3); operators=operators_plus_chain)

    expr_plus_ter = ex_p_x1 + ex_p_x2 + ex_p_x3
    @test expr_plus_ter.tree.degree == 3
    @test expr_plus_ter.tree.op == 1

    expr_plus_bin_const = ex_p_x1 + 0.5
    @test expr_plus_bin_const.tree.degree == 2
    @test expr_plus_bin_const.tree.op == 1
end

@testitem "N-ary String Representation (targeting Strings.jl changes)" tags = [:narity] begin
    using DynamicExpressions
    using Test

    my_str_unary_op(x) = x
    my_str_binary_op(x, y) = x
    my_str_ternary_op(x, y, z) = x

    operators = OperatorEnum(
        1 => (my_str_unary_op,), 2 => (my_str_binary_op,), 3 => (my_str_ternary_op,)
    )
    DynamicExpressions.@extend_operators operators

    x1_str = Node{Float64,3}(; feature=1) # Renamed
    x2_str = Node{Float64,3}(; feature=2) # Renamed
    x3_str = Node{Float64,3}(; feature=3) # Renamed

    tree_unary_expr = Expression(Node{Float64,3}(; op=1, children=(x1_str,)); operators)
    @test string_tree(tree_unary_expr) == "my_str_unary_op(x1)"

    tree_binary_expr = Expression(
        Node{Float64,3}(; op=1, children=(x1_str, x2_str)); operators
    )
    @test string_tree(tree_binary_expr) == "my_str_binary_op(x1, x2)"

    tree_ternary_expr = Expression(
        Node{Float64,3}(; op=1, children=(x1_str, x2_str, x3_str)); operators
    )
    @test string_tree(tree_ternary_expr) == "my_str_ternary_op(x1, x2, x3)"

    tree_unknown_unary = Node{Float64,3}(; op=1, children=(x1_str,))
    @test string_tree(tree_unknown_unary, nothing) == "unary_operator[1](x1)"
    tree_unknown_ternary = Node{Float64,3}(; op=1, children=(x1_str, x2_str, x3_str))
    @test string_tree(tree_unknown_ternary, nothing) == "operator_deg3[1](x1, x2, x3)"
end

@testitem "N-ary tree_mapreduce and base.jl convert" tags = [:narity] begin
    using DynamicExpressions
    using Test

    my_tmr_unary_op(x) = x
    my_tmr_binary_op(x, y) = x
    my_tmr_ternary_op(x, y, z) = x
    operators_tmr = OperatorEnum(
        1 => (my_tmr_unary_op,), 2 => (my_tmr_binary_op,), 3 => (my_tmr_ternary_op,)
    )

    x1_tmr = Node{Float64,3}(; feature=1)
    x2_tmr = Node{Float64,3}(; feature=2)
    x3_tmr = Node{Float64,3}(; feature=3)
    c1_tmr_node = Node{Float64,3}(; val=0.5) # Renamed

    unary_child_tmr = Node{Float64,3}(; op=1, children=(x1_tmr,))
    binary_child_tmr = Node{Float64,3}(; op=1, children=(x3_tmr, c1_tmr_node))
    tree_tmr = Node{Float64,3}(; op=1, children=(unary_child_tmr, x2_tmr, binary_child_tmr))

    num_nodes = tree_mapreduce(_ -> 1, (p, c...) -> p + sum(c), tree_tmr, Int)
    @test num_nodes == 7

    tree_f32_d3_target = convert(Node{Float32,3}, tree_tmr)
    @test typeof(tree_f32_d3_target) == Node{Float32,3}
    @test typeof(get_child(get_child(tree_f32_d3_target, 1), 1)) == Node{Float32,3}
    @test get_child(get_child(tree_f32_d3_target, 3), 2).val ≈ Float32(0.5)

    # Test convert(Node{T1}, node_of_type_N2{T2,D2})
    # This specific call needs Node{T1, D_source} as target for it to work without error with current convert.
    tree_f64_d3_for_convert = tree_tmr
    converted_tree_f32_d3_explicit_D = convert(Node{Float32,3}, tree_f64_d3_for_convert)
    @test typeof(converted_tree_f32_d3_explicit_D) == Node{Float32,3}

    # Test that converting between nodes with different degrees throws ArgumentError
    tree_d2 = Node{Float64,2}(; val=1.0)  # Node with max degree 2
    tree_d3 = Node{Float64,3}(; val=1.0)  # Node with max degree 3

    @test_throws ArgumentError convert(Node{Float64,2}, tree_d3)
    @test_throws ArgumentError convert(Node{Float64,3}, tree_d2)

    # Verify the error message contains the expected text
    try
        convert(Node{Float64,2}, tree_d3)
        @test false  # Should not reach here
    catch e
        @test e isa ArgumentError
        @test occursin("Cannot convert", e.msg)
        @test occursin("because they have different degrees", e.msg)
    end
end

@testitem "LoopVectorizationExt with N-ary (degn_eval)" tags = [:narity] begin
    using DynamicExpressions
    using Test
    using Random
    try
        using LoopVectorization
    catch e
        @warn "LoopVectorization not installed, skipping LoopVectorizationExt N-ary test."
        # To satisfy the rest of the test structure if LV is not available
        @eval const LoopVectorization = Nothing
    end

    my_lv_unary_op(x) = sin(x)
    my_lv_binary_op(x, y) = x^2 + y
    my_lv_ternary_op(x, y, z) = x * y - z

    let operators_for_lv = OperatorEnum(
            1 => (my_lv_unary_op,), 2 => (my_lv_binary_op,), 3 => (my_lv_ternary_op,)
        )
        if LoopVectorization !== Nothing &&
            DynamicExpressions.ExtensionInterfaceModule._is_loopvectorization_loaded(0)
            x1_lv = Node{Float64,3}(; feature=1)
            x2_lv = Node{Float64,3}(; feature=2)
            x3_lv = Node{Float64,3}(; feature=3)

            tree_lv_ternary = Node{Float64,3}(; op=1, children=(x1_lv, x2_lv, x3_lv))
            X_lv = randn(MersenneTwister(3), Float64, 3, 100)
            expected_lv_ternary = my_lv_ternary_op.(X_lv[1, :], X_lv[2, :], X_lv[3, :])

            output_lv_ternary_turbo, flag_turbo = eval_tree_array(
                tree_lv_ternary, X_lv, operators_for_lv; turbo=true
            )
            @test flag_turbo
            @test output_lv_ternary_turbo ≈ expected_lv_ternary

            output_lv_ternary_noturbo, flag_noturbo = eval_tree_array(
                tree_lv_ternary, X_lv, operators_for_lv; turbo=false
            )
            @test flag_noturbo
            @test output_lv_ternary_noturbo ≈ expected_lv_ternary
        else
            @warn "LoopVectorization not loaded or extension not triggered, skipping LoopVectorizationExt N-ary test."
        end
    end
end

@testitem "ParametricExpression with N-ary Node" tags = [:narity] begin
    using DynamicExpressions
    using Test
    using Random

    my_p_unary_op(x) = sin(x)
    my_p_binary_op(x, y) = x^2 + y
    my_p_ternary_op(x, y, z) = x * y - z

    operators_param = OperatorEnum(
        1 => (my_p_unary_op,), 2 => (my_p_binary_op,), 3 => (my_p_ternary_op,)
    )
    DynamicExpressions.@extend_operators operators_param

    pn_x1 = ParametricNode{Float64,3}(; feature=1)
    pn_x2 = ParametricNode{Float64,3}(; feature=2)
    pn_p1 = ParametricNode{Float64,3}()
    pn_p1.degree = UInt8(0)
    pn_p1.constant = false
    pn_p1.is_parameter = true
    pn_p1.parameter = UInt16(1)

    tree_parametric_ter = ParametricNode{Float64,3}(; op=1, children=(pn_p1, pn_x1, pn_x2))

    ex_param_ter = ParametricExpression(
        tree_parametric_ter;
        operators=operators_param,
        variable_names=["x1", "x2"],
        parameters=reshape([0.5, 1.5], 1, 2),
        parameter_names=["p1"],
    )

    @test DynamicExpressions.ExpressionModule.max_degree(ex_param_ter) == 3
    @test DynamicExpressions.ExpressionModule.node_type(ex_param_ter) ==
        ParametricNode{Float64,3}

    X_p = randn(MersenneTwister(4), Float64, 2, 10)
    classes_p = rand(MersenneTwister(5), 1:2, 10)

    expected_p = [
        my_p_ternary_op(
            ex_param_ter.metadata.parameters[1, classes_p[i]], X_p[1, i], X_p[2, i]
        ) for i in 1:10
    ]
    output_p, flag_p = eval_tree_array(ex_param_ter, X_p, classes_p, operators_param)
    @test flag_p
    @test output_p ≈ expected_p

    node_from_pex = convert(Node, ex_param_ter)
    @test typeof(node_from_pex) == Node{Float64,3}
    @test node_from_pex.degree == 3 && node_from_pex.op == 1
    @test get_child(node_from_pex, 1).feature == 1
    @test get_child(node_from_pex, 2).feature == 2
    @test get_child(node_from_pex, 3).feature == 3
end

@testitem "NodeUtils.jl NodeIndex for N-ary" tags = [:narity] begin
    using DynamicExpressions
    using Test
    using DynamicExpressions.NodeUtilsModule: index_constant_nodes, NodeIndex

    my_idx_unary(x) = x
    my_idx_binary(x, y) = x
    my_idx_ternary(x, y, z) = x
    operators_idx = OperatorEnum(
        1 => (my_idx_unary,), 2 => (my_idx_binary,), 3 => (my_idx_ternary,)
    )

    c1_idx = Node{Float64,3}(; val=1.0)
    f1_idx = Node{Float64,3}(; feature=1)
    c2_idx = Node{Float64,3}(; val=2.0)
    tree_idx = Node{Float64,3}(; op=1, children=(c1_idx, f1_idx, c2_idx))

    idx_tree = index_constant_nodes(tree_idx)

    @test idx_tree isa NodeIndex{UInt16,3}
    @test DynamicExpressions.NodeModule.max_degree(typeof(idx_tree)) == 3
    @test tree_idx.degree == 3
    @test idx_tree.degree == 3
    @test get_child(idx_tree, 1).degree == 0 && get_child(idx_tree, 1).val == UInt16(1)
    @test get_child(idx_tree, 2).degree == 0 && get_child(idx_tree, 2).val == UInt16(0)
    @test get_child(idx_tree, 3).degree == 0 && get_child(idx_tree, 3).val == UInt16(2)
end

@testitem "Edgecase with n-ary chainable operators" tags = [:narity] begin
    using DynamicExpressions

    operators = OperatorEnum(1 => (cos,), 2 => (-, *), 3 => (+,))
    x1, x2, x3 = Node{Float64,3}(; feature=1),
    Node{Float64,3}(; feature=2),
    Node{Float64,3}(; feature=3)
    operators = OperatorEnum(1 => (cos,), 2 => (-, *, +))

    # Normally this would have been an error, but now we chain it
    ex = Expression(+(x1, x2, x3); operators)
    @test string(ex) == "(x1 + x2) + x3"

    # But, we can still get explicit 3-degree nodes with + if it is defined
    operators = OperatorEnum(1 => (cos,), 2 => (-, *), 3 => (+,))
    ex2 = Expression(+(x1, x2, x3); operators)
    @test string(ex2) == "+(x1, x2, x3)"
end

@testitem "Node arbitrary-degree child access & mutation" begin
    using DynamicExpressions.NodeModule:
        Node, get_child, get_children, set_child!, set_children!, unsafe_get_children
    using Test

    # Build a tiny 3-ary tree:  +(1, 2, 3)
    c1 = Node{Float64,3}(; val=1.0)
    c2 = Node{Float64,3}(; val=2.0)
    c3 = Node{Float64,3}(; val=3.0)
    p = Node{Float64,3}(; op=1, children=(c1, c2, c3))

    @test p isa Node{Float64,3}          # new type parameter D shows up
    @test get_children(p, Val(3)) == (c1, c2, c3)
    @test get_child(p, 2) === c2
    @test get_children(p, Val(1)) === (c1,)  # Val-specialised accessor
    @test_throws BoundsError get_child(p, 4)

    # Replace the 3rd child in-place
    new_c3 = Node{Float64,3}(; val=30.0)
    set_child!(p, new_c3, 3)
    @test get_child(p, 3) === new_c3

    # Length mismatch is allowed; it will be poisoned at the end
    set_children!(p, (c1, c2))
    @test get_child(p, 2) === c2

    # Poison node is just self
    @test_throws UndefRefError get_child(p, 3)
    @test unsafe_get_children(p)[3].x === p
end

@testitem "Node property forwarding (`.l` / `.r`) still works" begin
    using DynamicExpressions.NodeModule: Node
    using Test

    a = Node{Float64,2}(; val=10.0)
    b = Node{Float64,2}(; val=20.0)
    root = Node{Float64,2}(; op=1, children=(a, b))

    @test root.l === a
    @test root.r === b

    new_a = Node{Float64,2}(; val=99.0)
    root.l = new_a
    @test root.l === new_a
end

@testitem "Constructor guards all-defaults call throws" begin
    using DynamicExpressions: AbstractExpressionNode, Nullable
    using Test

    mutable struct MyBadNode{T,D} <: AbstractExpressionNode{T,D}
        degree::UInt8
        constant::Bool
        val::T
        feature::UInt16
        op::UInt8
        children::NTuple{D,Nullable{MyBadNode{T,D}}}
    end

    # Calling with *nothing* default should now error
    @test_throws "Did you forget to define" MyBadNode{Float64,3}()

    mutable struct MyBadNode2{T,D} <: AbstractExpressionNode{T,D}
        degree::UInt8
        constant::Bool
        val::T
        feature::UInt16
        op::UInt8
        children::NTuple{D,Nullable{MyBadNode2{T,D}}}

        MyBadNode2{T,D}() where {T,D} = new{T,D}()
    end

    # We only need this; it will default to 2 for D anyways:
    node = MyBadNode2{Float64}()
    @test typeof(node) <: MyBadNode2{Float64,2}
end

@testitem "branch_copy_into! copies generic children" begin
    using DynamicExpressions.NodeModule: Node, unsafe_get_children
    using DynamicExpressions.NodePreallocationModule: branch_copy_into!
    using Test

    operators = OperatorEnum(2 => (+, -, *))
    leaves = ntuple(i -> Node{Float64,3}(; val=i), 3)
    src = Node{Float64,3}(; op=1, children=leaves)
    dummy = Node{Float64,3}(;
        op=2,
        children=(
            Node{Float64,3}(; val=0), Node{Float64,3}(; val=0), Node{Float64,3}(; val=0)
        ),
    )

    branch_copy_into!(dummy, src, unsafe_get_children(src)...)

    @test dummy.op == src.op
    @test unsafe_get_children(dummy) == unsafe_get_children(src)
end

@testitem "GraphNode compatibility" begin
    using DynamicExpressions
    using Test

    my_ternary_op(x, y, z) = x + y * z
    operators = OperatorEnum(1 => (sin,), 2 => (+, *), 3 => (my_ternary_op,))

    x1, x2, x3 = GraphNode{Float64,3}(; feature=1),
    GraphNode{Float64,3}(; feature=2),
    GraphNode{Float64,3}(; feature=3)
    c1 = GraphNode{Float64,3}(; val=2.5)

    # Test 1: Basic 3-degree node construction
    ternary_node = GraphNode{Float64,3}(; op=1, children=(x1, x2, x3))
    @test ternary_node.degree == 3
    @test ternary_node.op == 1
    @test length(ternary_node.children) == 3
    @test all(i -> get_child(ternary_node, i) === (x1, x2, x3)[i], 1:3)

    # Test 2: Sharing functionality
    shared_subexpr = GraphNode{Float64,3}(; op=1, children=(x1, x2, c1))
    tree_with_sharing = GraphNode{Float64,3}(;
        op=1, children=(shared_subexpr, x3, shared_subexpr)
    )

    # # Verify reference equality (sharing)
    @test get_child(tree_with_sharing, 1) === get_child(tree_with_sharing, 3)
    @test get_child(tree_with_sharing, 1) === shared_subexpr

    # Also should last through copy:
    tree_with_sharing_copy = copy(tree_with_sharing)
    @test get_child(tree_with_sharing_copy, 1) === get_child(tree_with_sharing_copy, 3)

    # Test 3: String representation shows sharing with {}
    sharing_string = string_tree(tree_with_sharing, operators)
    @test count("my_ternary_op(x1, x2, 2.5)", sharing_string) == 2
    @test count("{my_ternary_op(x1, x2, 2.5)}", sharing_string) == 1 # With sharing notation

    # Test 4: Copy operations
    # Breaking sharing creates separate instances
    tree_no_sharing = copy_node(tree_with_sharing; break_sharing=Val(true))
    @test get_child(tree_no_sharing, 1) !== get_child(tree_no_sharing, 3)  # No longer shared
    no_sharing_string = string_tree(tree_no_sharing, operators)
    @test !occursin("{", no_sharing_string)  # No more shared nodes
    @test count("my_ternary_op(x1, x2, 2.5)", no_sharing_string) == 2  # Appears twice

    # Test 5: Modification propagation through shared nodes
    original_string = string_tree(tree_with_sharing, operators)
    set_children!(shared_subexpr, (x3, x1, x2))  # Modify shared node
    modified_string = string_tree(tree_with_sharing, operators)
    @test original_string != modified_string
    @test get_child(tree_with_sharing, 1) === get_child(tree_with_sharing, 3)  # Still shared
    @test occursin("my_ternary_op(x3, x1, x2)", modified_string)

    # Test 6: Evaluation with 3-degree nodes
    X = [1.0 2.0; 0.5 1.5; 0.8 0.3]  # 3 features, 2 samples
    simple_ternary = GraphNode{Float64,3}(; op=1, children=(x1, x2, x3))
    expected = [my_ternary_op(1.0, 0.5, 0.8), my_ternary_op(2.0, 1.5, 0.3)]
    result, flag = eval_tree_array(simple_ternary, X, operators)
    @test flag
    @test result ≈ expected
end
