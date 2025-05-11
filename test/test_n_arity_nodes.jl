@testitem "N-ary Node Construction and Properties" tags = [:narity] begin
    using DynamicExpressions
    using Test

    # Define some simple operators for structure, not evaluation here
    my_unary_op(x) = x # Placeholder
    my_binary_op(x, y) = x # Placeholder
    my_ternary_op(x, y, z) = x # Placeholder

    # Corrected OperatorEnum constructor:
    operators = OperatorEnum(((my_unary_op,), (my_binary_op,), (my_ternary_op,)))

    # Arity 1 (Unary) in a Node{T,3} type (max_degree is 3)
    n_una_leaf = Node{Float64,3}(; feature=1)
    n_una = Node{Float64,3}(; op=1, children=(n_una_leaf,)) # op=1 for my_unary_op
    @test n_una.degree == 1
    @test n_una.op == 1
    @test DynamicExpressions.NodeModule.max_degree(n_una) == 3
    @test DynamicExpressions.NodeModule.max_degree(typeof(n_una)) == 3
    @test n_una.children[1] === n_una_leaf
    # Test poison (node refers to itself for unused children slots)
    @test n_una.children[2] === n_una
    @test n_una.children[3] === n_una

    # Arity 2 (Binary) in a Node{T,3} type
    n_bin_leaf1 = Node{Float64,3}(; feature=1)
    n_bin_leaf2 = Node{Float64,3}(; val=2.0)
    n_bin = Node{Float64,3}(; op=1, children=(n_bin_leaf1, n_bin_leaf2)) # op=1 for my_binary_op
    @test n_bin.degree == 2
    @test n_bin.op == 1
    @test DynamicExpressions.NodeModule.max_degree(n_bin) == 3
    @test n_bin.children[1] === n_bin_leaf1
    @test n_bin.children[2] === n_bin_leaf2
    @test n_bin.children[3] === n_bin # Poison
    @test DynamicExpressions.NodeModule.children(n_bin, Val(2)) ==
        (n_bin_leaf1, n_bin_leaf2)
    # .l and .r should NOT be used for Node{T,3} as @make_accessors is for Node{T,2}
    @test_throws ErrorException n_bin.l # getfield Node: no field l

    # Arity 3 (Ternary) in a Node{T,3} type
    n_ter_leaf1 = Node{Float64,3}(; feature=1)
    n_ter_leaf2 = Node{Float64,3}(; feature=2)
    n_ter_leaf3 = Node{Float64,3}(; val=0.5)
    n_ter = Node{Float64,3}(; op=1, children=(n_ter_leaf1, n_ter_leaf2, n_ter_leaf3)) # op=1 for my_ternary_op
    @test n_ter.degree == 3
    @test n_ter.op == 1
    @test DynamicExpressions.NodeModule.max_degree(n_ter) == 3
    @test n_ter.children[1] === n_ter_leaf1
    @test n_ter.children[2] === n_ter_leaf2
    @test n_ter.children[3] === n_ter_leaf3
    @test DynamicExpressions.NodeModule.children(n_ter, Val(3)) ==
        (n_ter_leaf1, n_ter_leaf2, n_ter_leaf3)

    # Test .l and .r accessors ONLY for Node{T,2}
    n2_leaf1_for_l_r = Node{Float64,2}(; feature=1)
    n2_leaf2_for_l_r = Node{Float64,2}(; val=2.0)
    # Need an operator enum where binary op is index 1
    ops_for_d2_accessors = OperatorEnum(((), (my_binary_op,)))
    n2_bin_for_l_r = Node{Float64,2}(; op=1, children=(n2_leaf1_for_l_r, n2_leaf2_for_l_r))
    @test n2_bin_for_l_r.l === n2_leaf1_for_l_r
    @test n2_bin_for_l_r.r === n2_leaf2_for_l_r
    n2_new_leaf_for_l_r = Node{Float64,2}(; feature=3)
    n2_bin_for_l_r.l = n2_new_leaf_for_l_r
    @test n2_bin_for_l_r.children[1] === n2_new_leaf_for_l_r

    # Test default D=2 for Node{T}
    n_default_D_leaf1 = Node{Float64}(; feature=1) # This is Node{Float64,2}
    @test DynamicExpressions.NodeModule.max_degree(typeof(n_default_D_leaf1)) == 2

    # Test promoting node types
    n_f32_d3_promo = Node{Float32,3}(; val=1.0f0)
    n_f64_d3_promo = Node{Float64,3}(; val=2.0)
    promoted_nodes = promote(n_f32_d3_promo, n_f64_d3_promo)
    @test promoted_nodes[1] isa Node{Float64,3}
    @test promoted_nodes[2] isa Node{Float64,3}

    # Test with_max_degree
    @test DynamicExpressions.NodeModule.with_max_degree(Node{Float64,2}, Val(3)) ==
        Node{Float64,3}
    @test DynamicExpressions.NodeModule.with_max_degree(Node{Float64,3}, Val(2)) ==
        Node{Float64,2}
    # Node{Float64} is UnionAll, Node{Float64,2} after constructor.
    # with_max_degree(Node{Float64} where T, Val(D)) is used by convert.
    @test DynamicExpressions.NodeModule.with_max_degree(Node{Float64}, Val(4)) ==
        Node{Float64,4}

    # Test defines_eltype (internal, but used in node_factory)
    @test DynamicExpressions.NodeModule.defines_eltype(Node{Float64,2}) == true
    @test DynamicExpressions.NodeModule.defines_eltype(Node) == false # Node is UnionAll
end

@testitem "N-ary OperatorEnum Structure" tags = [:narity] begin
    using DynamicExpressions
    using Test

    my_unary_op(x) = x
    my_binary_op(x, y) = x
    my_ternary_op(x, y, z) = x

    operators_unary_only = OperatorEnum(((my_unary_op,),))
    @test length(operators_unary_only) == 1
    @test operators_unary_only.unaops == (my_unary_op,)
    @test operators_unary_only.binops == ()
    @test operators_unary_only[1] == (my_unary_op,)

    operators_binary_only = OperatorEnum(((), (my_binary_op,))) # Empty tuple for unary
    @test length(operators_binary_only) == 2
    @test operators_binary_only.unaops == ()
    @test operators_binary_only.binops == (my_binary_op,)
    @test operators_binary_only[2] == (my_binary_op,)

    operators_full = OperatorEnum(((my_unary_op,), (my_binary_op,), (my_ternary_op,)))
    @test length(operators_full) == 3
    @test operators_full.unaops == (my_unary_op,)
    @test operators_full.binops == (my_binary_op,)
    @test operators_full[1] == (my_unary_op,)
    @test operators_full[2] == (my_binary_op,)
    @test operators_full[3] == (my_ternary_op,)

    @test DynamicExpressions.EvaluateModule.get_nuna(typeof(operators_full)) == 1
    @test DynamicExpressions.EvaluateModule.get_nbin(typeof(operators_full)) == 1
    @test DynamicExpressions.EvaluateModule.get_nuna(typeof(operators_unary_only)) == 1
    @test DynamicExpressions.EvaluateModule.get_nbin(typeof(operators_unary_only)) == 0
    @test DynamicExpressions.EvaluateModule.get_nuna(typeof(operators_binary_only)) == 0 # Correct
    @test DynamicExpressions.EvaluateModule.get_nbin(typeof(operators_binary_only)) == 1
end

@testitem "N-ary Evaluation (targeting dispatch_degn_eval)" tags = [:narity] begin
    using DynamicExpressions
    using Test
    using Random

    my_eval_unary_op(x) = sin(x)
    my_eval_binary_op(x, y) = x^2 + y
    my_eval_ternary_op(x, y, z) = x * y - z

    # Operators for Node{Float64,3} which will force dispatch_degn_eval
    operators_d3 = OperatorEnum((
        (my_eval_unary_op,), (my_eval_binary_op,), (my_eval_ternary_op,)
    ))

    x1 = Node{Float64,3}(; feature=1)
    x2 = Node{Float64,3}(; feature=2)
    x3 = Node{Float64,3}(; feature=3)
    c1 = Node{Float64,3}(; val=0.5)

    X = randn(MersenneTwister(0), Float64, 3, 10)

    # Scenario 1: A binary operator in a Node{Float64,3} type instance.
    # This should hit `dispatch_degn_eval` because max_degree(typeof(tree_bin_in_d3)) == 3.
    tree_bin_in_d3 = Node{Float64,3}(; op=1, children=(x1, c1)) # my_eval_binary_op(x1, 0.5)
    expected_bin_in_d3 = my_eval_binary_op.(X[1, :], 0.5)
    # Crucial assumption: is_constant and its dependencies (all/any in base.jl) must be D-arity aware
    # for this test to pass without erroring before reaching dispatch_degn_eval.
    # If they are not, this test *will* fail, pointing to that issue in the base library code.
    output_bin_in_d3, flag_bin_in_d3 = eval_tree_array(tree_bin_in_d3, X, operators_d3)
    @test flag_bin_in_d3
    @test output_bin_in_d3 ≈ expected_bin_in_d3

    # Scenario 2: A ternary operator in a Node{Float64,3} type instance.
    # This should also hit `dispatch_degn_eval`.
    tree_ter_in_d3 = Node{Float64,3}(; op=1, children=(x1, x2, c1)) # my_eval_ternary_op(x1, x2, 0.5)
    expected_ter_in_d3 = my_eval_ternary_op.(X[1, :], X[2, :], 0.5)
    output_ter_in_d3, flag_ter_in_d3 = eval_tree_array(tree_ter_in_d3, X, operators_d3)
    @test flag_ter_in_d3
    @test output_ter_in_d3 ≈ expected_ter_in_d3

    # Test nested with different arities, ensuring children are also Node{Float64,3}
    unary_child = Node{Float64,3}(; op=1, children=(x1,)) # my_eval_unary_op(x1)
    binary_child = Node{Float64,3}(; op=1, children=(x3, c1)) # my_eval_binary_op(x3, 0.5)
    tree_nested = Node{Float64,3}(; op=1, children=(unary_child, x2, binary_child)) # my_eval_ternary_op(...)
    expected_nested =
        my_eval_ternary_op.(
            my_eval_unary_op.(X[1, :]), X[2, :], my_eval_binary_op.(X[3, :], 0.5)
        )
    output_nested, flag_nested = eval_tree_array(tree_nested, X, operators_d3)
    @test flag_nested
    @test output_nested ≈ expected_nested

    # Test with type promotion in eval_tree_array (target node type max_degree inferred from input tree)
    tree_f32_c1 = Node{Float32,3}(; feature=1)
    tree_f32_c2 = Node{Float32,3}(; feature=2)
    tree_f32_c3 = Node{Float32,3}(; val=0.5f0)
    tree_f32 = Node{Float32,3}(; op=1, children=(tree_f32_c1, tree_f32_c2, tree_f32_c3)) # Ternary op
    X_f64 = randn(MersenneTwister(1), Float64, 2, 5) # Only 2 features needed for this tree's variable nodes if used

    output_promoted, flag_promoted = eval_tree_array(tree_f32, X_f64, operators_d3) # operators_d3 has Float64 ops
    @test flag_promoted
    @test eltype(output_promoted) == Float64
    expected_promoted = my_eval_ternary_op.(X_f64[1, :], X_f64[2, :], 0.5)
    @test output_promoted ≈ expected_promoted
end

@testitem "N-ary Constant Evaluation (targeting inner_dispatch_degn_eval_constant)" tags = [
    :narity
] begin
    using DynamicExpressions
    using Test

    my_c_unary_op(x) = sin(x)
    my_c_binary_op(x, y) = x^2 + y
    my_c_ternary_op(x, y, z) = x * y - z

    operators = OperatorEnum(((my_c_unary_op,), (my_c_binary_op,), (my_c_ternary_op,)))

    c1 = Node{Float64,3}(; val=0.5)
    c2 = Node{Float64,3}(; val=1.5)
    c3 = Node{Float64,3}(; val=2.5)
    X_dummy = zeros(Float64, 1, 1) # eval_tree_array needs X

    # Test structure: op_ter(op_una(c1), c2, op_bin(c1,c3))
    # This ensures recursive calls to dispatch_constant_tree and inner_dispatch_degn_eval_constant
    const_una_child = Node{Float64,3}(; op=1, children=(c1,)) # my_c_unary_op(0.5)
    const_bin_child = Node{Float64,3}(; op=1, children=(c1, c3)) # my_c_binary_op(0.5, 2.5)
    tree_const_nested = Node{Float64,3}(;
        op=1, children=(const_una_child, c2, const_bin_child)
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

    # `clamp` is one of the default 3-arity ops handled by @declare_expression_operator
    operators_clamp = OperatorEnum(((), (), (clamp,))) # arity 1 (empty), 2 (empty), 3
    DynamicExpressions.@extend_operators operators_clamp

    ex_x1 = Expression(Node{Float64,3}(; feature=1); operators=operators_clamp)
    ex_val_low = Expression(Node{Float64,3}(; val=0.0); operators=operators_clamp)
    ex_val_high = Expression(Node{Float64,3}(; val=1.0); operators=operators_clamp)

    expr_clamp3 = clamp(ex_x1, ex_val_low, ex_val_high) # Uses @declare_expression_operator
    @test expr_clamp3.tree.degree == 3
    @test expr_clamp3.tree.op == 1 # Index of clamp in ternary list
    X = [-0.5, 0.5, 1.5]'
    expected_clamp3 = clamp.(X[1, :], 0.0, 1.0)
    # Test evaluation of the Expression object
    output_clamp3, flag_clamp3 = eval_tree_array(expr_clamp3.tree, X, operators_clamp)
    @test flag_clamp3
    @test output_clamp3 ≈ expected_clamp3

    # Test chaining for `+` (another default N-ary)
    operators_plus_chain = OperatorEnum(((), (+,), (+,))) # Binary plus, Ternary plus
    DynamicExpressions.@extend_operators operators_plus_chain
    ex_p_x1 = Expression(Node{Float64,3}(; feature=1); operators=operators_plus_chain)
    ex_p_x2 = Expression(Node{Float64,3}(; feature=2); operators=operators_plus_chain)
    ex_p_x3 = Expression(Node{Float64,3}(; feature=3); operators=operators_plus_chain)

    # x1 + x2 + x3 should use ternary plus
    expr_plus_ter = ex_p_x1 + ex_p_x2 + ex_p_x3
    @test expr_plus_ter.tree.degree == 3
    @test expr_plus_ter.tree.op == 1 # Index of ternary +

    # x1 + x2 (constant) should use binary plus
    expr_plus_bin_const = ex_p_x1 + 0.5
    @test expr_plus_bin_const.tree.degree == 2
    @test expr_plus_bin_const.tree.op == 1 # Index of binary +
end

@testitem "N-ary String Representation (targeting Strings.jl changes)" tags = [:narity] begin
    using DynamicExpressions
    using Test

    my_str_unary_op(x) = x
    my_str_binary_op(x, y) = x
    my_str_ternary_op(x, y, z) = x

    operators = OperatorEnum((
        (my_str_unary_op,), (my_str_binary_op,), (my_str_ternary_op,)
    ))
    DynamicExpressions.@extend_operators operators # For Expression creation

    x1 = Node{Float64,3}(; feature=1)
    x2 = Node{Float64,3}(; feature=2)
    x3 = Node{Float64,3}(; feature=3)

    # Wrap in Expression to use its string_tree method
    tree_unary_expr = Expression(Node{Float64,3}(; op=1, children=(x1,)); operators)
    @test string_tree(tree_unary_expr) == "my_str_unary_op(x1)"

    tree_binary_expr = Expression(Node{Float64,3}(; op=1, children=(x1, x2)); operators)
    @test string_tree(tree_binary_expr) == "my_str_binary_op(x1, x2)"

    tree_ternary_expr = Expression(
        Node{Float64,3}(; op=1, children=(x1, x2, x3)); operators
    )
    @test string_tree(tree_ternary_expr) == "my_str_ternary_op(x1, x2, x3)"

    # Default naming for unknown operators (passed as nothing to string_tree)
    # These are raw nodes, so string_tree is called directly on them
    tree_unknown_unary = Node{Float64,3}(; op=1, children=(x1,))
    @test string_tree(tree_unknown_unary, nothing) == "unary_operator[1](x1)" # Assuming default op names
    tree_unknown_ternary = Node{Float64,3}(; op=1, children=(x1, x2, x3))
    @test string_tree(tree_unknown_ternary, nothing) == "operator_deg3[1](x1, x2, x3)"
end

@testitem "N-ary tree_mapreduce and base.jl convert" tags = [:narity] begin
    using DynamicExpressions
    using Test

    my_tmr_unary_op(x) = x
    my_tmr_binary_op(x, y) = x
    my_tmr_ternary_op(x, y, z) = x
    operators_tmr = OperatorEnum((
        (my_tmr_unary_op,), (my_tmr_binary_op,), (my_tmr_ternary_op,)
    ))

    x1_tmr = Node{Float64,3}(; feature=1)
    x2_tmr = Node{Float64,3}(; feature=2)
    x3_tmr = Node{Float64,3}(; feature=3)
    c1_tmr = Node{Float64,3}(; val=0.5)

    unary_child_tmr = Node{Float64,3}(; op=1, children=(x1_tmr,))
    binary_child_tmr = Node{Float64,3}(; op=1, children=(x3_tmr, c1_tmr))
    tree_tmr = Node{Float64,3}(; op=1, children=(unary_child_tmr, x2_tmr, binary_child_tmr))

    num_nodes = tree_mapreduce(_ -> 1, (p, c...) -> p + sum(c), tree_tmr, Int)
    @test num_nodes == 7

    tree_f32_tmr = convert(Node{Float32,3}, tree_tmr) # Converts Node{Float64,3} -> Node{Float32,3}
    @test typeof(tree_f32_tmr) == Node{Float32,3}
    @test typeof(tree_f32_tmr.children[1].children[1]) == Node{Float32,3} # Grandchild (x1_tmr)
    @test tree_f32_tmr.children[3].children[2].val ≈ Float32(0.5) # Grandchild (c1_tmr)

    # Test the convert variant: convert(Node{T1,D1_implicit}, node_of_type_N2{T2,D2})
    # It should become Node{T1,D2}
    tree_f64_d3_for_convert = tree_tmr # This is Node{Float64,3}
    # Convert to Node{Float32} (which implies Node{Float32,2} as target MAX_DEGREE initially)
    # but then with_max_degree(Node{Float32}, Val(3)) is used.
    converted_tree_f32_d3 = convert(Node{Float32}, tree_f64_d3_for_convert)
    @test typeof(converted_tree_f32_d3) == Node{Float32,3}
end

@testitem "LoopVectorizationExt with N-ary (degn_eval)" tags = [:narity] begin
    using DynamicExpressions
    using Test
    using Random

    my_lv_unary_op(x) = sin(x)
    my_lv_binary_op(x, y) = x^2 + y
    my_lv_ternary_op(x, y, z) = x * y - z

    let operators_for_lv = OperatorEnum((
            (my_lv_unary_op,), (my_lv_binary_op,), (my_lv_ternary_op,)
        ))
        if DynamicExpressions.ExtensionInterfaceModule._is_loopvectorization_loaded(0)
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

@testitem "SymbolicUtilsExt convert for N-ary" tags = [:narity] begin
    using DynamicExpressions
    using Test

    SU_EXT_LOADED =
        Base.get_extension(DynamicExpressions, :DynamicExpressionsSymbolicUtilsExt) !==
        nothing

    if SU_EXT_LOADED
        DynamicExpressionsSymbolicUtilsExt = Base.get_extension(
            DynamicExpressions, :DynamicExpressionsSymbolicUtilsExt
        )
        SymbolicUtils = DynamicExpressionsSymbolicUtilsExt.SymbolicUtils

        my_su_unary_op(x) = sin(x) # Needs to be ::Number for SU usually
        my_su_binary_op(x, y) = x * x + y
        # For ternary, must be careful how SU handles it. Let's use a registered one for safety if possible.
        # If not, SU might expand x*y-z into Term(-, [Term(*,...),...])
        # The `convert` diff handles SymbolicUtils.arguments(ex), which are direct arguments to a symbolic function.
        @eval MySUTernaryOp(x, y, z) = x * y - z # Dummy for registration
        SymbolicUtils.เด็กชาย(MySUTernaryOp) # Make it known to SU

        operators_su = OperatorEnum((
            (my_su_unary_op,), (my_su_binary_op,), (MySUTernaryOp,)
        ))

        SymbolicUtils.@syms x_sym y_sym z_sym

        # Unary: args length 1
        expr_su_unary = my_su_unary_op(x_sym)
        node_su_unary = convert(
            Node{Float64,3},
            expr_su_unary,
            operators_su;
            variable_names=["x_sym", "y_sym", "z_sym"],
        )
        @test node_su_unary.degree == 1 &&
            node_su_unary.op == 1 &&
            node_su_unary.children[1].feature == 1

        # Binary: args length 2
        expr_su_binary = my_su_binary_op(x_sym, y_sym)
        node_su_binary = convert(
            Node{Float64,3},
            expr_su_binary,
            operators_su;
            variable_names=["x_sym", "y_sym", "z_sym"],
        )
        @test node_su_binary.degree == 2 &&
            node_su_binary.op == 1 &&
            node_su_binary.children[1].feature == 1 &&
            node_su_binary.children[2].feature == 2

        # Ternary: args length 3. The diff's `else { (only(args),) }` path will be taken.
        # `only(args)` will error because `args` (from `SymbolicUtils.arguments`) has 3 elements.
        expr_su_ternary = MySUTernaryOp(x_sym, y_sym, z_sym)
        # This tests the code as written in the diff:
        @test_throws ArgumentError convert(
            Node{Float64,3},
            expr_su_ternary,
            operators_su;
            variable_names=["x_sym", "y_sym", "z_sym"],
        )
    else
        @warn "SymbolicUtils extension not loaded, skipping SymbolicUtilsExt N-ary test."
    end
end

@testitem "ParametricExpression with N-ary Node" tags = [:narity] begin
    using DynamicExpressions
    using Test
    using Random

    my_p_unary_op(x) = sin(x)
    my_p_binary_op(x, y) = x^2 + y
    my_p_ternary_op(x, y, z) = x * y - z

    operators_param = OperatorEnum((
        (my_p_unary_op,), (my_p_binary_op,), (my_p_ternary_op,)
    ))
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
        variable_names=["x1", "x2"], # x1 is feature 1, x2 is feature 2
        parameters=reshape([0.5, 1.5], 1, 2),
        parameter_names=["p1"],
    )

    @test DynamicExpressions.ExpressionModule.max_degree(ex_param_ter) == 3
    # node_type of ParametricExpression{T,N,D} is N, which is ParametricNode{T,D_node_type}
    # For this ex_param_ter, N is ParametricNode{Float64,3}
    @test DynamicExpressions.ExpressionModule.node_type(ex_param_ter) ==
        ParametricNode{Float64,3}

    X_p = randn(MersenneTwister(4), Float64, 2, 10) # 2 features: x1, x2
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
    @test typeof(node_from_pex) == Node{Float64,3} # D is from ParametricNode
    @test node_from_pex.degree == 3 && node_from_pex.op == 1
    @test node_from_pex.children[1].feature == 1 # p1 (num_params=1, so parameter 1 becomes feature 1)
    @test node_from_pex.children[2].feature == 2 # x1 (orig feat 1 becomes feat 1+1=2)
    @test node_from_pex.children[3].feature == 3 # x2 (orig feat 2 becomes feat 2+1=3)
end

@testitem "ReadOnlyNode with N-ary Node" tags = [:narity] begin
    using DynamicExpressions
    using Test

    my_ro_unary_op(x) = x
    my_ro_binary_op(x, y) = x
    my_ro_ternary_op(x, y, z) = x

    operators_ro = OperatorEnum((
        (my_ro_unary_op,), (my_ro_binary_op,), (my_ro_ternary_op,)
    ))
    DynamicExpressions.@extend_operators operators_ro

    x1_ro = Node{Float64,3}(; feature=1)
    x2_ro = Node{Float64,3}(; feature=2)
    x3_ro = Node{Float64,3}(; feature=3)
    tree_ro_ter = Node{Float64,3}(; op=1, children=(x1_ro, x2_ro, x3_ro))

    expr_ro = Expression(tree_ro_ter; operators=operators_ro)
    readonly_tree = DynamicExpressions.get_tree(expr_ro)

    @test readonly_tree isa DynamicExpressions.ReadOnlyNodeModule.AbstractReadOnlyNode
    inner_node_ro = DynamicExpressions.ReadOnlyNodeModule.inner(readonly_tree)
    @test DynamicExpressions.NodeModule.max_degree(inner_node_ro) == 3 # D of inner node
    @test readonly_tree.degree == 3 # Forwarded from inner node
    @test readonly_tree.op == 1   # Forwarded

    ro_children = DynamicExpressions.NodeModule.children(readonly_tree, Val(3))
    @test length(ro_children) == 3
    @test ro_children[1] isa DynamicExpressions.ReadOnlyNodeModule.AbstractReadOnlyNode
    @test ro_children[1].feature == 1 # Forwarded
    @test ro_children[2].feature == 2
    @test ro_children[3].feature == 3

    # .l and .r access on ReadOnlyNode wrapping Node{T,3}.
    # This should error because inner node Node{T,3} doesn't have .l/.r fields (only properties for Node{T,2}).
    @test_throws FieldError readonly_tree.l
    @test_throws FieldError readonly_tree.r
end

@testitem "Expression.jl default_node_type for N-ary" tags = [:narity] begin
    using DynamicExpressions
    using Test

    # Default node type for Expression{Float64} (which implies Node{T, DEFAULT_MAX_DEGREE=2} as its node_type parameter)
    # max_degree(Expression{Float64}) will be max_degree(Node{Float64,2}) = 2.
    # So, default_node_type(Expression{Float64}) becomes Node{Float64, 2}.
    DefaultNodeForExprT = DynamicExpressions.ExpressionModule.default_node_type(
        Expression{Float64}
    )
    @test DefaultNodeForExprT == Node{Float64,2}

    # If Expression is explicitly parameterized with Node{Float64,3}
    # max_degree(Expression{Float64, Node{Float64,3}}) will be max_degree(Node{Float64,3}) = 3.
    # So, default_node_type(...) becomes Node{Float64,3}.
    dt_expr_node3 = DynamicExpressions.ExpressionModule.default_node_type(
        Expression{Float64,Node{Float64,3}}
    )
    @test dt_expr_node3 == Node{Float64,3}

    # Test a custom expression type that influences default_node_type through its own max_degree
    # This struct itself determines a max_degree for expressions of its type
    struct MyCustomExprOverallArity{T,N<:AbstractExpressionNode{T},OVERALL_ARITY_PARAM} <:
           AbstractExpression{T,N}
        tree::N
    end
    DynamicExpressions.ExpressionModule.has_node_type(
        ::Type{<:MyCustomExprOverallArity{T,N,ARITY}}
    ) where {T,N,ARITY} = true
    DynamicExpressions.ExpressionModule.node_type(
        ::Type{<:MyCustomExprOverallArity{T,N,ARITY}}
    ) where {T,N,ARITY} = N
    DynamicExpressions.NodeModule.max_degree(
        ::Type{<:MyCustomExprOverallArity{T,N,ARITY}}
    ) where {T,N,ARITY} = ARITY # Expression type itself has a max_degree

    # default_node_type(MyCustomExprOverallArity{Float32, Node{Float32,2}, 4})
    # T = Float32. max_degree of this expression TYPE is 4.
    # So, default_node_type should be Node{Float32, 4}.
    dt_custom_overall_arity = DynamicExpressions.ExpressionModule.default_node_type(
        MyCustomExprOverallArity{Float32,Node{Float32,2},4}
    )
    @test dt_custom_overall_arity == Node{Float32,4}

    # A custom expression that defaults to max_degree(Node) because has_node_type is false.
    struct MySimpleExprNoNodeParam{T} <: AbstractExpression{T,Nothing} end
    DynamicExpressions.ExpressionModule.has_node_type(::Type{<:MySimpleExprNoNodeParam}) =
        false
    dt_my_simple_expr = DynamicExpressions.ExpressionModule.default_node_type(
        MySimpleExprNoNodeParam{Float64}
    )
    @test dt_my_simple_expr == Node{Float64,2} # max_degree(Node) is 2
end

@testitem "NodeUtils.jl NodeIndex for N-ary" tags = [:narity] begin
    using DynamicExpressions
    using Test
    using DynamicExpressions.NodeUtilsModule: index_constant_nodes, NodeIndex

    my_idx_unary(x) = x
    my_idx_binary(x, y) = x
    my_idx_ternary(x, y, z) = x
    operators_idx = OperatorEnum(((my_idx_unary,), (my_idx_binary,), (my_idx_ternary,)))

    c1_idx = Node{Float64,3}(; val=1.0)
    f1_idx = Node{Float64,3}(; feature=1)
    c2_idx = Node{Float64,3}(; val=2.0)
    tree_idx = Node{Float64,3}(; op=1, children=(c1_idx, f1_idx, c2_idx)) # my_idx_ternary_op(1.0, x1, 2.0)

    idx_tree = index_constant_nodes(tree_idx) # Should produce NodeIndex{UInt16,3}

    @test idx_tree isa NodeIndex{UInt16,3}
    @test DynamicExpressions.NodeModule.max_degree(typeof(idx_tree)) == 3
    @test idx_tree.degree == 3
    @test idx_tree.children[1].degree == 0 && idx_tree.children[1].val == UInt16(1) # Constant 1.0 is 1st const
    @test idx_tree.children[2].degree == 0 && idx_tree.children[2].val == UInt16(0) # Feature node
    @test idx_tree.children[3].degree == 0 && idx_tree.children[3].val == UInt16(2) # Constant 2.0 is 2nd const
end
