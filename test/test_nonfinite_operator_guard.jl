@testitem "nonfinite operator guard" begin
    using DynamicExpressions
    using DynamicExpressions.ValueInterfaceModule: turbo_can_eval_nonfinite
    using LoopVectorization
    using Test

    @test !turbo_can_eval_nonfinite(sin)
    @test turbo_can_eval_nonfinite(exp)
    @test turbo_can_eval_nonfinite(+)
    @test !turbo_can_eval_nonfinite(log)
    @test !turbo_can_eval_nonfinite(sqrt)
    f(x) = x
    @test !turbo_can_eval_nonfinite(f)

    operators = OperatorEnum(;
        binary_operators=(+, *, /, -), unary_operators=(sin, cos, tan, exp, log, sqrt)
    )

    # Compound trees hit the guarded @turbo paths in the LV extension.
    for T in (Float32, Float64)
        X = fill(T(Inf), 1, 8)
        for f in (sin, cos, tan, log, sqrt)
            tree = convert(Node{T}, f(f(Node("x1"))))
            out, flag = eval_tree_array(tree, X, operators; turbo=true)
            @test all(isnan, out)
            @test !flag
        end
    end

    for T in (Float32, Float64)
        X = reshape(T[1, 2, 3, 4, 5], 1, 5)
        for f in (sin, cos, exp, log)
            tree = convert(Node{T}, f(f(Node("x1"))))
            a, _ = eval_tree_array(tree, X, operators; turbo=true)
            b, _ = eval_tree_array(tree, X, operators; turbo=false)
            @test a ≈ b
        end
    end
end


@testitem "nonfinite guard - compound binary+unary paths" begin
    using DynamicExpressions
    using DynamicExpressions.ValueInterfaceModule: turbo_can_eval_nonfinite
    using LoopVectorization
    using Test

    operators = OperatorEnum(;
        binary_operators=(+, *, /, -), unary_operators=(exp, sin, log, sqrt)
    )

    # Trees that hit the `turbo_can_eval_nonfinite == true` branch (exp)
    # and `false` branch (sin, log, sqrt) in deg1 compound evaluation.
    for T in (Float32, Float64)
        X = reshape(T[1, 2, 3, 4, 5], 1, 5)

        for f in (exp, sin, log, sqrt)
            # unary(binary(const, feature)) — deg1_l2_ll0_lr0_eval, const+const
            tree = convert(Node{T}, f(Node(; val=T(2)) + Node("x1")))
            a, fa = eval_tree_array(tree, X, operators; turbo=true)
            b, fb = eval_tree_array(tree, X, operators; turbo=false)
            @test a ≈ b
            @test fa == fb

            # unary(binary(feature, const)) — deg1_l2_ll0_lr0_eval, feature+const
            tree = convert(Node{T}, f(Node("x1") + Node(; val=T(3))))
            a, fa = eval_tree_array(tree, X, operators; turbo=true)
            b, fb = eval_tree_array(tree, X, operators; turbo=false)
            @test a ≈ b
            @test fa == fb

            # unary(binary(feature, feature)) — deg1_l2_ll0_lr0_eval, feature+feature
            tree = convert(Node{T}, f(Node("x1") + Node("x1")))
            a, fa = eval_tree_array(tree, X, operators; turbo=true)
            b, fb = eval_tree_array(tree, X, operators; turbo=false)
            @test a ≈ b
            @test fa == fb

            # unary(feature) — deg1_l1_ll0_eval, feature branch
            tree = convert(Node{T}, f(Node("x1")))
            a, fa = eval_tree_array(tree, X, operators; turbo=true)
            b, fb = eval_tree_array(tree, X, operators; turbo=false)
            @test a ≈ b
            @test fa == fb
        end

        # With Inf input — tests the nonfinite guard path
        X_inf = fill(T(Inf), 1, 5)
        for f in (sin, log, sqrt)
            tree = convert(Node{T}, f(Node("x1") + Node(; val=T(1))))
            out, flag = eval_tree_array(tree, X_inf, operators; turbo=true)
            @test all(isnan, out)
            @test !flag
        end
    end
end
