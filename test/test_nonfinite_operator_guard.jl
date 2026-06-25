@testitem "nonfinite operator guard" begin
    using DynamicExpressions
    using DynamicExpressions.ValueInterfaceModule: can_eval_nonfinite
    using LoopVectorization
    using Test

    @test !can_eval_nonfinite(sin)
    @test can_eval_nonfinite(exp)
    @test can_eval_nonfinite(+)
    @test !can_eval_nonfinite(log)
    @test !can_eval_nonfinite(sqrt)
    f(x) = x
    @test !can_eval_nonfinite(f)

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
