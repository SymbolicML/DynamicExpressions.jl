@testitem "Basic optimization" begin
    using DynamicExpressions, Optim

    include("test_optim_setup.jl")
    tree = copy(original_tree)
    res = optimize(f, tree)

    # Should be unchanged by default
    ext = Base.get_extension(DynamicExpressions, :DynamicExpressionsOptimExt)
    @test res isa ext.ExpressionOptimizationResults
    @test tree == original_tree
    @test isapprox(
        first(get_scalar_constants(res.minimizer)),
        first(get_scalar_constants(target_tree));
        atol=0.01,
    )
end

@testitem "With gradients, using Zygote" begin
    using DynamicExpressions, Optim, Zygote

    include("test_optim_setup.jl")

    tree = copy(original_tree)
    res = optimize(f, g!, tree, BFGS())
    @test tree == original_tree
    @test isapprox(
        first(get_scalar_constants(res.minimizer)),
        first(get_scalar_constants(target_tree));
        atol=0.01,
    )
end

@testitem "With gradients, manually" begin
    using DynamicExpressions, Optim, Zygote

    include("test_optim_setup.jl")

    tree = copy(original_tree)
    did_i_run = Ref(false)
    # Now, try with gradients too (via Zygote and our hand-rolled forward-mode AD)
    g!(G, tree) =
        let
            ŷ, dŷ_dconstants, _ = eval_grad_tree_array(tree, X, operators; variable=false)
            dresult_dŷ = @. 2 * (ŷ - y)
            for i in eachindex(G)
                G[i] = sum(
                    j -> dresult_dŷ[j] * dŷ_dconstants[i, j],
                    eachindex(axes(dŷ_dconstants, 2), axes(dresult_dŷ, 1)),
                )
            end
            did_i_run[] = true
            return nothing
        end

    res = optimize(f, g!, tree, BFGS())
    @test did_i_run[]
    @test res.f_calls > 0
    @test isapprox(
        first(get_scalar_constants(res.minimizer)),
        first(get_scalar_constants(target_tree));
        atol=0.01,
    )
    @test Optim.minimizer(res) === res.minimizer
    @test propertynames(res) == (:tree, propertynames(getfield(res, :_results))...)

    @testset "Hessians not implemented" begin
        @test_throws ArgumentError optimize(f, g!, t -> t, tree, BFGS())
        @test_throws(
            "Optim.optimize does not yet support Hessians on `AbstractExpressionNode`",
            optimize(f, g!, t -> t, tree, BFGS())
        )
    end
end

# Now, try combined
@testitem "Combined evaluation with gradient" begin
    using DynamicExpressions, Optim, Zygote
    include("test_optim_setup.jl")

    tree = copy(original_tree)
    did_i_run_2 = Ref(false)
    function my_fg!(F, G, tree)
        if G !== nothing
            ŷ, dŷ_dconstants, _ = eval_grad_tree_array(tree, X, operators; variable=false)
            dresult_dŷ = @. 2 * (ŷ - y)
            for i in eachindex(G)
                G[i] = sum(
                    j -> dresult_dŷ[j] * dŷ_dconstants[i, j],
                    eachindex(axes(dŷ_dconstants, 2), axes(dresult_dŷ, 1)),
                )
            end
            if F !== nothing
                did_i_run_2[] = true
                return sum(abs2, ŷ .- y)
            end
        elseif F !== nothing
            # Only f
            return sum(abs2, tree(X, operators) .- y)
        end
    end
    res = optimize(Optim.only_fg!(my_fg!), tree, BFGS())

    @test did_i_run_2[]
    @test isapprox(
        first(get_scalar_constants(res.minimizer)),
        first(get_scalar_constants(target_tree));
        atol=0.01,
    )
end
