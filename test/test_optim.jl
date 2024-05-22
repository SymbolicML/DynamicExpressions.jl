using DynamicExpressions, Optim, Zygote
using Random: MersenneTwister as RNG
using Test

operators = OperatorEnum(; binary_operators=(+, -, *, /), unary_operators=(exp,))
x1, x2 = (i -> Node(Float64; feature=i)).(1:2)

X = rand(RNG(0), Float64, 2, 100)
y = @. exp(X[1, :] * 2.1 - 0.9) + X[2, :] * -0.9

original_tree = exp(x1 * 0.8 - 0.0) + 5.2 * x2
target_tree = exp(x1 * 2.1 - 0.9) + -0.9 * x2

f(tree) = sum(abs2, tree(X, operators) .- y)
function g!(G, tree)
    dy = only(gradient(f, tree))
    G .= dy.gradient
    return nothing
end

@testset "Basic optimization" begin
    tree = copy(original_tree)
    res = optimize(f, tree)

    # Should be unchanged by default
    if VERSION >= v"1.9"
        ext = Base.get_extension(DynamicExpressions, :DynamicExpressionsOptimExt)
        @test res isa ext.ExpressionOptimizationResults
    end
    @test tree == original_tree
    @test isapprox(get_constants(res.minimizer), get_constants(target_tree); atol=0.01)
end

@testset "With gradients, using Zygote" begin
    tree = copy(original_tree)
    res = optimize(f, g!, tree, BFGS())
    @test tree == original_tree
    @test isapprox(get_constants(res.minimizer), get_constants(target_tree); atol=0.01)
end

@testset "With gradients, manually" begin
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
    @test isapprox(get_constants(res.minimizer), get_constants(target_tree); atol=0.01)
    @test Optim.minimizer(res) === res.minimizer
    @test propertynames(res) == (:tree, propertynames(getfield(res, :_results))...)

    @testset "Hessians not implemented" begin
        @test_throws ArgumentError optimize(f, g!, t -> t, tree, BFGS())
        VERSION >= v"1.9" && @test_throws(
            "Optim.optimize does not yet support Hessians on `AbstractExpressionNode`",
            optimize(f, g!, t -> t, tree, BFGS())
        )
    end
end

# Now, try combined
@testset "Combined evaluation with gradient" begin
    tree = copy(original_tree)
    did_i_run_2 = Ref(false)
    fg!(F, G, tree) =
        let
            if G !== nothing
                ŷ, dŷ_dconstants, _ = eval_grad_tree_array(
                    tree, X, operators; variable=false
                )
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
    res = optimize(Optim.only_fg!(fg!), tree, BFGS())

    @test did_i_run_2[]
    @test isapprox(
        first(get_constants(res.minimizer)), first(get_constants(target_tree)); atol=0.01
    )
end
