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
    res = optimize(Optim.NLSolversBase.only_fg!(my_fg!), tree, BFGS())

    @test did_i_run_2[]
    @test isapprox(
        first(get_scalar_constants(res.minimizer)),
        first(get_scalar_constants(target_tree));
        atol=0.01,
    )
end

@testitem "Wrap NLSolversBase.InplaceObjective hvp/fghvp/fjvp" begin
    using DynamicExpressions, Optim
    include("test_optim_setup.jl")

    ext = Base.get_extension(DynamicExpressions, :DynamicExpressionsOptimExt)

    tree = copy(original_tree)
    x0, refs = get_scalar_constants(tree)
    x = x0 .+ 1.234
    v = x0 .- 5.678

    did_fdf = Ref(false)
    did_hvp = Ref(false)
    did_fghvp = Ref(false)
    did_fjvp = Ref(false)

    seen_fdf_tree = Ref{Any}(nothing)
    seen_hvp_tree = Ref{Any}(nothing)
    seen_hvp_v = Ref{Any}(nothing)
    seen_fghvp_tree = Ref{Any}(nothing)
    seen_fghvp_v = Ref{Any}(nothing)
    seen_fjvp_tree = Ref{Any}(nothing)
    seen_fjvp_v = Ref{Any}(nothing)

    my_fdf!(F, t) = (did_fdf[] = true; seen_fdf_tree[] = t; return nothing)
    my_hvp!(HV, t, vin) = (
        did_hvp[] = true; seen_hvp_tree[] = t; seen_hvp_v[] = vin; return nothing
    )
    my_fghvp!(F, G, t, vin) = (
        did_fghvp[] = true; seen_fghvp_tree[] = t; seen_fghvp_v[] = vin; return nothing
    )
    my_fjvp!(F, J, t, vin) = (
        did_fjvp[] = true; seen_fjvp_tree[] = t; seen_fjvp_v[] = vin; return nothing
    )

    # Construct an InplaceObjective regardless of the exact field set/order.
    fields = fieldnames(Optim.NLSolversBase.InplaceObjective)
    vals = map(fields) do name
        if name == :fdf
            my_fdf!
        elseif name == :hvp || name == :hv
            my_hvp!
        elseif name == :fghvp || name == :fghv
            my_fghvp!
        elseif name == :fjvp
            my_fjvp!
        else
            nothing
        end
    end
    obj = Optim.NLSolversBase.InplaceObjective(vals...)
    wrapped = ext.wrap_func(obj, tree, refs)

    if :fdf in fields
        wrapped.fdf(nothing, x)
        @test did_fdf[]
        @test seen_fdf_tree[] === tree
        @test first(get_scalar_constants(tree)) == x
    end

    if (:hvp in fields) || (:hv in fields)
        getfield(wrapped, (:hvp in fields ? :hvp : :hv))(nothing, x, v)
        @test did_hvp[]
        @test seen_hvp_tree[] === tree
        @test seen_hvp_v[] === v
        @test first(get_scalar_constants(tree)) == x
    end

    if (:fghvp in fields) || (:fghv in fields)
        getfield(wrapped, (:fghvp in fields ? :fghvp : :fghv))(nothing, nothing, x, v)
        @test did_fghvp[]
        @test seen_fghvp_tree[] === tree
        @test seen_fghvp_v[] === v
        @test first(get_scalar_constants(tree)) == x
    end

    if :fjvp in fields
        wrapped.fjvp(nothing, nothing, x, v)
        @test did_fjvp[]
        @test seen_fjvp_tree[] === tree
        @test seen_fjvp_v[] === v
        @test first(get_scalar_constants(tree)) == x
    end
end

@testitem "Wrap objective (..., x, v): error on wrong arity" begin
    using DynamicExpressions, Optim
    include("test_optim_setup.jl")

    ext = Base.get_extension(DynamicExpressions, :DynamicExpressionsOptimExt)

    tree = copy(original_tree)
    x0, refs = get_scalar_constants(tree)

    wrapped = ext._wrap_objective_xv_tail((args...) -> nothing, tree, refs)
    @test_throws ArgumentError wrapped(x0)
end

@testitem "Wrap InplaceObjective: error on unknown field" begin
    using DynamicExpressions, Optim
    include("test_optim_setup.jl")

    ext = Base.get_extension(DynamicExpressions, :DynamicExpressionsOptimExt)

    tree = copy(original_tree)
    _, refs = get_scalar_constants(tree)

    # Construct a dummy InplaceObjective for the *installed* NLSolversBase/Optim
    # layout, then directly hit the internal error branch.
    fields = fieldnames(Optim.NLSolversBase.InplaceObjective)
    dummy = (args...) -> nothing
    obj = Optim.NLSolversBase.InplaceObjective((dummy for _ in fields)...)

    spec = if fields == ext._INPLACEOBJECTIVE_SPEC_V8.field_syms
        ext._INPLACEOBJECTIVE_SPEC_V8
    elseif fields == ext._INPLACEOBJECTIVE_SPEC_V7.field_syms
        ext._INPLACEOBJECTIVE_SPEC_V7
    else
        # Should be unreachable for supported Optim majors, but pick one so the
        # test still compiles if a future layout appears.
        ext._INPLACEOBJECTIVE_SPEC_V8
    end

    @test_throws ArgumentError ext._wrap_inplaceobjective_field(
        Val(:__unknown_field__), obj, tree, refs, spec
    )
end
