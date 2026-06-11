@testitem "ArenaNode interface and evaluation" begin
    using Test
    using DynamicExpressions
    using DynamicExpressions: NodeInterface
    using Interfaces: Interfaces

    const AN = DynamicExpressions.ArenaNodeModule

    operators = OperatorEnum(1 => (sin, cos), 2 => (+, *))
    x1 = Node{Float64}(; feature=1)
    tree = sin(x1) + x1 * 3.2
    atree = convert(AN.ArenaNode{Float64}, tree)

    @test atree isa AN.ArenaNode{Float64,2}
    @test count_nodes(atree) == count_nodes(tree)
    @test string_tree(atree, operators) == string_tree(tree, operators)
    @test tree_mapreduce(_ -> 1, +, atree, Int) == count_nodes(atree)

    @test Interfaces.test(
        NodeInterface,
        AN.ArenaNode,
        [
            atree,
            convert(AN.ArenaNode{Float64}, sin(x1)),
            convert(AN.ArenaNode{Float64}, x1),
            convert(AN.ArenaNode{Float64}, Node{Float64}(; val=1.0)),
        ],
    )

    if atree.degree != 0
        cs = DynamicExpressions.NodeModule.unsafe_get_children(atree)
        @test cs isa NTuple{2,DynamicExpressions.Nullable{typeof(atree)}}
        @test length(get_children(atree, atree.degree)) == atree.degree
    end

    cursor = AN.ArenaCursor(atree; capacity=count_nodes(atree))
    seen = Int32[]
    AN.foreach_preorder!(n -> push!(seen, n.idx), atree, cursor)
    seen2 = Int32[]
    AN.foreach_preorder!(n -> push!(seen2, n.idx), atree, cursor)
    @test seen == seen2

    collected = collect(atree; break_sharing=Val(true))
    @test map(n -> n.idx, collected) == seen

    X = randn(Float64, 1, 50)
    y_tree, ok_tree = eval_tree_array(tree, X, operators)
    y_atree, ok_atree = eval_tree_array(atree, X, operators)
    @test ok_tree
    @test ok_atree
    @test y_tree ≈ y_atree

    const_nodes = filter(t -> t.degree == 0 && t.constant, atree)
    @test !isempty(const_nodes)
    const_nodes[1].val = 10.0
    y_mut, ok_mut = eval_tree_array(atree, X, operators)
    @test ok_mut
    @test !(y_mut ≈ y_tree)

    atree2 = copy(atree)
    @test atree2 == atree
    const_nodes2 = filter(t -> t.degree == 0 && t.constant, atree2)
    const_nodes2[1].val = -5.0
    @test atree2 != atree

    tree2 = convert(Node, atree)
    y_tree2, ok_tree2 = eval_tree_array(tree2, X, operators)
    @test ok_tree2
    @test y_tree2 ≈ y_mut
end

@testitem "ArenaNode mutation and simplification" begin
    using Test
    using DynamicExpressions

    const AN = DynamicExpressions.ArenaNodeModule

    operators = OperatorEnum(1 => (sin, cos), 2 => (+, *))
    x1 = Node{Float64}(; feature=1)
    tree = sin(x1) + x1 * 3.2
    X = randn(Float64, 1, 50)

    atree_setnode = convert(AN.ArenaNode{Float64}, tree)
    atree_setnode2 = copy(atree_setnode)
    set_node!(atree_setnode, atree_setnode2)
    @test string_tree(atree_setnode, operators) == string_tree(atree_setnode2, operators)

    parent = convert(AN.ArenaNode{Float64}, sin(x1))
    other = convert(AN.ArenaNode{Float64}, x1 * 3.2)
    set_child!(parent, other, 1)
    @test get_child(parent, 1).arena === parent.arena
    other.r.val = 99.0
    y_parent, ok_parent = eval_tree_array(parent, X, operators)
    @test ok_parent
    @test y_parent ≈ sin.(X[1, :] .* 3.2)

    tree_fold = Node{Float64}(; val=2.0) + Node{Float64}(; val=3.0)
    atree_fold = convert(AN.ArenaNode{Float64}, tree_fold)
    simplify_tree!(atree_fold, operators)
    @test atree_fold.degree == 0
    @test atree_fold.constant
    @test atree_fold.val == 5.0
end

@testitem "Expression with ArenaNode" begin
    using Test
    using DynamicExpressions
    using DynamicExpressions: ExpressionInterface, get_tree
    using Interfaces: test

    const AN = DynamicExpressions.ArenaNodeModule

    operators = OperatorEnum(1 => (sin, cos), 2 => (+, *))
    x1 = Node{Float64}(; feature=1)
    atree = convert(AN.ArenaNode{Float64}, sin(x1) + x1 * 3.2)
    expr = Expression(atree; operators, variable_names=["x"])
    @test get_tree(expr) === atree
    @test test(ExpressionInterface, Expression, [expr])

    simple_expr = Expression(
        convert(AN.ArenaNode{Float64}, x1); operators, variable_names=["x"]
    )
    @test test(ExpressionInterface, Expression, [simple_expr])
end

@testitem "ArenaNode derivatives through Expression" begin
    using Test
    using DynamicExpressions
    using DynamicExpressions: eval_grad_tree_array, extract_gradient
    using DifferentiationInterface: AutoZygote, gradient
    using Zygote

    const AN = DynamicExpressions.ArenaNodeModule

    operators_grad = OperatorEnum(1 => (sin, cos, exp), 2 => (+, -, *, /))
    x1 = Expression(
        convert(AN.ArenaNode{Float64}, Node{Float64}(; feature=1));
        operators=operators_grad,
        variable_names=[:x1, :x2],
    )
    x2 = Expression(
        convert(AN.ArenaNode{Float64}, Node{Float64}(; feature=2));
        operators=operators_grad,
        variable_names=[:x1, :x2],
    )
    expr_grad = sin(2.0 * x1 + exp(x2 + 5.0))

    Xg = rand(Float64, 2, 10) .+ 1
    expected = @. sin(2.0 * Xg[1, :] + exp(Xg[2, :] + 5.0))
    expected_dy_dx1 = @. 2.0 * cos(2.0 * Xg[1, :] + exp(Xg[2, :] + 5.0))

    result, ok = eval_tree_array(expr_grad, Xg)
    @test ok
    @test result ≈ expected

    _, grad2, ok2 = eval_grad_tree_array(expr_grad, Xg; variable=Val(true))
    @test ok2
    @test grad2[1, :] ≈ expected_dy_dx1

    grad_zygote = expr_grad'(Xg)
    @test grad_zygote[1, :] ≈ expected_dy_dx1

    operators_const = OperatorEnum(2 => (+,))
    x1c = Expression(
        convert(AN.ArenaNode{Float64}, Node{Float64}(; feature=1));
        operators=operators_const,
        variable_names=["x1"],
    )
    expr_const = x1c + 1.5
    _, grad3, ok3 = eval_grad_tree_array(expr_const, ones(1, 5); variable=Val(false))
    @test ok3
    @test grad3[1, :] ≈ fill(1.0, 5)

    d_ex = gradient(AutoZygote(), expr_const) do ex
        sum(ex(ones(1, 5)))
    end
    @test extract_gradient(d_ex, expr_const) ≈ [5.0]
end

@testitem "ArenaNode allocations" begin
    using PerformanceTestTools

    project_dir = dirname(Base.active_project())
    local_prefs = joinpath(project_dir, "LocalPreferences.toml")
    old_prefs = isfile(local_prefs) ? read(local_prefs, String) : nothing
    prefs_text = string(
        "[DynamicExpressions]\n", "dispatch_doctor_mode = ", repr("disable"), "\n"
    )

    try
        write(local_prefs, prefs_text)
        PerformanceTestTools.include_foreach(
            joinpath(@__DIR__, "test_arenanode_allocations.jl"),
            [Dict("JULIA_PKG_PRECOMPILE_AUTO" => "0")],
        )
    finally
        if old_prefs === nothing
            rm(local_prefs; force=true)
        else
            write(local_prefs, old_prefs)
        end
    end
end
