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
        @test get_child(tree, UInt8(1)) == get_child(tree, 1)
        @test get_child(atree, UInt8(1)) == get_child(atree, 1)
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

    default = AN.ArenaNode{Float64,2}()
    @test default.degree == 0
    @test default.constant
    @test default.val == 0.0
    default.val = 2
    default.feature = 1
    default.op = 2
    default.constant = false
    default.degree = 0
    @test default.val == 2.0
    @test default.feature == 1
    @test default.op == 2
    @test !default.constant
    @test_throws ArgumentError default.foo = 1

    parent = convert(AN.ArenaNode{Float64}, sin(x1))
    other = convert(AN.ArenaNode{Float64}, x1 * 3.2)
    set_child!(parent, other, 1)
    @test get_child(parent, 1).arena === parent.arena
    other.r.val = 99.0
    y_parent, ok_parent = eval_tree_array(parent, X, operators)
    @test ok_parent
    @test y_parent ≈ sin.(X[1, :] .* 3.2)

    @test_throws ArgumentError set_child!(parent, Node{Float32}(; val=1.0f0), 1)
    @test_throws UndefRefError get_child(convert(AN.ArenaNode{Float64}, x1), 1)

    rewritten = convert(AN.ArenaNode{Float64}, sin(x1))
    set_children!(rewritten, (convert(AN.ArenaNode{Float64}, x1 * 2.0),))
    @test get_child(rewritten, 1).arena === rewritten.arena
    @test string_tree(rewritten, operators) == "sin(x1 * 2.0)"

    bad_children = (
        DynamicExpressions.Nullable(true, Node{Float64}(; val=0.0)),
        Node{Float32}(; val=1.0f0),
    )
    @test_throws ArgumentError set_children!(rewritten, bad_children)

    tree_fold = Node{Float64}(; val=2.0) + Node{Float64}(; val=3.0)
    atree_fold = convert(AN.ArenaNode{Float64}, tree_fold)
    simplify_tree!(atree_fold, operators)
    @test atree_fold.degree == 0
    @test atree_fold.constant
    @test atree_fold.val == 5.0

    other_cursor = AN.ArenaCursor(convert(AN.ArenaNode{Float64}, x1))
    @test_throws ArgumentError AN.foreach_preorder!(identity, atree_fold, other_cursor)
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
        return sum(ex(ones(1, 5)))
    end
    @test extract_gradient(d_ex, expr_const) ≈ [5.0]
end

@testitem "ArenaNode allocations" begin
    include(joinpath(@__DIR__, "test_arenanode_allocations.jl"))
end

@testitem "ArenaNode flat copy and whole-tree fast paths" begin
    using DynamicExpressions
    using DynamicExpressions: Node, copy_node
    using DynamicExpressions.NodePreallocationModule: allocate_container, copy_into!

    const AN = DynamicExpressions.ArenaNodeModule

    operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[sin, cos])
    x1 = Node{Float64}(; feature=1)
    x2 = Node{Float64}(; feature=2)
    tree = sin(x1 * 3.2 - 0.9) + x2 * (x1 - 0.5)
    atree = convert(AN.ArenaNode{Float64}, tree)

    @testset "compact flat copy" begin
        @test AN.is_compact_root(atree)
        c = copy(atree)
        @test c.arena !== atree.arena
        @test convert(Node, c) == tree
        c.l.l.r.val = 99.0
        @test convert(Node, atree) == tree
        @test convert(Node, c) != tree
    end

    @testset "subtree copy falls back and re-compacts" begin
        sub = atree.l
        @test !AN.is_compact_root(sub)
        csub = copy(sub)
        @test AN.is_compact_root(csub)
        @test convert(Node, csub) == tree.l
    end

    @testset "structural mutation invalidates fast paths" begin
        mutated = convert(AN.ArenaNode{Float64}, tree)
        set_child!(mutated, convert(AN.ArenaNode{Float64}, cos(x2)), 2)
        @test !mutated.arena.compact[]
        expected = copy(tree)
        set_child!(expected, cos(x2), 2)
        @test convert(Node, mutated) == expected
        @test count_nodes(mutated) == count_nodes(expected)
        @test has_constants(mutated) == has_constants(expected)
        recompacted = copy(mutated)
        @test AN.is_compact_root(recompacted)
        @test count_nodes(recompacted) == count_nodes(expected)

        leafed = convert(AN.ArenaNode{Float64}, tree)
        node = leafed.r
        node.degree = 0
        node.constant = true
        node.val = 1.0
        @test !leafed.arena.compact[]
        expected2 = copy(tree)
        expected2.r = Node{Float64}(; val=1.0)
        @test convert(Node, leafed) == expected2
        @test count_nodes(leafed) == count_nodes(expected2)
        @test has_constants(leafed) == has_constants(expected2)
    end

    @testset "preallocated copy_into!" begin
        dest = allocate_container(atree)
        out = copy_into!(dest, atree)
        @test out.arena === dest
        @test convert(Node, out) == tree
        out2 = copy_into!(dest, atree)
        @test convert(Node, out2) == tree
    end

    @testset "copy_node entry point" begin
        c = copy_node(atree)
        @test c.arena !== atree.arena
        @test convert(Node, c) == tree
    end

    @testset "Expression-level preallocated copy (SR mutation path)" begin
        ex = Expression(
            convert(AN.ArenaNode{Float64}, tree);
            operators=operators,
            variable_names=["x1", "x2"],
        )
        container = allocate_container(ex)
        ex2 = copy_into!(container, ex)
        @test convert(Node, DynamicExpressions.get_tree(ex2)) == tree
    end

    @testset "whole-tree scans match Node" begin
        @test count_nodes(atree) == count_nodes(tree)
        @test count(t -> t.degree == 2, atree) == count(t -> t.degree == 2, tree)
        @test count(t -> t.degree == 0, atree.l) == count(t -> t.degree == 0, tree.l)
        @test length(atree) == length(tree)
        @test count_constant_nodes(atree) == count_constant_nodes(tree)
        @test has_constants(atree) == has_constants(tree)
        leaf = convert(AN.ArenaNode{Float64}, Node{Float64}(; feature=1))
        @test !has_constants(leaf)
        @test count_constant_nodes(leaf) == 0
    end

    @testset "scalar constants via arena indices" begin
        fresh = convert(AN.ArenaNode{Float64}, tree)
        vals, refs = get_scalar_constants(fresh)
        @test refs isa Vector{Int32}
        @test DynamicExpressions.count_scalar_constants(fresh) == length(vals)
        @test vals == first(get_scalar_constants(tree))
        set_scalar_constants!(fresh, vals .* 2, refs)
        @test first(get_scalar_constants(fresh)) == vals .* 2

        # Indices remain valid in flat copies of the tree:
        c = copy(fresh)
        set_scalar_constants!(c, vals, refs)
        @test first(get_scalar_constants(c)) == vals

        # Non-compact trees also use arena-index refs:
        sub = fresh.l
        vsub, rsub = get_scalar_constants(sub)
        @test rsub isa Vector{Int32}
        @test vsub == first(get_scalar_constants(convert(Node, sub)))
        set_scalar_constants!(sub, vsub .+ 1, rsub)
        @test first(get_scalar_constants(sub)) == vsub .+ 1
    end
end

@testitem "Arena array interface guards compactness automatically" begin
    using DynamicExpressions
    using DynamicExpressions: Node

    const AN = DynamicExpressions.ArenaNodeModule

    operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])
    x1 = Node{Float64}(; feature=1)
    tree = sin(x1 * 3.2) + 0.5
    atree = convert(AN.ArenaNode{Float64}, tree)
    a = atree.arena
    @test a isa AbstractVector{AN.ArenaEntry{Float64,2}}
    @test length(a) == count_nodes(tree)

    e = a[1]
    a[1] = AN._replace(e; val=42.0)
    @test AN.is_compact_root(atree)

    vals, refs = get_scalar_constants(atree)
    set_scalar_constants!(atree, vals .* 2, refs)
    @test AN.is_compact_root(atree)

    bi = findfirst(e -> e.degree == 0x02, collect(a))
    e = a[bi]
    a[bi] = AN._replace(e; degree=0x00)
    @test !a.compact[]

    b = convert(AN.ArenaNode{Float64}, tree).arena
    scrambled = reverse(collect(b))
    copyto!(b, scrambled)
    @test !b.compact[]

    c = convert(AN.ArenaNode{Float64}, tree).arena
    copyto!(c, collect(c))
    @test c.compact[]
end

@testitem "ArenaNode fast paths agree with Node under random mutations" begin
    using DynamicExpressions
    using DynamicExpressions: Node
    using Random

    const AN = DynamicExpressions.ArenaNodeModule

    operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[sin, cos])
    rng = MersenneTwister(42)

    random_leaf(rng) =
        if rand(rng) < 0.5
            Node{Float64}(; val=randn(rng))
        else
            Node{Float64}(; feature=rand(rng, 1:3))
        end

    function random_tree(rng, n)
        tree = random_leaf(rng)
        while count_nodes(tree) < n
            leaf = rand(rng, filter(t -> t.degree == 0, tree))
            if rand(rng) < 0.3
                leaf.degree = 1
                leaf.op = rand(rng, 1:2)
                leaf.l = random_leaf(rng)
            else
                leaf.degree = 2
                leaf.op = rand(rng, 1:4)
                leaf.l = random_leaf(rng)
                leaf.r = random_leaf(rng)
            end
            leaf.constant = false
            leaf.val = 0.0
        end
        return tree
    end

    function all_facades(n, acc=AN.ArenaNode{Float64,2}[])
        push!(acc, n)
        for i in 1:(n.degree)
            all_facades(get_child(n, i), acc)
        end
        return acc
    end

    for _ in 1:20
        root = convert(AN.ArenaNode{Float64}, random_tree(rng, rand(rng, 5:25)))
        for _ in 1:8
            nodes = all_facades(root)
            node = rand(rng, nodes)
            choice = rand(rng, 1:4)
            if choice == 1 && node.degree == 0
                if node.constant
                    node.val = randn(rng)
                else
                    node.feature = rand(rng, 1:3)
                end
            elseif choice == 2 && node.degree > 0
                node.op = rand(rng, 1:(node.degree == 1 ? 2 : 4))
            elseif choice == 3 && node.degree > 0
                set_child!(
                    node,
                    convert(AN.ArenaNode{Float64}, random_tree(rng, rand(rng, 1:5))),
                    rand(rng, 1:(node.degree)),
                )
            else
                node.degree = 0
                node.constant = true
                node.val = randn(rng)
            end

            expected = convert(Node, root)
            @test count_nodes(root) == count_nodes(expected)
            @test count_constant_nodes(root) == count_constant_nodes(expected)
            @test has_constants(root) == has_constants(expected)
            @test DynamicExpressions.NodeUtilsModule.is_constant(root) ==
                DynamicExpressions.NodeUtilsModule.is_constant(expected)
            @test count(t -> t.degree == 2, root) == count(t -> t.degree == 2, expected)
            @test first(get_scalar_constants(root)) == first(get_scalar_constants(expected))
            c = copy(root)
            @test AN.is_compact_root(c)
            @test convert(Node, c) == expected
        end
    end
end

@testitem "ArenaNode buffered plan evaluation matches generic evaluator" begin
    using DynamicExpressions
    using DynamicExpressions: Node, EvalOptions, ArrayBuffer
    using Random

    const AN = DynamicExpressions.ArenaNodeModule

    operators = OperatorEnum(; binary_operators=[+, *, /, -], unary_operators=[cos, exp])
    rng = MersenneTwister(11)

    for T in (Float32, Float64)
        random_leaf(rng) =
            if rand(rng) < 0.4
                Node{T}(; val=randn(rng, T))
            else
                Node{T}(; feature=rand(rng, 1:5))
            end
        function random_tree(rng, n)
            tree = random_leaf(rng)
            while count_nodes(tree) < n
                leaf = rand(rng, filter(t -> t.degree == 0, tree))
                if rand(rng) < 0.3
                    leaf.degree = 1
                    leaf.op = rand(rng, 1:2)
                    leaf.l = random_leaf(rng)
                else
                    leaf.degree = 2
                    leaf.op = rand(rng, 1:4)
                    leaf.l = random_leaf(rng)
                    leaf.r = random_leaf(rng)
                end
                leaf.constant = false
                leaf.val = zero(T)
            end
            return tree
        end

        X = randn(rng, T, 5, 37)
        buf_a = zeros(T, 40, 37)
        buf_n = zeros(T, 40, 37)
        for trial in 1:60, early_exit in (true, false)
            tree = random_tree(rng, rand(rng, 1:30))
            atree = convert(AN.ArenaNode{T}, tree)
            opts_a = EvalOptions(; early_exit, buffer=ArrayBuffer(buf_a, Ref(0)))
            opts_n = EvalOptions(; early_exit, buffer=ArrayBuffer(buf_n, Ref(0)))
            # Unbuffered ground truth; results of buffered evals are views, so
            # copy before they can alias each other.
            rt = try
                (
                    eval_tree_array(
                        tree, X, operators; eval_options=EvalOptions(; early_exit)
                    ),
                    false,
                )
            catch
                (nothing, true)
            end
            ra = try
                (eval_tree_array(atree, X, operators; eval_options=opts_a), false)
            catch
                (nothing, true)
            end
            @test rt[2] == ra[2]
            (rt[2] || ra[2]) && continue
            (yref, okref) = rt[1]
            (ya, oka) = ra[1]
            @test okref == oka
            if okref
                @test yref ≈ ya || (any(!isfinite, yref) && any(!isfinite, ya))
            end
            # buffered Node evaluation agrees too
            (yn, okn) = eval_tree_array(tree, X, operators; eval_options=opts_n)
            @test okn == okref
        end

        # Stacks deeper than 64 take the generic path (with the same buffer):
        deep = Node{T}(; val=T(0.5))
        for _ in 1:70
            deep = Node{T}(; op=1, l=Node{T}(; feature=1), r=deep)
        end
        adeep = convert(AN.ArenaNode{T}, deep)
        big = zeros(T, 80, 37)
        o = EvalOptions(; buffer=ArrayBuffer(big, Ref(0)))
        y1, ok1 = eval_tree_array(copy(deep), X, operators)
        y2, ok2 = eval_tree_array(adeep, X, operators; eval_options=o)
        @test ok1 == ok2
        ok1 && @test y1 ≈ y2
    end
end
