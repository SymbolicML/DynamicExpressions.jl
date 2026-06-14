@testmodule ArenaTreeGen begin
    using DynamicExpressions
    using Random: AbstractRNG

    export random_tree

    function random_leaf(rng, ::Type{T}, ::Val{D}, nfeat, const_p) where {T,D}
        if rand(rng) < const_p
            return Node{T,D}(; val=randn(rng, T))
        else
            return Node{T,D}(; feature=rand(rng, 1:nfeat))
        end
    end

    """Grow a random `Node{T,D}` tree to `n` nodes. `arity_cdf[d]` is the
    cumulative probability of expanding a leaf to degree `d`; `nops[d]` is the
    number of degree-`d` operators in the testitem's `OperatorEnum`."""
    function random_tree(
        rng::AbstractRNG,
        n;
        T=Float64,
        D=2,
        nfeat=3,
        nops=(2, 4),
        arity_cdf=(0.3, 1.0),
        const_p=0.5,
    )
        leaf() = random_leaf(rng, T, Val(D), nfeat, const_p)
        tree = leaf()
        while count_nodes(tree) < n
            node = rand(rng, filter(t -> t.degree == 0, tree))
            r = rand(rng)
            d = something(findfirst(>=(r), arity_cdf), D)
            node.degree = d
            node.op = rand(rng, 1:nops[d])
            set_children!(node, ntuple(_ -> leaf(), d))
            node.constant = false
            node.val = zero(T)
        end
        return tree
    end
end

@testitem "ArenaNode interface and evaluation" begin
    using Test
    using DynamicExpressions
    using DynamicExpressions: NodeInterface
    using Interfaces: Interfaces

    using DynamicExpressions: ArenaNode, Arena
    using DynamicExpressions.ArenaNodeModule:
        ArenaEntry, _replace, is_compact_root, push_constant!, push_feature!

    operators = OperatorEnum(1 => (sin, cos), 2 => (+, *))
    x1 = Node{Float64}(; feature=1)
    tree = sin(x1) + x1 * 3.2
    atree = convert(ArenaNode{Float64}, tree)

    @test atree isa ArenaNode{Float64,2}
    @test count_nodes(atree) == count_nodes(tree)
    @test string_tree(atree, operators) == string_tree(tree, operators)
    @test tree_mapreduce(_ -> 1, +, atree, Int) == count_nodes(atree)

    @test Interfaces.test(
        NodeInterface,
        ArenaNode,
        [
            atree,
            convert(ArenaNode{Float64}, sin(x1)),
            convert(ArenaNode{Float64}, x1),
            convert(ArenaNode{Float64}, Node{Float64}(; val=1.0)),
        ],
    )

    if atree.degree != 0
        cs = DynamicExpressions.NodeModule.unsafe_get_children(atree)
        @test cs isa NTuple{2,DynamicExpressions.Nullable{typeof(atree)}}
        @test length(get_children(atree, atree.degree)) == atree.degree
        @test get_child(tree, UInt8(1)) == get_child(tree, 1)
        @test get_child(atree, UInt8(1)) == get_child(atree, 1)
    end

    collected = collect(atree; break_sharing=Val(true))
    @test !isempty(collected) && collected[1].idx == atree.idx

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

    using DynamicExpressions: ArenaNode, Arena
    using DynamicExpressions.ArenaNodeModule:
        ArenaEntry, _replace, is_compact_root, push_constant!, push_feature!

    operators = OperatorEnum(1 => (sin, cos), 2 => (+, *))
    x1 = Node{Float64}(; feature=1)
    tree = sin(x1) + x1 * 3.2
    X = randn(Float64, 1, 50)

    atree_setnode = convert(ArenaNode{Float64}, tree)
    atree_setnode2 = copy(atree_setnode)
    set_node!(atree_setnode, atree_setnode2)
    @test string_tree(atree_setnode, operators) == string_tree(atree_setnode2, operators)

    default = ArenaNode{Float64,2}()
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

    parent = convert(ArenaNode{Float64}, sin(x1))
    other = convert(ArenaNode{Float64}, x1 * 3.2)
    set_child!(parent, other, 1)
    @test get_child(parent, 1).arena === parent.arena
    other.r.val = 99.0
    y_parent, ok_parent = eval_tree_array(parent, X, operators)
    @test ok_parent
    @test y_parent ≈ sin.(X[1, :] .* 3.2)

    guarded = convert(ArenaNode{Float64}, sin(x1))
    foreign_child = convert(ArenaNode{Float64}, x1 * 3.2)
    n_before = length(guarded.arena.nodes)
    @test_throws BoundsError set_child!(guarded, foreign_child, 3)
    @test length(guarded.arena.nodes) == n_before

    @test_throws ArgumentError set_child!(parent, Node{Float32}(; val=1.0f0), 1)
    @test_throws UndefRefError get_child(convert(ArenaNode{Float64}, x1), 1)

    rewritten = convert(ArenaNode{Float64}, sin(x1))
    set_children!(rewritten, (convert(ArenaNode{Float64}, x1 * 2.0),))
    @test get_child(rewritten, 1).arena === rewritten.arena
    @test string_tree(rewritten, operators) == "sin(x1 * 2.0)"

    bad_children = (
        DynamicExpressions.Nullable(true, Node{Float64}(; val=0.0)),
        Node{Float32}(; val=1.0f0),
    )
    @test_throws ArgumentError set_children!(rewritten, bad_children)

    tree_fold = Node{Float64}(; val=2.0) + Node{Float64}(; val=3.0)
    atree_fold = convert(ArenaNode{Float64}, tree_fold)
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

    using DynamicExpressions: ArenaNode, Arena
    using DynamicExpressions.ArenaNodeModule:
        ArenaEntry, _replace, is_compact_root, push_constant!, push_feature!

    operators = OperatorEnum(1 => (sin, cos), 2 => (+, *))
    x1 = Node{Float64}(; feature=1)
    atree = convert(ArenaNode{Float64}, sin(x1) + x1 * 3.2)
    expr = Expression(atree; operators, variable_names=["x"])
    @test get_tree(expr) === atree
    @test test(ExpressionInterface, Expression, [expr])

    simple_expr = Expression(
        convert(ArenaNode{Float64}, x1); operators, variable_names=["x"]
    )
    @test test(ExpressionInterface, Expression, [simple_expr])
end

@testitem "ArenaNode derivatives through Expression" begin
    using Test
    using DynamicExpressions
    using DynamicExpressions: eval_grad_tree_array, extract_gradient
    using DifferentiationInterface: AutoZygote, gradient
    using Zygote

    using DynamicExpressions: ArenaNode, Arena
    using DynamicExpressions.ArenaNodeModule:
        ArenaEntry, _replace, is_compact_root, push_constant!, push_feature!

    operators_grad = OperatorEnum(1 => (sin, cos, exp), 2 => (+, -, *, /))
    x1 = Expression(
        convert(ArenaNode{Float64}, Node{Float64}(; feature=1));
        operators=operators_grad,
        variable_names=[:x1, :x2],
    )
    x2 = Expression(
        convert(ArenaNode{Float64}, Node{Float64}(; feature=2));
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
        convert(ArenaNode{Float64}, Node{Float64}(; feature=1));
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

    using DynamicExpressions: ArenaNode, Arena
    using DynamicExpressions.ArenaNodeModule:
        ArenaEntry, _replace, is_compact_root, push_constant!, push_feature!

    operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[sin, cos])
    x1 = Node{Float64}(; feature=1)
    x2 = Node{Float64}(; feature=2)
    tree = sin(x1 * 3.2 - 0.9) + x2 * (x1 - 0.5)
    atree = convert(ArenaNode{Float64}, tree)

    @testset "compact flat copy" begin
        @test is_compact_root(atree)
        c = copy(atree)
        @test c.arena !== atree.arena
        @test convert(Node, c) == tree
        c.l.l.r.val = 99.0
        @test convert(Node, atree) == tree
        @test convert(Node, c) != tree
    end

    @testset "subtree copy falls back and re-compacts" begin
        sub = atree.l
        @test !is_compact_root(sub)
        csub = copy(sub)
        @test is_compact_root(csub)
        @test convert(Node, csub) == tree.l
    end

    @testset "structural mutation invalidates fast paths" begin
        mutated = convert(ArenaNode{Float64}, tree)
        set_child!(mutated, convert(ArenaNode{Float64}, cos(x2)), 2)
        @test !mutated.arena.compact[]
        expected = copy(tree)
        set_child!(expected, cos(x2), 2)
        @test convert(Node, mutated) == expected
        @test count_nodes(mutated) == count_nodes(expected)
        @test has_constants(mutated) == has_constants(expected)
        recompacted = copy(mutated)
        @test is_compact_root(recompacted)
        @test count_nodes(recompacted) == count_nodes(expected)

        leafed = convert(ArenaNode{Float64}, tree)
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
        ref = Ref(-1)
        out = copy_into!(dest, atree; ref)
        @test out.arena === dest
        @test convert(Node, out) == tree
        @test ref[] == length(atree)
        out2 = copy_into!(dest, atree; ref)
        @test convert(Node, out2) == tree
        @test ref[] == length(atree)

        same_ref = Ref(-1)
        same = copy_into!(out2.arena, out2; ref=same_ref)
        @test same === out2
        @test same_ref[] == length(out2)
    end

    @testset "copy_node entry point" begin
        c = copy_node(atree)
        @test c.arena !== atree.arena
        @test convert(Node, c) == tree
    end

    @testset "Expression-level preallocated copy (SR mutation path)" begin
        ex = Expression(
            convert(ArenaNode{Float64}, tree);
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
        leaf = convert(ArenaNode{Float64}, Node{Float64}(; feature=1))
        @test !has_constants(leaf)
        @test count_constant_nodes(leaf) == 0
    end

    @testset "scalar constants via arena indices" begin
        fresh = convert(ArenaNode{Float64}, tree)
        vals, refs = get_scalar_constants(fresh)
        @test refs isa Vector{Int32}
        @test DynamicExpressions.count_scalar_constants(fresh) == length(vals)
        @test vals == first(get_scalar_constants(tree))
        @test set_scalar_constants!(fresh, vals .* 2, refs) === fresh
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

@testitem "Arena array interface guards compactness" begin
    using DynamicExpressions
    using DynamicExpressions: Node

    using DynamicExpressions: ArenaNode, Arena
    using DynamicExpressions.ArenaNodeModule:
        ArenaEntry, _replace, is_compact_root, push_constant!, push_feature!

    operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])
    x1 = Node{Float64}(; feature=1)
    tree = sin(x1 * 3.2) + 0.5
    atree = convert(ArenaNode{Float64}, tree)
    a = atree.arena
    @test a isa AbstractVector{ArenaEntry{Float64,2}}
    @test length(a) == count_nodes(tree)

    e = a[1]
    a[1] = _replace(e; val=42.0)
    @test is_compact_root(atree)

    vals, refs = get_scalar_constants(atree)
    set_scalar_constants!(atree, vals .* 2, refs)
    @test is_compact_root(atree)

    bi = findfirst(e -> e.degree == 0x02, collect(a))
    e = a[bi]
    a[bi] = _replace(e; degree=0x00)
    @test !a.compact[]

    b = convert(ArenaNode{Float64}, tree).arena
    scrambled = reverse(collect(b))
    copyto!(b, scrambled)
    @test !b.compact[]

    c = convert(ArenaNode{Float64}, tree).arena
    copyto!(c, collect(c))
    @test c.compact[]
end

@testitem "ArenaNode fast paths match Node" setup = [ArenaTreeGen] begin
    using DynamicExpressions
    using DynamicExpressions: Node
    using Random

    using DynamicExpressions: ArenaNode, Arena
    using DynamicExpressions.ArenaNodeModule:
        ArenaEntry, _replace, is_compact_root, push_constant!, push_feature!

    operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[sin, cos])
    rng = MersenneTwister(42)

    function all_facades(n, acc=ArenaNode{Float64,2}[])
        push!(acc, n)
        for i in 1:(n.degree)
            all_facades(get_child(n, i), acc)
        end
        return acc
    end

    for _ in 1:20
        root = convert(ArenaNode{Float64}, ArenaTreeGen.random_tree(rng, rand(rng, 5:25)))
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
                    convert(
                        ArenaNode{Float64}, ArenaTreeGen.random_tree(rng, rand(rng, 1:5))
                    ),
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
            @test is_compact_root(c)
            @test convert(Node, c) == expected
        end
    end
end

@testitem "ArenaNode buffered plan eval" setup = [ArenaTreeGen] begin
    using DynamicExpressions
    using DynamicExpressions: Node, EvalOptions, ArrayBuffer
    using Random

    using DynamicExpressions: ArenaNode, Arena
    using DynamicExpressions.ArenaNodeModule:
        ArenaEntry, _replace, is_compact_root, push_constant!, push_feature!

    operators = OperatorEnum(; binary_operators=[+, *, /, -], unary_operators=[cos, exp])
    rng = MersenneTwister(11)

    for T in (Float32, Float64)
        X = randn(rng, T, 5, 37)
        buf_a = zeros(T, 40, 37)
        buf_n = zeros(T, 40, 37)
        for trial in 1:60, early_exit in (true, false)
            tree = ArenaTreeGen.random_tree(rng, rand(rng, 1:30); T, nfeat=5, const_p=0.4)
            atree = convert(ArenaNode{T}, tree)
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
        adeep = convert(ArenaNode{T}, deep)
        big = zeros(T, 80, 37)
        o = EvalOptions(; buffer=ArrayBuffer(big, Ref(0)))
        y1, ok1 = eval_tree_array(copy(deep), X, operators)
        y2, ok2 = eval_tree_array(adeep, X, operators; eval_options=o)
        @test ok1 == ok2
        ok1 && @test y1 ≈ y2
    end
end

@testitem "ArenaNode buffered eval, degree 3" setup = [ArenaTreeGen] begin
    using DynamicExpressions
    using DynamicExpressions: Node, EvalOptions, ArrayBuffer
    using Random

    using DynamicExpressions: ArenaNode, Arena
    using DynamicExpressions.ArenaNodeModule:
        ArenaEntry, _replace, is_compact_root, push_constant!, push_feature!

    my3(x, y, z) = x * y + z
    operators = OperatorEnum(1 => (cos, exp), 2 => (+, *, -, /), 3 => (fma, my3))
    rng = MersenneTwister(5)
    T = Float64

    X = randn(rng, T, 5, 29)
    buf = zeros(T, 48, 29)
    for trial in 1:40, early_exit in (true, false)
        tree = ArenaTreeGen.random_tree(
            rng,
            rand(rng, 1:25);
            D=3,
            nfeat=5,
            nops=(2, 4, 2),
            arity_cdf=(0.25, 0.6, 1.0),
            const_p=0.4,
        )
        atree = convert(ArenaNode{T,3}, tree)
        o = EvalOptions(; early_exit, buffer=ArrayBuffer(buf, Ref(0)))
        rt = try
            (
                eval_tree_array(
                    copy(tree), X, operators; eval_options=EvalOptions(; early_exit)
                ),
                false,
            )
        catch
            (nothing, true)
        end
        ra = try
            (eval_tree_array(atree, X, operators; eval_options=o), false)
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
    end
end

@testitem "ArenaNode review regressions" begin
    using DynamicExpressions
    using DynamicExpressions: Node, EvalOptions, ArrayBuffer
    using DynamicExpressions.NodePreallocationModule: allocate_container, copy_into!

    using DynamicExpressions: ArenaNode, Arena
    using DynamicExpressions.ArenaNodeModule:
        ArenaEntry, _replace, is_compact_root, push_constant!, push_feature!

    operators = OperatorEnum(1 => (cos, exp), 2 => (+, *, -, /))
    T = Float64
    X = T[1.0 2.0 3.0; 0.5 1.5 2.5]
    nrows = size(X, 2)
    buf() = ArrayBuffer(zeros(T, 16, nrows), Ref(0))
    to_arena(t) = convert(ArenaNode{T,2}, t)
    x1 = Node{T}(; feature=1)
    x2 = Node{T}(; feature=2)

    @testset "multi-root arenas fail closed" begin
        a = Arena{T,2}()
        push_constant!(a, 1.0)
        i2 = push_feature!(a, 2)
        n2 = ArenaNode(a, i2)
        @test !is_compact_root(n2)
        @test count_nodes(n2) == 1
        y, ok = eval_tree_array(n2, X, operators; eval_options=EvalOptions(; buffer=buf()))
        @test ok && y ≈ X[2, :]
    end

    @testset "set_scalar_constants! converts the eltype" begin
        small = to_arena(Node{T}(; op=1, l=x1, r=Node{T}(; val=9.0)))
        _, refs = get_scalar_constants(small)
        set_scalar_constants!(small, Float32[2.5], refs)
        @test any(n -> n.degree == 0 && n.constant && n.val == 2.5, small)
    end

    @testset "out-of-arity get_child throws" begin
        leaf = ArenaNode{T,1}()
        @test_throws BoundsError leaf.r
    end

    @testset "half-built nodes throw on traversal" begin
        half = to_arena(x1)
        half.degree = 1
        half.op = 1
        @test_throws UndefRefError DynamicExpressions.NodeUtilsModule.is_constant(half)
        @test_throws UndefRefError any(_ -> false, half)
    end

    function check_parity(tree, ops, Xm; early_exit)
        atree = convert(ArenaNode{T,2}, tree)
        yn, okn = eval_tree_array(
            copy(tree), Xm, ops; eval_options=EvalOptions(; early_exit)
        )
        o = EvalOptions(; early_exit, buffer=ArrayBuffer(zeros(T, 16, size(Xm, 2)), Ref(0)))
        ya, oka = eval_tree_array(atree, Xm, ops; eval_options=o)
        @test okn == oka
        if okn
            @test yn ≈ ya || (any(!isfinite, yn) && any(!isfinite, ya))
        end
    end

    @testset "ok-flag parity with the generic evaluator" begin
        nanleaf = Node{T}(; val=NaN)
        infdiv = Node{T}(; op=4, l=Node{T}(; val=1.0), r=Node{T}(; val=0.0))
        for early_exit in (true, false)
            check_parity(nanleaf, operators, X; early_exit)
            check_parity(Node{T}(; op=1, l=x1, r=nanleaf), operators, X; early_exit)
            check_parity(Node{T}(; op=1, l=x1, r=infdiv), operators, X; early_exit)
            check_parity(
                Node{T}(; op=1, l=x1, r=Node{T}(; val=Inf)), operators, X; early_exit
            )
            grow = Node{T}(;
                op=2,
                l=Node{T}(; op=2, l=x1, r=Node{T}(; val=1e300)),
                r=Node{T}(; val=1e300),
            )
            check_parity(Node{T}(; op=1, l=grow, r=x2), operators, X; early_exit)  # Inf intermediate
            check_parity(grow, operators, X; early_exit)  # Inf at the root
        end

        nanclean(x) = ifelse(isnan(x), zero(x), x)
        ops2 = OperatorEnum(1 => (nanclean,), 2 => (+, *))
        Xnan = copy(X)
        Xnan[1, 2] = NaN
        for early_exit in (true, false)
            check_parity(copy(x1), ops2, Xnan; early_exit)  # bare feature root, NaN input
            check_parity(Node{T}(; op=1, l=x1), ops2, Xnan; early_exit)  # NaN input absorbed
            check_parity(Node{T}(; op=1, l=Node{T}(; val=NaN)), ops2, X; early_exit)  # NaN leaf folded away
        end
    end

    @testset "use_fused=false takes the generic path" begin
        t = to_arena(Node{T}(; op=1, l=x1, r=Node{T}(; op=2, l=x2, r=Node{T}(; val=3.0))))
        o_nofuse = EvalOptions(; buffer=buf(), use_fused=Val(false))
        y1, ok1 = eval_tree_array(t, X, operators; eval_options=o_nofuse)
        @test ok1 && o_nofuse.buffer.index[] > 0  # generic buffer protocol engaged
        o_plan = EvalOptions(; buffer=buf())
        y2, ok2 = eval_tree_array(t, X, operators; eval_options=o_plan)
        @test ok2 && o_plan.buffer.index[] == 0  # plan path bypasses the index
        @test y1 ≈ y2
    end

    @testset "cross-representation ==" begin
        tn = Node{T}(; op=1, l=x1, r=Node{T}(; op=2, l=x2, r=Node{T}(; val=0.5)))
        ta = to_arena(tn)
        @test ta == tn
        @test tn == ta
        @test convert(ArenaNode{Float32,2}, tn) == tn
    end

    @testset "copy_into! container reuse" begin
        t = to_arena(Node{T}(; op=1, l=x1, r=Node{T}(; val=2.0)))
        c = allocate_container(t)
        t2 = copy_into!(c, t)
        t3 = copy_into!(c, t2)
        @test string_tree(t3, operators) == string_tree(t, operators)
    end

    @testset "non-isbits eltype takes the generic path safely" begin
        tb = Node{BigFloat,2}(;
            op=1, l=Node{BigFloat,2}(; feature=1), r=Node{BigFloat,2}(; val=big"1.5")
        )
        ab = convert(ArenaNode{BigFloat,2}, tb)
        Xb = BigFloat.(X)
        bufb = ArrayBuffer(Matrix{BigFloat}(undef, 16, nrows), Ref(0))
        yb, okb = eval_tree_array(
            ab, Xb, operators; eval_options=EvalOptions(; buffer=bufb)
        )
        @test okb && yb ≈ Xb[1, :] .+ big"1.5"
    end
end

@testitem "ArenaNode supposition invariants" begin
    using Test
    using Supposition
    using Supposition: @check, Data
    using DynamicExpressions
    using DynamicExpressions: Node, EvalOptions, ArrayBuffer, get_tree

    using DynamicExpressions: ArenaNode

    include("supposition_utils.jl")

    const T = Float64
    const N_FEATURES = 5
    const OPERATORS = OperatorEnum(1 => (abs, cos), 2 => (+, -, *, /))

    expr_gen = make_expression_generator(
        T; num_features=N_FEATURES, max_layers=8, operators=OPERATORS
    )
    tree_gen = map(get_tree, expr_gen)
    input_gen = make_input_matrix_generator(T; n_features=N_FEATURES)

    # Round-trip and read-only properties are representation-independent.
    roundtrip = @check function arena_roundtrip(tree=tree_gen)
        atree = convert(ArenaNode{T,2}, tree)
        back = convert(Node, atree)
        return back == tree &&
               string_tree(atree, OPERATORS) == string_tree(tree, OPERATORS) &&
               count_nodes(atree) == count_nodes(tree) &&
               count_constant_nodes(atree) == count_constant_nodes(tree) &&
               count_depth(atree) == count_depth(tree) &&
               hash(atree) == hash(tree) &&
               has_constants(atree) == has_constants(tree) &&
               has_operators(atree) == has_operators(tree) &&
               copy(atree) == atree
    end
    @test something(roundtrip.result) isa Supposition.Pass

    function evals_match(tree, atree, X)
        # Unbuffered ArenaNode eval takes the same generic path as Node, so
        # both the values and the `ok` flag must match exactly.
        yn, okn = eval_tree_array(copy(tree), X, OPERATORS)
        ya, oka = eval_tree_array(atree, X, OPERATORS)
        okn == oka || return false
        okn && !(yn ≈ ya) && return false
        # The buffered plan path's `ok` is best-effort and may differ from the
        # generic evaluator (which fuses some shapes without materializing the
        # intermediate this path validates; the `is_valid(sum(...))` check can
        # also overflow on finite-but-huge values). Values must agree whenever
        # both sides report ok.
        buffer = ArrayBuffer(zeros(T, 64, size(X, 2)), Ref(0))
        yb, okb = eval_tree_array(atree, X, OPERATORS; eval_options=EvalOptions(; buffer))
        okb && okn && !(yb ≈ yn) && return false
        return true
    end

    evals = @check function arena_eval_matches_node(tree=tree_gen, X=input_gen)
        return evals_match(tree, convert(ArenaNode{T,2}, tree), X)
    end
    @test something(evals.result) isa Supposition.Pass

    # Valid mutations applied identically to both representations keep them
    # equivalent. Mutations are specified positionally (preorder index).
    # a single multi-arg map (no Data.OneOf / generator unions, which older
    # Supposition versions in the downgrade-compat CI job cannot handle)
    mutation_gen = map(
        (kind, i, value, op, feature) -> if kind == 1
            (:set_val, i, value)
        elseif kind == 2
            (:set_op, i, op)
        elseif kind == 3
            (:to_leaf, i, value)
        else
            (:to_feature, i, feature)
        end,
        Data.SampledFrom(1:4),
        Data.Integers(1, 64),
        Data.Floats{T}(; nans=false, infs=false),
        Data.Integers(1, 4),
        Data.Integers(1, N_FEATURES),
    )

    function apply_mutation!(tree, (kind, i, x))
        nodes = collect(tree)
        node = nodes[mod1(i, length(nodes))]
        if kind == :set_val
            if node.degree == 0 && node.constant
                node.val = x
            end
        elseif kind == :set_op
            if node.degree == 2
                node.op = mod1(x, 4)
            elseif node.degree == 1
                node.op = mod1(x, 2)
            end
        elseif kind == :to_leaf
            node.degree = 0
            node.constant = true
            node.val = x
        elseif kind == :to_feature
            node.degree = 0
            node.constant = false
            node.feature = x
        end
        return tree
    end

    mutations = @check function arena_mutation_matches_node(
        tree0=tree_gen, X=input_gen, muts=Data.Vectors(mutation_gen; min_size=1, max_size=5)
    )
        tree = copy(tree0)
        atree = convert(ArenaNode{T,2}, tree0)
        for mut in muts
            apply_mutation!(tree, mut)
            apply_mutation!(atree, mut)
        end
        return string_tree(atree, OPERATORS) == string_tree(tree, OPERATORS) &&
               count_nodes(atree) == count_nodes(tree) &&
               convert(Node, atree) == tree &&
               evals_match(tree, atree, X)
    end
    @test something(mutations.result) isa Supposition.Pass

    # Re-compacting after mutation (via copy) preserves the tree and re-enables
    # the flat fast paths.
    recompact = @check function arena_copy_recompacts(
        tree0=tree_gen, X=input_gen, muts=Data.Vectors(mutation_gen; min_size=1, max_size=3)
    )
        tree = copy(tree0)
        atree = convert(ArenaNode{T,2}, tree0)
        for mut in muts
            apply_mutation!(tree, mut)
            apply_mutation!(atree, mut)
        end
        compacted = copy(atree)
        return DynamicExpressions.ArenaNodeModule.is_compact_root(compacted) &&
               compacted == atree &&
               hash(compacted) == hash(atree) &&
               evals_match(tree, compacted, X)
    end
    @test something(recompact.result) isa Supposition.Pass
end
