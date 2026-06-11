@testitem "Test arena-backed node prototype" begin
    using Test
    using DynamicExpressions
    using DynamicExpressions: NodeInterface
    using Interfaces: Interfaces

    const AN = DynamicExpressions.ArenaNodeModule

    @testset "Arena-backed node prototype" begin
        operators = OperatorEnum(1 => (sin, cos), 2 => (+, *))

        # Build a normal heap tree:
        x1 = Node{Float64}(; feature=1)
        tree = sin(x1) + x1 * 3.2

        # Convert to arena-backed representation:
        atree = AN.arena_from_tree(tree)

        @test atree isa AN.ArenaNode{Float64,2}
        @test count_nodes(atree) == count_nodes(tree)
        @test string_tree(atree, operators) == string_tree(tree, operators)
        @test tree_mapreduce(_ -> 1, +, atree, Int) == count_nodes(atree)

        @test Interfaces.test(
            NodeInterface,
            AN.ArenaNode,
            [
                atree,
                AN.arena_from_tree(sin(x1)),
                AN.arena_from_tree(x1),
                AN.arena_from_tree(Node{Float64}(; val=1.0)),
            ],
        )

        # Children accessors should behave like `Node`:
        if atree.degree != 0
            cs = DynamicExpressions.NodeModule.unsafe_get_children(atree)
            @test cs isa NTuple{2,DynamicExpressions.Nullable{typeof(atree)}}
            @test length(get_children(atree, atree.degree)) == atree.degree
        end

        # Cursor traversal should match the package's collect() DFS order
        # (and the cursor should be reusable without reallocating the stack).
        cursor = AN.ArenaCursor(atree; capacity=count_nodes(atree))
        seen = Int32[]
        AN.foreach_preorder!(n -> push!(seen, n.idx), atree, cursor)
        seen2 = Int32[]
        AN.foreach_preorder!(n -> push!(seen2, n.idx), atree, cursor)
        @test seen == seen2

        collected = collect(atree; break_sharing=Val(true))
        collected_idxs = map(n -> n.idx, collected)
        @test collected_idxs == seen

        # Evaluation should match:
        X = randn(Float64, 1, 50)
        y_tree, ok_tree = eval_tree_array(tree, X, operators)
        y_atree, ok_atree = eval_tree_array(atree, X, operators)
        @test ok_tree
        @test ok_atree
        @test y_tree ≈ y_atree

        # In-place set_node! should work even when the source tree is from a different arena.
        # (This is important for API-compat with algorithms that construct new subtrees.)
        atree_setnode = AN.arena_from_tree(tree)
        atree_setnode2 = copy(atree_setnode)
        set_node!(atree_setnode, atree_setnode2)
        @test string_tree(atree_setnode, operators) ==
            string_tree(atree_setnode2, operators)

        # set_child! should accept children from another arena by copying them into the target arena.
        parent = AN.arena_from_tree(sin(x1))
        other = AN.arena_from_tree(x1 * 3.2)
        set_child!(parent, other, 1)
        @test get_child(parent, 1).arena === parent.arena
        other.r.val = 99.0
        y_parent, ok_parent = eval_tree_array(parent, X, operators)
        @test ok_parent
        @test y_parent ≈ sin.(X[1, :] .* 3.2)

        # In-place simplify should work.
        tree_fold = Node{Float64}(; val=2.0) + Node{Float64}(; val=3.0)
        atree_fold = AN.arena_from_tree(tree_fold)
        simplify_tree!(atree_fold, operators)
        @test atree_fold.degree == 0
        @test atree_fold.constant
        @test atree_fold.val == 5.0

        # Mutating a constant in-place via the facade should affect evaluation:
        const_nodes = filter(t -> t.degree == 0 && t.constant, atree)
        @test !isempty(const_nodes)
        const_nodes[1].val = 10.0
        y_mut, ok_mut = eval_tree_array(atree, X, operators)
        @test ok_mut
        @test !(y_mut ≈ y_tree)

        # Copy should deep-copy into a new arena.
        atree2 = copy(atree)
        @test atree2 == atree
        # Mutate copy and confirm original unchanged.
        const_nodes2 = filter(t -> t.degree == 0 && t.constant, atree2)
        const_nodes2[1].val = -5.0
        @test atree2 != atree

        # Roundtrip conversion back to heap nodes should preserve semantics:
        tree2 = convert(Node, atree)
        y_tree2, ok_tree2 = eval_tree_array(tree2, X, operators)
        @test ok_tree2
        @test y_tree2 ≈ y_mut

        @testset "Postfix / debug utilities (not an execution strategy)" begin
            # Postfix stack-based utilities (mirroring symbolic_regression.rs patterns):
            @test AN.is_valid_postfix(atree)

            sizes = Int[]
            size_stack = Int[]
            AN.subtree_sizes_into!(atree, sizes, size_stack)
            start, stop = AN.subtree_range(sizes, Int(atree.idx))
            @test start == 1
            @test stop == Int(atree.idx)

            depth_stack = Int[]
            depth_postfix = AN.tree_mapreduce_postfix_with_stack(
                atree, _ -> 1, _ -> 0, (_, children) -> maximum(children) + 1, depth_stack
            )
            @test depth_postfix == count_depth(atree)

            # Postfix roundtrip sanity check (debug utility; not an execution strategy):
            pf = AN.emit_postfix(atree)
            atree_pf = AN.parse_postfix_to_arena(pf)
            @test AN.is_valid_postfix(atree_pf)
            @test count_nodes(atree_pf) == count_nodes(atree)
            @test string_tree(atree_pf, operators) == string_tree(atree, operators)
            y_pf, ok_pf = eval_tree_array(atree_pf, X, operators)
            @test ok_pf
            @test y_pf ≈ y_mut

            # Minimal rewrite prototype should preserve postfix validity:
            tree_constleft = 3.2 * x1
            atree_constleft = AN.arena_from_tree(tree_constleft)
            @test AN.is_valid_postfix(atree_constleft)
            y_before, ok_before = eval_tree_array(atree_constleft, X, operators)
            @test ok_before
            @test atree_constleft.l.constant
            AN.rewrite_commutative_constants_right!(atree_constleft, operators)
            @test AN.is_valid_postfix(atree_constleft)
            @test !atree_constleft.l.constant
            @test atree_constleft.r.constant
            y_after, ok_after = eval_tree_array(atree_constleft, X, operators)
            @test ok_after
            @test y_after ≈ y_before
        end

        @testset "Expression with ArenaNode" begin
            using DynamicExpressions: ExpressionInterface, Expression, get_tree
            using Interfaces: test

            expr = Expression(atree; operators, variable_names=["x"])
            @test get_tree(expr) === atree
            @test test(ExpressionInterface, Expression, [expr])

            # Also test with a simpler expression
            simple_atree = AN.arena_from_tree(x1)
            simple_expr = Expression(simple_atree; operators, variable_names=["x"])
            @test test(ExpressionInterface, Expression, [simple_expr])
        end

        @testset "Derivatives with ArenaNode-based Expression" begin
            using Zygote
            using DynamicExpressions: eval_grad_tree_array, extract_gradient
            using DifferentiationInterface: AutoZygote, gradient

            operators_grad = OperatorEnum(;
                binary_operators=[+, -, *, /], unary_operators=[sin, cos, exp]
            )
            x1g = Node{Float64}(; feature=1)
            x2g = Node{Float64}(; feature=2)
            tree_grad = sin(2.0 * x1g + exp(x2g + 5.0))
            atree_grad = AN.arena_from_tree(tree_grad)
            expr_grad = Expression(
                atree_grad; operators=operators_grad, variable_names=[:x1, :x2]
            )

            Xg = rand(Float64, 2, 10) .+ 1
            expected = @. sin(2.0 * Xg[1, :] + exp(Xg[2, :] + 5.0))
            expected_dy_dx1 = @. 2.0 * cos(2.0 * Xg[1, :] + exp(Xg[2, :] + 5.0))

            result, ok = eval_tree_array(expr_grad, Xg)
            @test ok
            @test result ≈ expected

            # Variable gradients via eval_grad_tree_array
            result2, grad2, ok2 = eval_grad_tree_array(expr_grad, Xg; variable=Val(true))
            @test ok2
            @test grad2[1, :] ≈ expected_dy_dx1

            # Variable gradients via Zygote
            grad_zygote = expr_grad'(Xg)
            @test grad_zygote[1, :] ≈ expected_dy_dx1

            # Constant gradients via eval_grad_tree_array
            arena_const = AN.arena_from_tree(x1g + 1.5)
            expr_const = Expression(
                arena_const;
                operators=OperatorEnum(; binary_operators=[+]),
                variable_names=["x1"],
            )
            result3, grad3, ok3 = eval_grad_tree_array(
                expr_const, ones(1, 5); variable=Val(false)
            )
            @test ok3
            @test grad3[1, :] ≈ fill(1.0, 5)

            # Constant gradients via Zygote + DifferentiationInterface
            d_ex = gradient(AutoZygote(), expr_const) do ex
                sum(ex(ones(1, 5)))
            end
            @test extract_gradient(d_ex, expr_const) ≈ [5.0]
        end

        @testset "Arena allocations" begin
            using PerformanceTestTools
            PerformanceTestTools.include_foreach(
                joinpath(@__DIR__, "test_arenanode_allocations.jl"),
                [Dict("JULIA_PKG_PRECOMPILE_AUTO" => "0")],
            )
        end
    end
end
