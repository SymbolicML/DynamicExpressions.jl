using Test
using DynamicExpressions

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
    @test string_tree(atree_setnode, operators) == string_tree(atree_setnode2, operators)

    # set_child! should accept children from another arena by copying them into the target arena.
    parent = AN.arena_from_tree(sin(x1))
    other = AN.arena_from_tree(x1 * 3.2)
    set_child!(parent, other, 1)
    @test get_child(parent, 1).arena === parent.arena

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
    tree2 = AN.tree_from_arena(atree)
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
            atree,
            _ -> 1,
            _ -> 0,
            (_, children) -> maximum(children) + 1,
            depth_stack,
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

    @testset "Arena allocations" begin
        # DispatchDoctor checks in the test environment can dominate allocation counts.
        # Measure these low-level allocation properties in a fresh process using the
        # package project (dispatch doctor disabled by default there).
        project_root = normpath(joinpath(@__DIR__, ".."))

        alloc_script = raw"""
            local_prefs = joinpath(dirname(Base.active_project()), "LocalPreferences.toml")
            prefs_text = string(
                "[DynamicExpressions]\n",
                "dispatch_doctor_mode = ",
                repr("disable"),
                "\n",
            )
            write(local_prefs, prefs_text)
            atexit(() -> rm(local_prefs; force=true))

            using DynamicExpressions
            const AN = DynamicExpressions.ArenaNodeModule

            operators = OperatorEnum(1 => (sin, cos), 2 => (+, *))
            x1 = DynamicExpressions.Node{Float64}(; feature=1)

            function alloc_push_constant!(arena)
                AN.push_constant!(arena, 1.0)
                return nothing
            end

            function alloc_set_child!(parent, child)
                set_child!(parent, child, 1)
                return nothing
            end

            function alloc_copy_tree!(arena, tree)
                AN._copy_to_arena!(arena, tree)
                return nothing
            end

            arena_push = AN.Arena{Float64,2}(; capacity=16)

            base_tree = sin(x1)
            parent_arena = AN.Arena{Float64,2}(; capacity=16)
            parent_idx = AN._copy_to_arena!(parent_arena, base_tree)
            parent = AN.ArenaNode(parent_arena, parent_idx)

            child_tree = x1 * 3.2
            child_arena = AN.Arena{Float64,2}(; capacity=16)
            child_idx = AN._copy_to_arena!(child_arena, child_tree)
            child = AN.ArenaNode(child_arena, child_idx)

            tree_large = sin(x1) + x1 * 3.2 + cos(x1)
            arena_large = AN.Arena{Float64,2}(; capacity=64)

            alloc_push_constant!(arena_push) # warmup
            alloc_set_child!(parent, child) # warmup
            alloc_copy_tree!(arena_large, tree_large) # warmup

            println("push_constant=$(@allocated alloc_push_constant!(arena_push))")
            println("set_child=$(@allocated alloc_set_child!(parent, child))")
            println("copy_tree=$(@allocated alloc_copy_tree!(arena_large, tree_large))")
        """

        julia_bin = joinpath(Sys.BINDIR, Base.julia_exename())
        cmd = `$(julia_bin) --startup-file=no --project=$(project_root) -e $(alloc_script)`
        out = read(cmd, String)

        allocs = Dict{String,Int}()
        for m in eachmatch(r"(push_constant|set_child|copy_tree)=(\d+)", out)
            allocs[m.captures[1]] = parse(Int, m.captures[2])
        end

        @test all(k -> haskey(allocs, k), ("push_constant", "set_child", "copy_tree"))
        @test allocs["push_constant"] <= 1024
        @test allocs["set_child"] <= 1024
        @test allocs["copy_tree"] <= 1024
    end

end
