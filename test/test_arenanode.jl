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

    # Evaluation should match:
    X = randn(Float64, 1, 50)
    y_tree, ok_tree = eval_tree_array(tree, X, operators)
    y_atree, ok_atree = eval_tree_array(atree, X, operators)
    @test ok_tree
    @test ok_atree
    @test y_tree ≈ y_atree

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
end
