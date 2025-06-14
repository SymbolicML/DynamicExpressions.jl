@testitem "Node interface" begin
    using DynamicExpressions
    using DynamicExpressions: NodeInterface
    using Interfaces: Interfaces

    x1 = Node{Float64}(; feature=1)
    x2 = Node{Float64}(; feature=2)

    operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])

    tree_branch_deg2 = x1 + sin(x2 * 3.5)
    tree_branch_deg1 = sin(x1)
    tree_leaf_feature = x1
    tree_leaf_constant = Node{Float64}(; val=1.0)
    graph_tree_branch_deg2 = convert(GraphNode, tree_branch_deg2)
    graph_tree_branch_deg1 = convert(GraphNode, tree_branch_deg1)
    graph_tree_leaf_feature = convert(GraphNode, tree_leaf_feature)
    graph_tree_leaf_constant = convert(GraphNode, tree_leaf_constant)

    @test Interfaces.test(
        NodeInterface,
        Node,
        [tree_branch_deg2, tree_branch_deg1, tree_leaf_feature, tree_leaf_constant],
    )
    @test Interfaces.test(
        NodeInterface,
        GraphNode,
        [
            graph_tree_branch_deg2,
            graph_tree_branch_deg1,
            graph_tree_leaf_feature,
            graph_tree_leaf_constant,
        ],
    )
end

@testitem "Node interface on n-arity nodes" begin
    using DynamicExpressions
    using DynamicExpressions: NodeInterface
    using Interfaces: Interfaces

    for D in (3, 4, 5)
        x = [Node{Float64,D}(; feature=i) for i in 1:3]
        operator_tuple = ((sin, cos, exp), (+, *, /, -), (fma, clamp), (max, min), ())
        # Create pairs for degrees 1 through D
        pairs = [i => operator_tuple[i] for i in 1:D if !isempty(operator_tuple[i])]
        operators =
            isempty(pairs) ? OperatorEnum(1 => ()) : OperatorEnum(pairs[1], pairs[2:end]...)
        DynamicExpressions.OperatorEnumConstructionModule.empty_all_globals!()
        let tree = Node{Float64,D}(; op=2, children=(x[1], x[2]))  # *
            if D > 2
                fma_idx = 1
                tree = Node{Float64,D}(; op=fma_idx, children=(tree, x[1], x[2]))  # fma
            end
            if D > 3
                idx_max = 1
                tree = Node{Float64,D}(; op=idx_max, children=(tree, x[1], x[2], x[3]))  # max
            end
            @test Interfaces.test(NodeInterface, Node, [tree])
        end
    end
end

@testitem "Node left/right property setters + preserve_sharing" begin
    using DynamicExpressions.NodeModule: Node, preserve_sharing
    using Test

    l = Node{Float64,2}(; val=1.0)
    r = Node{Float64,2}(; val=2.0)
    p = Node{Float64,2}(; op=1, children=(l, r))

    new_l = Node{Float64,2}(; val=3.0)
    p.l = new_l         # triggers `setproperty!` branch for :l
    @test p.l === new_l

    new_r = Node{Float64,2}(; val=4.0)
    p.r = new_r
    @test p.r === new_r

    # Trying to set the right child without the left will result in an error,
    # because the `children` field is not yet created (and won't be by the `.r` setter)
    p2 = Node{Float64,2}()
    p2.degree = 2
    p2.op = 1
    @test_throws UndefRefError (p2.r = new_r)
    p2.l = new_r
    p2.r = new_r  # Now we can add it
    @test p2.r === new_r
end
