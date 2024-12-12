@testitem "copy_node! - random trees" begin
    using DynamicExpressions
    using DynamicExpressions: copy_node!
    include("tree_gen_utils.jl")

    operators = OperatorEnum(; binary_operators=[+, *, /], unary_operators=[sin, cos])

    for size in [1, 2, 5, 10, 20], _ in 1:10, N in (Node, ParametricNode)
        tree = gen_random_tree_fixed_size(size, operators, 5, Float64, N)
        n_nodes = count_nodes(tree)
        @test n_nodes == size  # Verify gen_random_tree_fixed_size worked

        # Make array larger than needed to test bounds:
        dest_array = [N{Float64}() for _ in 1:(n_nodes + 10)]
        orig_nodes = dest_array[(n_nodes + 1):end]  # Save reference to unused nodes

        ref = Ref(0)
        result = copy_node!(dest_array, tree; ref)

        @test ref[] == n_nodes  # Increment once per node

        # Should be the same tree:
        @test result == tree
        @test hash(result) == hash(tree)

        # The root should be the last node in the destination array:
        @test result === dest_array[n_nodes]

        # Every node in the resultant tree should be from an allocated
        # node in the destination array:
        @test all(n -> any(n === x for x in dest_array[1:n_nodes]), result)

        # There should be no aliasing:
        @test Set(map(objectid, result)) == Set(map(objectid, dest_array[1:n_nodes]))
    end
end

@testitem "copy_node! - leaf nodes" begin
    using DynamicExpressions
    using DynamicExpressions: copy_node!

    leaf_constant = Node{Float64}(; val=1.0)
    leaf_feature = Node{Float64}(; feature=1)

    for leaf in [leaf_constant, leaf_feature]
        dest_array = [Node{Float64}() for _ in 1:1]
        ref = Ref(0)
        result = copy_node!(dest_array, leaf; ref=ref)
        @test ref[] == 1
        @test result == leaf
        @test result === dest_array[1]
    end
end
