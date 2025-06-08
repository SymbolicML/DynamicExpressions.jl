@testitem "3-degree GraphNode Compatibility and Sharing" tags = [:narity, :graphs] begin
    using DynamicExpressions
    using Test

    # Setup: Define ternary operator and create operator enum
    my_ternary_op(x, y, z) = x + y * z
    operators = OperatorEnum(1 => (sin,), 2 => (+, *), 3 => (my_ternary_op,))
    
    # Create basic nodes
    x1, x2, x3 = GraphNode{Float64,3}(; feature=1), GraphNode{Float64,3}(; feature=2), GraphNode{Float64,3}(; feature=3)
    c1 = GraphNode{Float64,3}(; val=2.5)
    
    # Test 1: Basic 3-degree node construction
    ternary_node = GraphNode{Float64,3}(; op=1, children=(x1, x2, x3))
    @test ternary_node.degree == 3
    @test ternary_node.op == 1
    @test length(ternary_node.children) == 3
    @test all(ternary_node.children .=== (x1, x2, x3))
    
    # Test 2: Sharing functionality
    shared_subexpr = GraphNode{Float64,3}(; op=1, children=(x1, x2, c1))
    tree_with_sharing = GraphNode{Float64,3}(; op=1, children=(shared_subexpr, x3, shared_subexpr))
    
    # Verify reference equality (sharing)
    @test tree_with_sharing.children[1] === tree_with_sharing.children[3]
    @test tree_with_sharing.children[1] === shared_subexpr
    
    # Test 3: String representation shows sharing with {}
    sharing_string = string_tree(tree_with_sharing, operators)
    @test occursin("{", sharing_string)  # Contains sharing indicators
    @test count("my_ternary_op(x1, x2, 2.5)", sharing_string) == 1  # Shared part appears once
    @test occursin("{my_ternary_op(x1, x2, 2.5)}", sharing_string)  # With sharing notation
    
    # Test 4: Copy operations
    # Breaking sharing creates separate instances
    tree_no_sharing = copy_node(tree_with_sharing; break_sharing=Val(true))
    @test tree_no_sharing.children[1] !== tree_no_sharing.children[3]  # No longer shared
    no_sharing_string = string_tree(tree_no_sharing, operators)
    @test !occursin("{", no_sharing_string)  # No sharing indicators
    @test count("my_ternary_op(x1, x2, 2.5)", no_sharing_string) == 2  # Appears twice
    
    # Preserving sharing maintains reference equality
    tree_preserved = copy_node(tree_with_sharing; break_sharing=Val(false))
    @test tree_preserved.children[1] === tree_preserved.children[3]  # Still shared
    @test occursin("{", string_tree(tree_preserved, operators))
    
    # Test 5: Modification propagation through shared nodes
    original_string = string_tree(tree_with_sharing, operators)
    shared_subexpr.children = (x3, x1, x2)  # Modify shared node
    modified_string = string_tree(tree_with_sharing, operators)
    @test original_string != modified_string
    @test tree_with_sharing.children[1] === tree_with_sharing.children[3]  # Still shared
    @test occursin("my_ternary_op(x3, x1, x2)", modified_string)
    
    # Test 6: Evaluation with 3-degree nodes
    X = [1.0 2.0; 0.5 1.5; 0.8 0.3]  # 3 features, 2 samples
    simple_ternary = GraphNode{Float64,3}(; op=1, children=(x1, x2, x3))
    expected = [my_ternary_op(1.0, 0.5, 0.8), my_ternary_op(2.0, 1.5, 0.3)]
    result, flag = eval_tree_array(simple_ternary, X, operators)
    @test flag
    @test result ≈ expected
    
    # Test 7: Type conversion preserves sharing
    gn_f32 = GraphNode{Float32,3}(; feature=1)
    shared_f32 = GraphNode{Float32,3}(; op=1, children=(gn_f32, gn_f32, gn_f32))
    @test shared_f32.children[1] === shared_f32.children[2] === shared_f32.children[3]
    
    converted = convert(GraphNode{Float64,3}, shared_f32)
    @test typeof(converted) == GraphNode{Float64,3}
    @test converted.children[1] === converted.children[2] === converted.children[3]
end