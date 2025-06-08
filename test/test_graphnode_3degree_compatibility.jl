@testitem "3-degree GraphNode Compatibility and Sharing" tags = [:narity, :graphs] begin
    using DynamicExpressions
    using Test

    # Define operators including ternary
    my_ternary_op(x, y, z) = x + y * z
    clamp_op(x, low, high) = clamp(x, low, high)
    
    operators = OperatorEnum(
        1 => (sin, cos),
        2 => (+, *, -),
        3 => (my_ternary_op, clamp_op)
    )
    
    # Create 3-degree GraphNodes
    x1 = GraphNode{Float64,3}(; feature=1)
    x2 = GraphNode{Float64,3}(; feature=2)
    x3 = GraphNode{Float64,3}(; feature=3)
    c1 = GraphNode{Float64,3}(; val=2.5)
    c2 = GraphNode{Float64,3}(; val=0.0)
    c3 = GraphNode{Float64,3}(; val=1.0)
    
    # Test basic 3-degree node construction
    ternary_node = GraphNode{Float64,3}(; op=1, children=(x1, x2, c1))
    @test ternary_node.degree == 3
    @test ternary_node.op == 1
    @test DynamicExpressions.NodeModule.max_degree(ternary_node) == 3
    @test ternary_node.children[1] === x1
    @test ternary_node.children[2] === x2  
    @test ternary_node.children[3] === c1
    
    # Test sharing with 3-degree nodes
    shared_subexpr = GraphNode{Float64,3}(; op=2, children=(x1, x2, x3))  # clamp_op
    tree_with_sharing = GraphNode{Float64,3}(; op=1, children=(shared_subexpr, c1, shared_subexpr))
    
    # Verify actual sharing via reference equality
    @test tree_with_sharing.children[1] === tree_with_sharing.children[3]
    @test tree_with_sharing.children[1] === shared_subexpr
    @test tree_with_sharing.children[3] === shared_subexpr
    
    # Test string representation shows sharing with {}
    expected_string_pattern = r"my_ternary_op\(clamp_op\(x1, x2, x3\), 2\.5, \{clamp_op\(x1, x2, x3\)\}\)"
    actual_string = string_tree(tree_with_sharing, operators)
    @test occursin(expected_string_pattern, actual_string)
    @test occursin("{", actual_string)  # Contains sharing indicators
    
    # Test that breaking sharing gives different string without {}
    tree_no_sharing = copy_node(tree_with_sharing; break_sharing=Val(true))
    no_sharing_string = string_tree(tree_no_sharing, operators)
    @test !occursin("{", no_sharing_string)  # No sharing indicators
    @test occursin("clamp_op(x1, x2, x3)", no_sharing_string)
    # Should contain the pattern twice (unshared)
    @test length(collect(eachmatch(r"clamp_op\(x1, x2, x3\)", no_sharing_string))) == 2
    
    # Test that preserving sharing maintains reference equality
    tree_with_preserved_sharing = copy_node(tree_with_sharing; break_sharing=Val(false))
    @test tree_with_preserved_sharing.children[1] === tree_with_preserved_sharing.children[3]
    preserved_sharing_string = string_tree(tree_with_preserved_sharing, operators)
    @test occursin("{", preserved_sharing_string)  # Still has sharing indicators
    
    # Test modification propagation through shared nodes
    old_string = string_tree(tree_with_sharing, operators)
    # Modify the shared subexpression
    shared_subexpr.children = (x3, x1, x2)  # Change order of children
    new_string = string_tree(tree_with_sharing, operators)
    @test old_string != new_string
    # Both occurrences should show the change
    @test tree_with_sharing.children[1] === tree_with_sharing.children[3]  # Still shared
    @test occursin("clamp_op(x3, x1, x2)", new_string)
    @test occursin("{clamp_op(x3, x1, x2)}", new_string)
    
    # Test hash consistency with sharing
    tree1 = GraphNode{Float64,3}(; op=1, children=(shared_subexpr, c1, shared_subexpr))
    tree2 = GraphNode{Float64,3}(; op=1, children=(shared_subexpr, c1, shared_subexpr))
    @test hash(tree1) == hash(tree2)
    
    # Different hash when sharing is broken
    tree_broken_sharing = copy_node(tree1; break_sharing=Val(true))
    @test hash(tree1) != hash(tree_broken_sharing)
    
    # Test complex sharing with nested 3-degree nodes
    nested_shared = GraphNode{Float64,3}(; op=1, children=(x1, c2, c3))  # my_ternary_op
    complex_tree = GraphNode{Float64,3}(; op=2, children=(nested_shared, c2, nested_shared))  # clamp_op
    
    @test complex_tree.children[1] === complex_tree.children[3]
    complex_string = string_tree(complex_tree, operators)
    @test occursin("my_ternary_op(x1, 0.0, 1.0)", complex_string)
    @test occursin("{my_ternary_op(x1, 0.0, 1.0)}", complex_string)
    
    # Test evaluation works correctly with sharing
    import Random
    X = [1.5 2.0; 0.5 1.0; 0.8 1.5]  # 3 features, 2 samples
    
    # Simple 3-degree evaluation
    simple_ternary = GraphNode{Float64,3}(; op=1, children=(x1, x2, x3))
    expected_simple = [my_ternary_op(1.5, 0.5, 0.8), my_ternary_op(2.0, 1.0, 1.5)]
    result_simple, flag_simple = eval_tree_array(simple_ternary, X, operators)
    @test flag_simple
    @test result_simple ≈ expected_simple
    
    # Evaluation with sharing
    shared_eval_tree = GraphNode{Float64,3}(; op=1, children=(simple_ternary, c1, simple_ternary))
    expected_shared = [my_ternary_op(expected_simple[1], 2.5, expected_simple[1]), 
                      my_ternary_op(expected_simple[2], 2.5, expected_simple[2])]
    result_shared, flag_shared = eval_tree_array(shared_eval_tree, X, operators)
    @test flag_shared
    @test result_shared ≈ expected_shared
    
    # Test that GraphNode preserves sharing through promote
    gn1 = GraphNode{Float32,3}(; feature=1)
    gn2 = GraphNode{Float64,3}(; feature=1)
    
    shared_f32 = GraphNode{Float32,3}(; op=1, children=(gn1, gn1, gn1))
    @test shared_f32.children[1] === shared_f32.children[2] === shared_f32.children[3]
    
    # Test type conversion preserves sharing  
    converted_tree = convert(GraphNode{Float64,3}, shared_f32)
    @test typeof(converted_tree) == GraphNode{Float64,3}
    @test converted_tree.children[1] === converted_tree.children[2] === converted_tree.children[3]
end