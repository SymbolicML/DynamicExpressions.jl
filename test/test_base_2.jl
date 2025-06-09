@testitem "Literate example" begin
    #literate_begin file="src/examples/base_operations.md"

    #=
    # Node and Tree Operations

    This example demonstrates how to create and manipulate expression trees
    using the [`Node`](@ref) type.

    First, let's create a node to reference `feature=1` of our dataset:
    =#
    using DynamicExpressions, Random, Test

    x = Node{Float64}(; feature=1)
    @test x isa Node{Float64,2}

    # We can also create values, using `val`:
    const_1 = Node{Float64}(; val=1.0)

    #=
    Now, let's declare some operators to use in our expression tree.
    =#
    operators = OperatorEnum(1 => (sin, cos, exp), 2 => (+, -, *, /))

    # Now, let's create another variable
    y = Node{Float64}(; feature=2)

    # And we can now create expression trees using explicit Node construction:
    # tree = (x + y) * const_1 - sin(x)
    # This becomes: - (* (+ x y) const_1) (sin x)
    add_node = Node{Float64}(; op=1, children=(x, y))  # x + y (op=1 is first binop: +)
    mul_node = Node{Float64}(; op=3, children=(add_node, const_1))  # (* (+ x y) const_1) (op=3 is third binop: *)
    sin_node = Node{Float64}(; op=1, children=(x,))  # sin(x) (op=1 is first unary: sin)
    tree = Node{Float64}(; op=2, children=(mul_node, sin_node))  # - (* ...) (sin x) (op=2 is second binop: -)

    # The type of this is the same as the type of the variables
    # and constants, meaning we have type stability:
    typeof(tree), typeof(x)
    @test typeof(tree) == typeof(x)  #src

    # We can also create another tree:
    # tree2 = 2x - sin(x) becomes: - (* 2 x) (sin x)
    const_2 = Node{Float64}(; val=2.0)
    mul_node2 = Node{Float64}(; op=3, children=(const_2, x))  # 2 * x
    sin_node2 = Node{Float64}(; op=1, children=(x,))  # sin(x)
    tree2 = Node{Float64}(; op=2, children=(mul_node2, sin_node2))  # - (* 2 x) (sin x)

    # As you have noticed, the tree is printed as an expression.
    # We can control this with the [`string_tree`](@ref) function,
    # which also lets us pass the `operators` explicitly:
    string_tree(tree, operators; variable_names=["x", "y"])
    #=
    This also lets us control how each branch node and leaf node (variable/constant)
    is printed in the tree.

    There are a lot of operations you can do on tree objects,
    such as evaluating them over batched data:
    =#
    rng = Random.MersenneTwister(0)
    tree2(randn(rng, Float64, 2, 5), operators)

    #=
    Now, how does this actually work? How do these functions traverse
    the tree?

    The core operation is the [`tree_mapreduce`](@ref) function,
    which applies a function to each node in the tree,
    and then combines the results. Unlike a standard `mapreduce`,
    the `tree_mapreduce` allows you to specify different maps for
    branch nodes and leaf nodes. Also unlike a `mapreduce`, the
    reduction function needs to handle a variable number of inputs – it takes
    the mapped branch node, as well as all of the mapped children.

    Let's see an example. Say we just want to count the nodes in the tree:
    =#
    num_nodes = tree_mapreduce(node -> 1, +, tree)
    @test num_nodes == 8 #src
    #=
    Here, the `+` handles both the cases of 1 child and 2 children.
    Here, we didn't need to specify a custom branch function, but we could do that too:
    =#
    num_leafs = tree_mapreduce(leaf_node -> 1, branch_node -> 0, +, tree)
    @test num_leafs == 4 #src
    #=
    This counts the number of leaf nodes in the tree. For `tree`,
    this was `x`, `y`, `const_1`, and `x`.

    You can access fields of the [`Node`](@ref) type here to create more
    complex operations, just be careful to not access undefined fields (be sure
    to read the API specification).

    Most operators can be built with this simple pattern, even including
    evaluation of the tree, and printing of expressions. (It also allows
    for graph-like expressions like [`GraphNode`](@ref) via a `f_on_shared` keyword.)

    As a more complex example, let's compute the depth of a tree. Here, we need
    to use a more complicated reduction operation – the `max`:
    =#
    # complex_expr = x + sin(sin(exp(x))) becomes: + x (sin (sin (exp x)))
    exp_node = Node{Float64}(; op=3, children=(x,))  # exp(x) (op=3 is third unary: exp)
    sin_node1 = Node{Float64}(; op=1, children=(exp_node,))  # sin(exp(x))
    sin_node2 = Node{Float64}(; op=1, children=(sin_node1,))  # sin(sin(exp(x)))
    complex_tree = Node{Float64}(; op=1, children=(x, sin_node2))  # x + sin(sin(exp(x)))
    
    depth = tree_mapreduce(
        node -> 1, (parent, children...) -> 1 + max(children...), complex_tree
    )
    @test depth == 5 #src
    #=
    Here, the `max` handles both the cases of 1 child and 2 children.
    The parent node contributes `1` at each depth. Note that the inputs
    to the reduction are already mapped to `1`.

    Many operations do not need to handle branching, and thus, many of the typical
    operations on collections in Julia are available. For example,
    we can collect each node in the tree into a list:
    =#
    collect(tree)
    # Note that the first node in this list is the root note, which is
    # the subtraction operation:
    tree == first(collect(tree))
    @test tree == first(collect(tree)) #src
    # We can look at the operator:
    tree.degree, tree.op
    # And compare it to our list:
    operators.binops
    # Many other collection operations are available. For example, we can aggregate a relationship over each node:
    sum(node -> node.degree == 0 ? 1.5 : 0.0, tree)
    # We can even use `any` which has an early exit from the depth-first tree traversal:
    any(node -> node.degree == 2, tree)
    # We can also randomly sample nodes, using [`NodeSampler`](@ref),
    # which permits filters:
    rand(rng, NodeSampler(; tree, filter=node -> node.degree == 1))

    #literate_end
end
