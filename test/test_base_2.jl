@testitem "Literate example" begin
    #literate_begin file="src/examples/base_operations.md"

    #=
    # Node and Tree Operations

    This example demonstrates how to create and manipulate expression trees
    using the [`Node`](@ref) type. We'll create a tree,
    perform various operations, and show how to traverse and modify it.

    First, let's create a simple expression tree.
    We can bootstrap this by creating a node to hold `feature=1`,
    indicating the first input variable (first column of data):
    =#
    using DynamicExpressions, Random

    x = Node{Float64}(; feature=1)

    # We can also create values, using `val`:
    const_1 = Node{Float64}(; val=1.0)

    #=
    Now, let's declare some operators to use in our expression tree.

    Note that the declaration of the `OperatorEnum` updates
    a global mapping from operators to their index in a list.
    This is purely for convenience, and most of the time, you would
    either operate directly on the `OperatorEnum`, like with [`eval_tree_array`](@ref),
    or use [`Expression`](@ref) objects to store them alongside the expression.
    =#
    operators = OperatorEnum(; unary_operators=(sin, exp), binary_operators=(+, -, *, /))

    # Now, let's create another variable
    y = Node{Float64}(; feature=2)

    # And we can now create expression trees:
    tree = (x + y) * const_1 - sin(x)

    # The type of this is the same as the type of the variables
    # and constants, meaning we have type stability:
    typeof(tree), typeof(x)
    @test typeof(tree) == typeof(x)  #src

    # We can also just use scalars directly:
    tree2 = 2x - sin(x)

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
    tree_mapreduce(node -> 1, +, tree)
    #=
    Here, the `+` handles both the cases of 1 child and 2 children.
    Here, we didn't need to specify a custom branch function, but we could do that too:
    =#
    tree_mapreduce(leaf_node -> 1, branch_node -> 0, +, tree)
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
    tree_mapreduce(
        node -> 1, (parent, children...) -> 1 + max(children...), x + sin(sin(exp(x)))
    )
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
