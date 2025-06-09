```@meta
EditURL = "../../../test/test_expressions.jl"
```

# `Expression` example

`Expression` is a fundamental type in DynamicExpressions that represents
a mathematical expression as a tree structure. It combines an
`AbstractExpressionNode` (typically a `Node`) with metadata like operators
and variable names.

````@example expression
using DynamicExpressions, Random
````

First, let's define our operators and variable names:

````@example expression
operators = OperatorEnum(1 => (sin, cos, exp), 2 => (+, -, *, /))
````

````@example expression
variable_names = ["x", "y"]
````

Now, let's create an Expression manually:

````@example expression
x = Node{Float64}(; feature=1)
x_expr = Expression(x; operators, variable_names)
````

We can build up more complex expressions using these basic building blocks:

````@example expression
y = Node{Float64}(; feature=2)
c = Node{Float64}(; val=2.0)
complex_node = Node(; op=3, l=x, r=Node(; op=1, l=y, r=c))
````

where the `3` indicates `*` and `1` indicates `+`.

````@example expression
complex_expr = Expression(complex_node; operators, variable_names)
````

This expression includes its own metadata: the operators and variable names,
and so there are no scope issues as with raw `AbstractExpressionNode` types
which depend on the last-used metadata for convenience functions like printing.
In other words, you can print this expression, or evaluate it, directly:

````@example expression
rng = Random.MersenneTwister(0)
complex_expr(randn(rng, 2, 5))
````

While creating expressions manually is faster, and should be preferred within packages,
it can be cumbersome for quickly writing more complex expressions.
DynamicExpressions provides a more convenient way to create expressions using
the `parse_expression` function, which directly parses a Julia object:

````@example expression
parsed_expr = parse_expression(
    :(sin(2.0 * x + exp(y + 5.0))); operators=operators, variable_names=variable_names
)
````

We can convert an expression into the primitive `AbstractExpressionNode` type
with [`get_tree`](@ref):

````@example expression
tree = get_tree(parsed_expr)
````

Some `AbstractExpression` types may choose to store their expression in
a different way than simply saving it as one of the fields. For any expression,
you can get the raw contents with [`get_contents`](@ref):

````@example expression
get_contents(parsed_expr)
````

Similarly, you can get the metadata for an expression with [`get_metadata`](@ref):

````@example expression
get_metadata(parsed_expr)
````

These can be used with [`with_contents`](@ref) and [`with_metadata`](@ref) to
create new expressions based on the original:

````@example expression
with_contents(parsed_expr, Node(; op=2, l=get_contents(parsed_expr)))
````

`Expression` objects support various operations defined on regular trees,
which permits us to overload specific methods with modified behavior.
For example, we can count the number of nodes, which simply forwards
to the method as it is defined on [`Node`](@ref):

````@example expression
node_count = count_nodes(parsed_expr)
println("Number of nodes: $node_count")
````

The [`tree_mapreduce`] will by default call [`get_tree`](@ref) to get the tree,
so it can be used with any expression type that overloads this method.
For example, we can compute the depth of a tree:

````@example expression
tree_mapreduce(
    leaf -> 1, branch -> 1, (parent, child...) -> parent + max(child...), parsed_expr
)
````

We can also perform more complex operations, like simplification:

````@example expression
complex_expr = parse_expression(
    :((2.0 + x) + 3.0); operators=operators, variable_names=["x"]
)
simplified_expr = combine_operators(copy(complex_expr))
println("Original: ", complex_expr)
println("Simplified: ", simplified_expr)
````

`AbstractExpression` types also have many operators in `Base` defined, which
will automatically look up the matching index in the stored [`OperatorEnum`](@ref).
This means we can combine expressions like so:

````@example expression
xs = [Expression(Node{Float64}(; feature=i); operators, variable_names) for i in 1:5]

xs[1] + xs[2]
````

These have the same type – they simply combine their `AbstractExpressionNode` objects
and ensure the metadata is the same.

````@example expression
typeof(xs[1] + xs[2])
````

This gives us an easy way to quickly construct expressions with minimal memory overhead,
and fast evaluation speed:

````@example expression
ex = xs[1] * 2.1 - exp(3 * xs[2])
````

Evaluation:

````@example expression
X = randn(rng, 5, 2)
ex(X)
````

Or, if we have loaded Zygote, we can differentiate with respect
to the variables:

````@example expression
using Zygote
ex'(X)
````

Or the constants of the expression:

````@example expression
ex'(X; variable=Val(false))
````

Which can be used for optimization.

---

*This page was generated using [Literate.jl](https://github.com/fredrikekre/Literate.jl).*

