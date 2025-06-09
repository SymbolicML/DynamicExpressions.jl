```@meta
EditURL = "../../../test/test_structured_expression.jl"
```

# `StructuredExpression` example

`StructuredExpression`s allow you to specify a predefined structure for an
expression that exists outside of the regular `AbstractExpressionNode` objects
which store expressions as trees.

Let's look at an example:

````@example structured_expression
using DynamicExpressions, Random
````

First, we will create some normal `Expression` objects.

````@example structured_expression
operators = OperatorEnum(1 => (cos, exp), 2 => (+, -, *, /))
variable_names = ["x", "y"]
x = Expression(Node{Float64}(; feature=1); operators, variable_names)
y = Expression(Node{Float64}(; feature=2); operators, variable_names)

typeof(x)
````

Any `AbstractExpression`, such as this `Expression` object, can be composed together
using standard Julia math operations. For example, let's some complex expressions from these:

````@example structured_expression
f = x * x - cos(2.5f0 * y + -0.5f0)
g = exp(2.0 - y * y)

f, g
````

We can then create a `StructuredExpression` from these two expressions.
This is a composite `AbstractExpression` object that composes multiple
expressions during evaluation.

````@example structured_expression
ex = StructuredExpression(
    (; f, g); structure=nt -> nt.f + nt.g, operators, variable_names
)
ex
````

Note that this is displayed as a single tree, with the `+` operator
used to combine them. Despite this, the expression is not actually
*stored* with the `+` operator in an `AbstractExpressionNode`.

By default, using `get_tree` will evaluate the result of `nt.f + nt.g`.
This let's us use things like the regular operations available to
`AbstractExpressionNode`s:

````@example structured_expression
length(get_tree(ex))
````

Next, let's try to evaluate this on some random data:

````@example structured_expression
rng = Random.MersenneTwister(0)
X = randn(rng, Float64, 2, 5)
X
````

Followed by the evaluation. Since we have stored the operators directly
in the expression object, we do not need to pass the operators explicitly.
Evaluation of an `AbstractExpression` is set up to forward through
`get_tree`, so this will work automatically.

````@example structured_expression
ex(X)
````

Which we can verify against the individual expressions:

````@example structured_expression
f(X) + g(X)
````

---

*This page was generated using [Literate.jl](https://github.com/fredrikekre/Literate.jl).*

