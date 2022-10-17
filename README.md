<div align="center">

# DynamicExpressions.jl

Blazingly-fast dynamic expressions.

</div>

DynamicExpressions.jl is the backbone of 
[SymbolicRegression.jl](https://github.com/MilesCranmer/SymbolicRegression.jl) and
[PySR](https://github.com/MilesCranmer/PySR).

Example:

```julia
using DynamicExpressions

operators = OperatorEnum(; binary_operators=[+, -, *], unary_operators=[cos])

x1 = Node(; feature=1)
x2 = Node(; feature=2)

expression = x1 * cos(x2 - 3.2)

X = randn(Float64, 2, 100);
expression(X) # 100-element Vector{Float64}
```

This evaluation is extremely fast, without us having to compile it.

For comparison:

```julia
@btime expression(X)
# 1.488 us
```

Now let's see the performance if we had hard-coded it:

```julia
f(X) = X[1, :] .* cos.(X[2, :] .- 3.2)
@btime f(X)
# 0.952 us
```

So, our dynamic expression evaluation is only 1.5x the time it took to evaluate a hard-coded expression!
Not bad at all.

We can change `expression` throughout runtime, and expect the same performance:
this makes this data structure ideal for symbolic regression and other evaluation-based searches
over expression trees.


For the record, let's see what this would look like
if we had evaluated the dynamic expression naively,
with Julia symbols and `eval`:

```julia
@btime eval(:(X[1, :] .* cos.(X[2, :] .- 3.2)))
# 151.333 us
```

so this custom data structure is quite important for
fast evaluation!
