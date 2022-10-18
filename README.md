<div align="center">

<img src="https://user-images.githubusercontent.com/7593028/196523542-305f3fc2-18d2-41e5-9252-1f96c3d0b7e7.png" height="50%" width="50%"></img>

*Ridiculously fast dynamic expressions.*

[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://symbolicml.org/DynamicExpressions.jl/dev) [![CI](https://github.com/SymbolicML/DynamicExpressions.jl/actions/workflows/CI.yml/badge.svg)](https://github.com/SymbolicML/DynamicExpressions.jl/actions/workflows/CI.yml) [![Coverage Status](https://coveralls.io/repos/github/SymbolicML/DynamicExpressions.jl/badge.svg?branch=master)](https://coveralls.io/github/SymbolicML/DynamicExpressions.jl?branch=master)

DynamicExpressions.jl is the backbone of 
[SymbolicRegression.jl](https://github.com/MilesCranmer/SymbolicRegression.jl) and
[PySR](https://github.com/MilesCranmer/PySR).

</div>

## Summary

A dynamic expression is a snippet of code that can change throughout
runtime - compilation is not possible!

DynamicExpressions.jl:
1. Defines an enum over user-specified scalar operators.
2. Using this enum, it defines a very lightweight
and type-stable data structure for arbitrary expressions.
3. It then generates specialized evaluation kernels for
the space of potential operators.
4. It also generates kernels for the first-order derivatives, using [Zygote.jl](https://github.com/FluxML/Zygote.jl).

## Example

```julia
using DynamicExpressions

operators = OperatorEnum(; binary_operators=[+, -, *], unary_operators=[cos])

x1 = Node(; feature=1)
x2 = Node(; feature=2)

expression = x1 * cos(x2 - 3.2)

X = randn(Float64, 2, 100);
expression(X) # 100-element Vector{Float64}
```

### Speed

First, what happens if we naively use Julia symbols to define
and then evaluate this expression?

```julia
@btime eval(:(X[1, :] .* cos.(X[2, :] .- 3.2)))
# 117,000 ns
```

This is quite slow, meaning it will be hard to
quickly search over the space of expressions.
Let's see how DynamicExpressions.jl compares:

```julia
@btime expression(X)
# 693 ns
```

Much faster!
And we didn't even need to compile it.
If we change `expression` dynamically with a random number generator,
it will have the same performance:

```julia
@btime begin
    expression.op = rand(1:3)  # random operator in [+, -, *]
    expression(X)
end
# 842 ns
```

Now, let's see the performance if we had hard-coded these expressions:

```julia
f(X) = X[1, :] .* cos.(X[2, :] .- 3.2)
@btime f(X)
# 708 ns
```

So, our dynamic expression evaluation is about the same (or even a bit faster)
as evaluating a basic hard-coded expression!
Let's see if we can optimize the hard-coded version:

```julia
f_optimized(X) = begin
    y = Vector{Float64}(undef, 100)
    @inbounds @simd for i=1:100;
        y[i] = X[1, i] * cos(X[2, i] - 3.2)
    end
    y
end
@btime f_optimized(X)
# 526 ns
```

The `DynamicExpressions.jl` version is only 25% slower than one which
has been optimized by hand into a single SIMD kernel! Not bad at all.

More importantly: we can change `expression` throughout runtime,
and expect the same performance.
This makes this data structure ideal for symbolic
regression and other evaluation-based searches
over expression trees.


## Derivatives

We can also compute gradients with the same speed:

```julia
operators = OperatorEnum(;
    binary_operators=[+, -, *],
    unary_operators=[cos],
    enable_autodiff=true,
)
x1 = Node(; feature=1)
x2 = Node(; feature=2)
expression = x1 * cos(x2 - 3.2)
```

We can take the gradient with respect to inputs
with simply the `'` character:

```julia
grad = expression'(X)
```

This is quite fast:

```julia
@btime expression'(X)
# 2.894 us
```

Internally, this is calling the `eval_grad_tree_array` function,
which performs forward-mode automatic differentiation
on the expression tree with Zygote-compiled kernels.
We can also compute the derivative with respect to constants:

```julia
result, grad, did_finish = eval_grad_tree_array(expression, X, operators; variable=false)
```

or with respect to variables, and only in a single direction:

```julia
feature = 2
result, grad, did_finish = eval_diff_tree_array(expression, X, operators, feature)
```
