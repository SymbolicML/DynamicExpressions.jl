<div align="center">

<img src="https://user-images.githubusercontent.com/7593028/196523542-305f3fc2-18d2-41e5-9252-1f96c3d0b7e7.png" height="50%" width="50%"></img>

*Ridiculously fast dynamic expressions.*

[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://symbolicml.org/DynamicExpressions.jl/dev) [![CI](https://github.com/SymbolicML/DynamicExpressions.jl/actions/workflows/CI.yml/badge.svg)](https://github.com/SymbolicML/DynamicExpressions.jl/actions/workflows/CI.yml) [![Coverage Status](https://coveralls.io/repos/github/SymbolicML/DynamicExpressions.jl/badge.svg?branch=master)](https://coveralls.io/github/SymbolicML/DynamicExpressions.jl?branch=master)

DynamicExpressions.jl is the backbone of [SymbolicRegression.jl](https://github.com/MilesCranmer/SymbolicRegression.jl) and
[PySR](https://github.com/MilesCranmer/PySR).

</div>

## Summary

A dynamic expression is a snippet of code that can change throughout runtime - compilation is not possible! **DynamicExpressions.jl does the following:**
1. Defines an enum over user-specified operators.
2. Using this enum, it defines a [very lightweight and type-stable data structure](https://symbolicml.org/DynamicExpressions.jl/dev/types/#DynamicExpressions.EquationModule.Node) for arbitrary expressions.
3. It then generates specialized [evaluation kernels](https://github.com/SymbolicML/DynamicExpressions.jl/blob/fe8e6dfa160d12485fb77c226d22776dd6ed697a/src/EvaluateEquation.jl#L29-L66) for the space of potential operators.
4. It also generates kernels for the [first-order derivatives](https://github.com/SymbolicML/DynamicExpressions.jl/blob/fe8e6dfa160d12485fb77c226d22776dd6ed697a/src/EvaluateEquationDerivative.jl#L139-L175), using [Zygote.jl](https://github.com/FluxML/Zygote.jl).
5. DynamicExpressions.jl can also operate on arbitrary other types (vectors, tensors, symbols, strings, or even unions) - see last part below.

It also has import and export functionality with [SymbolicUtils.jl](https://github.com/JuliaSymbolics/SymbolicUtils.jl), so you can move your runtime expression into a CAS!


## Example

```julia
using DynamicExpressions

operators = OperatorEnum(; binary_operators=[+, -, *], unary_operators=[cos])

x1 = Node(; feature=1)
x2 = Node(; feature=2)

expression = x1 * cos(x2 - 3.2)

X = randn(Float64, 2, 100);
expression(X, operators) # 100-element Vector{Float64}
```

(We can construct this expression with normal operators, since calling `OperatorEnum()` will `@eval` new functions on `Node` that use the specified enum.)

## Speed

First, what happens if we naively use Julia symbols to define and then evaluate this expression?

```julia
@btime eval(:(X[1, :] .* cos.(X[2, :] .- 3.2)))
# 117,000 ns
```

This is quite slow, meaning it will be hard to quickly search over the space of expressions. Let's see how DynamicExpressions.jl compares:

```julia
@btime expression(X, operators)
# 693 ns
```

Much faster! And we didn't even need to compile it. (Internally, this is calling `eval_tree_array(expression, X, operators)`). 

If we change `expression` dynamically with a random number generator, it will have the same performance:

```julia
@btime begin
    expression.op = rand(1:3)  # random operator in [+, -, *]
    expression(X, operators)
end
# 842 ns
```
Now, let's see the performance if we had hard-coded these expressions:

```julia
f(X) = X[1, :] .* cos.(X[2, :] .- 3.2)
@btime f(X)
# 708 ns
```

So, our dynamic expression evaluation is about the same (or even a bit faster) as evaluating a basic hard-coded expression! Let's see if we can optimize the speed of the hard-coded version:

```julia
f_optimized(X) = begin
    y = Vector{Float64}(undef, 100)
    @inbounds @simd for i=1:100
        y[i] = X[1, i] * cos(X[2, i] - 3.2)
    end
    y
end
@btime f_optimized(X)
# 526 ns
```

The `DynamicExpressions.jl` version is only 25% slower than one which has been optimized by hand into a single SIMD kernel! Not bad at all.

More importantly: we can change `expression` throughout runtime, and expect the same performance. This makes this data structure ideal for symbolic regression and other evaluation-based searches over expression trees.


## Derivatives

We can also compute gradients with the same speed:

```julia
using Zygote  # trigger extension

operators = OperatorEnum(;
    binary_operators=[+, -, *],
    unary_operators=[cos],
    enable_autodiff=true,
)
x1 = Node(; feature=1)
x2 = Node(; feature=2)
expression = x1 * cos(x2 - 3.2)
```

We can take the gradient with respect to inputs with simply the `'` character:

```julia
grad = expression'(X, operators)
```

This is quite fast:

```julia
@btime expression'(X, operators)
# 2894 ns
```

and again, we can change this expression at runtime, without loss in performance!

```julia
@btime begin
    expression.op = rand(1:3)
    expression'(X, operators)
end
# 3198 ns
```

Internally, this is calling the `eval_grad_tree_array` function, which performs forward-mode automatic differentiation on the expression tree with Zygote-compiled kernels. We can also compute the derivative with respect to constants:

```julia
result, grad, did_finish = eval_grad_tree_array(expression, X, operators; variable=false)
```

or with respect to variables, and only in a single direction:

```julia
feature = 2
result, grad, did_finish = eval_diff_tree_array(expression, X, operators, feature)
```

## Generic types

> Does this work for only scalar operators on real numbers, or will it work for `MyCrazyType`?

I'm so glad you asked. `DynamicExpressions.jl` actually will work for **arbitrary types**! However, to work on operators other than real scalars, you need to use the `GenericOperatorEnum <: AbstractOperatorEnum` instead of the normal `OperatorEnum`. Let's try it with strings!

```julia
x1 = Node(String; feature=1) 
```

This node, will be used to index input data (whatever it may be) with either `data[feature]` (1D abstract arrays) or `selectdim(data, 1, feature)` (ND abstract arrays). Let's now define some operators to use:

```julia
my_string_func(x::String) = "ello $x"

operators = GenericOperatorEnum(;
    binary_operators=[*],
    unary_operators=[my_string_func]
)
```

Now, let's extend our operators to work with the
expression types used by `DynamicExpressions.jl`:

```julia
@extend_operators operators
```

Now, let's create an expression:

```julia
tree = "H" * my_string_func(x1)
# ^ `(H * my_string_func(x1))`

tree(["World!", "Me?"], operators)
# Hello World!
```

So indeed it works for arbitrary types. It is a bit slower due to the potential for type instability, but it's not too bad:

```julia
@btime tree(["Hello", "Me?"], operators)
# 1738 ns
``` 

## Tensors

> Does this work for tensors, or even unions of scalars and tensors?

Also yes! Let's see:

```julia
using DynamicExpressions

T = Union{Float64,Vector{Float64}}

c1 = Node(T; val=0.0)  # Scalar constant
c2 = Node(T; val=[1.0, 2.0, 3.0])  # Vector constant
x1 = Node(T; feature=1)

# Some operators on tensors (multiple dispatch can be used for different behavior!)
vec_add(x, y) = x .+ y
vec_square(x) = x .* x

# Set up an operator enum:
operators = GenericOperatorEnum(;binary_operators=[vec_add], unary_operators=[vec_square])
@extend_operators operators

# Construct the expression:
tree = vec_add(vec_add(vec_square(x1), c2), c1)

X = [[-1.0, 5.2, 0.1], [0.0, 0.0, 0.0]]

# Evaluate!
tree(X, operators)  # [2.0, 29.04, 3.01]
```

Note that if an operator is not defined for the particular input, `nothing` will be returned instead.

This is all still pretty fast, too:

```julia
@btime tree(X, operators)
# 2,949 ns
@btime eval(:(vec_add(vec_add(vec_square(X[1]), [1.0, 2.0, 3.0]), 0.0)))
# 115,000 ns
```
