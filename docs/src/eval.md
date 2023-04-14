# Evaluation

Given an expression tree specified with a `Node` type, you may evaluate the expression
over an array of data with the following command:

```@docs
eval_tree_array(tree::Node{T}, cX::AbstractMatrix{T}, operators::OperatorEnum) where {T<:Number}
```

Assuming you are only using a single `OperatorEnum`, you can also use
the following short-hand by using the expression as a function:

```@example
operators = OperatorEnum(; binary_operators=[+, -, *], unary_operators=[cos])
tree = Node(; feature=1) * cos(Node(; feature=2) - 3.2)

tree(X)
```

This is possible because when you call `OperatorEnum`, it automatically re-defines
`(::Node)(X)` to call the evaluation operation with the given `operators loaded.
It also re-defines `print`, `show`, and the various operators, to work with the `Node` type.

!!! warning

    The `Node` type does not know about which `OperatorEnum` you used to create it.
    Thus, if you define an expression with one `OperatorEnum`, and then try to
    evaluate it or print it with a different `OperatorEnum`, you will get undefined behavior!

You can also work with arbitrary types, by defining a `GenericOperatorEnum` instead.
The notation is the same for `eval_tree_array`, though it will return `nothing`
when it can't find a method, and not do any NaN checks:

```@docs
    eval_tree_array(tree, cX::AbstractArray, operators::GenericOperatorEnum; throw_errors::Bool=true)
```

## Derivatives

`DynamicExpressions.jl` can efficiently compute first-order derivatives
of expressions with respect to variables or constants. This is done using
either `eval_diff_tree_array`, to compute derivative with respect to a single
variable, or with `eval_grad_tree_array`, to compute the gradient with respect
all variables (or, all constants). Both use forward-mode automatic, but use
`Zygote.jl` to compute derivatives of each operator, so this is very efficient.

```@docs
eval_diff_tree_array(tree::Node{T}, cX::AbstractMatrix{T}, operators::OperatorEnum, direction::Int) where {T<:Number}
eval_grad_tree_array(tree::Node{T}, cX::AbstractMatrix{T}, operators::OperatorEnum; variable::Bool=false) where {T<:Number}
```

Alternatively, you can compute higher-order derivatives by using `ForwardDiff` on
the function `differentiable_eval_tree_array`, although this will be slower.

```@docs
differentiable_eval_tree_array(tree::Node{T}, cX::AbstractMatrix{T}, operators::OperatorEnum) where {T<:Number}
```

## Printing

You can also print a tree as follows:

```@docs
string_tree(tree::Node, operators::AbstractOperatorEnum)
```

When you define an `OperatorEnum`, the standard `show` and `print` methods
will be overwritten to use `string_tree`.
