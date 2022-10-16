# Evaluation

Given an expression tree specified with the `Node` type, you may evaluate the expression
over an array of data with the following operation:

```@docs
eval_tree_array(tree::Node{T}, cX::AbstractMatrix{T}, operators::OperatorEnum) where {T<:Real}
```

## Derivatives

`DynamicExpressions.jl` can efficiently compute first-order derivatives
of expressions with respect to variables or constants. This is done using
either `eval_diff_tree_array`, to compute derivative with respect to a single
variable, or with `eval_grad_tree_array`, to compute the gradient with respect
all variables (or, all constants). Both use forward-mode automatic, but use
`Zygote.jl` to compute derivatives of each operator, so this is very efficient.

```@docs
eval_diff_tree_array(tree::Node{T}, cX::AbstractMatrix{T}, operators::OperatorEnum, direction::Int) where {T<:Real}
eval_grad_tree_array(tree::Node{T}, cX::AbstractMatrix{T}, operators::OperatorEnum; variable::Bool=false) where {T<:Real}
```

Alternatively, you can compute higher-order derivatives by using `ForwardDiff` on
the function `differentiable_eval_tree_array`, although this will be slower.

```@docs
differentiable_eval_tree_array(tree::Node{T}, cX::AbstractMatrix{T}, operators::OperatorEnum) where {T<:Real}
```

## Printing

You can also print a tree as follows:

```@docs
string_tree(tree::Node, operators::OperatorEnum)
```

When you define an `OperatorEnum`, the standard `show` and `print` methods
will be overwritten to use `string_tree`.
