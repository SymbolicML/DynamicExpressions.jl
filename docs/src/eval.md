# Evaluation

Given an expression tree specified with a `Node` type, you may evaluate the expression
over an array of data with the following command:

```@docs
eval_tree_array(tree::Node{T}, cX::AbstractMatrix{T}, operators::OperatorEnum) where {T<:Number}
```

Assuming you are only using a single `OperatorEnum`, you can also use
the following shorthand by using the expression as a function:

```
    (tree::Node)(X::AbstractMatrix{T}, operators::OperatorEnum; turbo::Bool=false)

Evaluate a binary tree (equation) over a given data matrix. The
operators contain all of the operators used in the tree.

# Arguments
- `X::AbstractMatrix{T}`: The input data to evaluate the tree on.
- `operators::OperatorEnum`: The operators used in the tree.
- `turbo::Bool`: Use `LoopVectorization.@turbo` for faster evaluation.

# Returns
- `output::AbstractVector{T}`: the result, which is a 1D array.
    Any NaN, Inf, or other failure during the evaluation will result in the entire
    output array being set to NaN.
```

For example,

```@example
using DynamicExpressions

operators = OperatorEnum(; binary_operators=[+, -, *], unary_operators=[cos])
tree = Node(; feature=1) * cos(Node(; feature=2) - 3.2)

tree([1 2 3; 4 5 6.], operators)
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
eval_tree_array(tree::Node, cX::AbstractMatrix, operators::GenericOperatorEnum; throw_errors::Bool=true)
```

Likewise for the shorthand notation:

```
    (tree::Node)(X::AbstractMatrix, operators::GenericOperatorEnum; throw_errors::Bool=true)

# Arguments
- `X::AbstractArray`: The input data to evaluate the tree on.
- `operators::GenericOperatorEnum`: The operators used in the tree.
- `throw_errors::Bool=true`: Whether to throw errors
    if they occur during evaluation. Otherwise,
    MethodErrors will be caught before they happen and 
    evaluation will return `nothing`,
    rather than throwing an error. This is useful in cases
    where you are unsure if a particular tree is valid or not,
    and would prefer to work with `nothing` as an output.

# Returns
- `output`: the result of the evaluation.
    If evaluation failed, `nothing` will be returned for the first argument.
    A `false` complete means an operator was called on input types
    that it was not defined for. You can change this behavior by
    setting `throw_errors=false`.
```

## Derivatives

`DynamicExpressions.jl` can efficiently compute first-order derivatives
of expressions with respect to variables or constants. This is done using
either `eval_diff_tree_array`, to compute derivative with respect to a single
variable, or with `eval_grad_tree_array`, to compute the gradient with respect
all variables (or, all constants). Both use forward-mode automatic, but use
`Zygote.jl` to compute derivatives of each operator, so this is very efficient.

```@docs
eval_diff_tree_array(tree::Node{T}, cX::AbstractMatrix{T}, operators::OperatorEnum, direction::Integer) where {T<:Number}
eval_grad_tree_array(tree::Node{T}, cX::AbstractMatrix{T}, operators::OperatorEnum; turbo::Bool=false, variable::Bool=false) where {T<:Number}
```

You can compute gradients this with shorthand notation as well (which by default computes
gradients with respect to input matrix, rather than constants).

```
    (tree::Node{T})'(X::AbstractMatrix{T}, operators::OperatorEnum; turbo::Bool=false, variable::Bool=true)

Compute the forward-mode derivative of an expression, using a similar
structure and optimization to eval_tree_array. `variable` specifies whether
we should take derivatives with respect to features (i.e., X), or with respect
to every constant in the expression.

# Arguments
- `X::AbstractMatrix{T}`: The data matrix, with each column being a data point.
- `operators::OperatorEnum`: The operators used to create the `tree`. Note that `operators.enable_autodiff`
    must be `true`. This is needed to create the derivative operations.
- `variable::Bool`: Whether to take derivatives with respect to features (i.e., `X` - with `variable=true`),
    or with respect to every constant in the expression (`variable=false`).
- `turbo::Bool`: Use `LoopVectorization.@turbo` for faster evaluation.

# Returns

- `(evaluation, gradient, complete)::Tuple{AbstractVector{T}, AbstractMatrix{T}, Bool}`: the normal evaluation,
    the gradient, and whether the evaluation completed as normal (or encountered a nan or inf).
```

## Printing

You can also print a tree as follows:

```@docs
string_tree(tree::Node, operators::AbstractOperatorEnum)
```

When you define an `OperatorEnum`, the standard `show` and `print` methods
will be overwritten to use `string_tree`.
