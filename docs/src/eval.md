# Evaluation & Derivatives

## Evaluation

Given an expression tree specified with a `Node` type, you may evaluate the expression
over an array of data with the following command:

```@docs
eval_tree_array(tree::Node{T}, cX::AbstractMatrix{T}, operators::OperatorEnum) where {T<:Number}
```

Assuming you are only using a single `OperatorEnum`, you can also use
the following shorthand by using the expression as a function:

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
- `operators::OperatorEnum`: The operators used to create the `tree`.
- `variable::Bool`: Whether to take derivatives with respect to features (i.e., `X` - with `variable=true`),
    or with respect to every constant in the expression (`variable=false`).
- `turbo::Bool`: Use `LoopVectorization.@turbo` for faster evaluation.

# Returns

- `(evaluation, gradient, complete)::Tuple{AbstractVector{T}, AbstractMatrix{T}, Bool}`: the normal evaluation,
    the gradient, and whether the evaluation completed as normal (or encountered a nan or inf).
```

Alternatively, you can compute higher-order derivatives by using `ForwardDiff` on
the function `differentiable_eval_tree_array`, although this will be slower.

```@docs
differentiable_eval_tree_array(tree::Node{T}, cX::AbstractMatrix{T}, operators::OperatorEnum) where {T<:Number}
```

### Enzyme

`DynamicExpressions.jl` also supports automatic differentiation with
[`Enzyme.jl`](https://github.com/EnzymeAD/Enzyme.jl). Note that this is
**extremely experimental**.
You should expect to see occasional incorrect gradients.
Be sure to explicitly verify gradients are correct for a particular
space of operators (e.g., with finite differences).

Let's look at an example. First, let's create a tree:

```julia
using DynamicExpressions

operators = OperatorEnum(binary_operators=(+, -, *, /), unary_operators=(cos, sin))

x1 = Node{Float64}(feature=1)
x2 = Node{Float64}(feature=2)

tree = 0.5 * x1 + cos(x2 - 0.2)
```

Now, say we want to take the derivative of this expression with respect to x1 and x2.
First, let's evaluate it normally:
```julia
X = [1.0 2.0 3.0; 4.0 5.0 6.0]  # 2x3 matrix (2 features, 3 rows)

tree(X, operators)
```

Now, let's use `Enzyme.jl` to compute the derivative of the outputs
with respect to x1 and x2, using reverse-mode autodiff:

```julia
using Enzyme

function my_loss_function(tree, X, operators)
    # Get the outputs
    y = tree(X, operators)
    # Sum them (so we can take a gradient, rather than a jacobian)
    return sum(y)
end


dX = begin
    storage=zero(X)
    autodiff(
        Reverse,
        my_loss_function,
        Active,
        ## Actual arguments to function:
        Const(tree),
        Duplicated(X, storage),
        Const(operators),
    )
    storage
end
```

This will get returned as

```text
 2×3 Matrix{Float64}:
  0.5       0.5       0.5
  0.611858  0.996165  0.464602
```

which one can confirm is the correct gradient!

This will take a while the first time you run it, as Enzyme needs to take the
gradients of the actual LLVM IR code. Subsequent runs won't spend any time compiling
and be much faster.

Some general notes about this:

1. We want to take a reverse-mode gradient, so we pass `Reverse` to `autodiff`.
2. Since we want to take the gradient of the _output_ of `my_loss_function`,
   we declare `Active` as the third argument.
3. Following this, we pass our actual arguments to the function.
    - Objects which we don't want to take gradients with respect to,
       and also don't temporarily store any data during the computation
       (such as `tree` and `operators` here) should be wrapped with `Const`.
    - Objects which we wish to take derivatives with respect to, we need to use
        `Duplicated`, and explicitly create a copy of it, with all numerical values
        set to zero. Enzyme will then store the derivatives in this object.

Note that you should never use anything other than `turbo=Val(false)` with Enzyme,
as Enzyme and LoopVectorization are not compatible, and will cause a segfault.
_Even using `turbo=false` will not work, because it would cause Enzyme to trace the (unused) LoopVectorization code!_
