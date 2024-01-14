# Types

## Operator Enum

All equations are represented as a tree of operators. Each node in this tree
specifies its operator with an integer - which indexes an `enum` of operators.
This `enum` is defined as follows:

```@docs
OperatorEnum
```

Construct this operator specification as follows:

```@docs
OperatorEnum(; binary_operators=[], unary_operators=[], define_helper_functions::Bool=true)
```

This is just for scalar operators. However, you can use
the following for more general operators:

```@docs
GenericOperatorEnum(; binary_operators=[], unary_operators=[], define_helper_functions::Bool=true)
```

By default, these operators will define helper functions for constructing trees,
so that you can write `Node(;feature=1) + Node(;feature=2)` instead of
`Node(1, Node(;feature=1), Node(;feature=2))` (assuming `+` is the first operator).
You can turn this off with `define_helper_functions=false`.

For other operators *not* found in `Base`, including user-defined functions, you may
use the `@extend_operators` macro:

```@docs
@extend_operators operators
```

This will extend the operators you have passed to work with `Node` types, so that
it is easier to construct expression trees.

Note that you are free to use the `Node` constructors directly.
This is a more robust approach, and should be used when creating libraries
which use `DynamicExpressions.jl`.

## Equations

Equations are specified as binary trees with the `Node` type, defined
as follows:

```@docs
Node
```

When you create an `Options` object, the operators
passed are also re-defined for `Node` types.
This allows you use, e.g., `t=Node(; feature=1) * 3f0` to create a tree, so long as
`*` was specified as a binary operator.

When using these node constructors, types will automatically be promoted.
You can convert the type of a node using `convert`:

```@docs
convert(::Type{AbstractExpressionNode{T1}}, tree::AbstractExpressionNode{T2}) where {T1, T2}
```

You can set a `tree` (in-place) with `set_node!`:

```@docs
set_node!
```

You can create a copy of a node with `copy_node`:

```@docs
copy_node
```

## Graph-Like Equations

You can describe an equation as a *graph* rather than a tree
by using the `GraphNode` type:

```@docs
GraphNode{T}
```

This makes it so you can have multiple parents for a given node,
and share parts of an expression. For example:

```julia
julia> operators = OperatorEnum(;
           binary_operators=[+, -, *], unary_operators=[cos, sin, exp]
       );

julia> x1, x2 = GraphNode(feature=1), GraphNode(feature=2)
(x1, x2)

julia> y = sin(x1) + 1.5
sin(x1) + 1.5

julia> z = exp(y) + y
exp(sin(x1) + 1.5) + {(sin(x1) + 1.5)}
```

Here, the curly braces `{}` indicate that the node
is shared by another (or more) parent node.

This means that we only need to change it once
to have changes propagate across the expression:

```julia
julia> y.r.val *= 0.9
1.35

julia> z
exp(sin(x1) + 1.35) + {(sin(x1) + 1.35)}
```

This also means there are fewer nodes to describe an expression:

```julia
julia> length(z)
6

julia> length(convert(Node, z))
10
```

where we have converted the `GraphNode` to a `Node` type,
which breaks shared connections into separate nodes.

## Abstract Types

Both the `Node` and `GraphNode` types are subtypes of the abstract type:

```@docs
AbstractExpressionNode{T}
```

which can be used to create additional expression-like types.
The supertype of this abstract type is the `AbstractNode` type,
which is more generic but does not have all of the same methods:

```@docs
AbstractNode{T}
```
