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
OperatorEnum(; binary_operators=[], unary_operators=[], enable_autodiff::Bool=false, define_helper_functions::Bool=true)
```

This is just for scalar real operators. However, you can use
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
Node{T}
```

There are a variety of constructors for `Node` objects, including:

```@docs
Node(::Type{T}; val=nothing, feature::Integer=nothing) where {T}
Node(op::Int, l::Node)
Node(op::Int, l::Node, r::Node)
Node(var_string::String)
```

When you create an `Options` object, the operators
passed are also re-defined for `Node` types.
This allows you use, e.g., `t=Node(; feature=1) * 3f0` to create a tree, so long as
`*` was specified as a binary operator.

When using these node constructors, types will automatically be promoted.
You can convert the type of a node using `convert`:

```@docs
convert(::Type{Node{T1}}, tree::Node{T2}) where {T1, T2}
```

You can set a `tree` (in-place) with `set_node!`:

```@docs
set_node!(tree::Node{T}, new_tree::Node{T}) where {T}
```

You can create a copy of a node with `copy_node`:

```@docs
copy_node(tree::Node)
```
