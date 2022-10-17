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
OperatorEnum(; binary_operators, unary_operators)
```

## Equations

Equations are specified as binary trees with the `Node` type, defined
as follows:

```@docs
Node{T<:Real}
```

There are a variety of constructors for `Node` objects, including:

```@docs
Node(; val::Real=nothing, feature::Integer=nothing)
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
