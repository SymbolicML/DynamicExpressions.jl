# API

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

## Nodes

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
convert(::Type{N1}, tree::N2) where {T1,T2,D1,D2,N1<:AbstractExpressionNode{T1,D1},N2<:AbstractExpressionNode{T2,D2}}
```

You can set a `tree` (in-place) with `set_node!`:

```@docs
set_node!
```

You can create a copy of a node with `copy_node`:

```@docs
copy_node
```

## Generic Node Accessors

For working with nodes of arbitrary arity:

```@docs
get_child
set_child!
get_children
set_children!
```

Examples:

```julia
# Define operators including ternary
my_ternary(x, y, z) = x + y * z
operators = OperatorEnum(((sin,), (+, *), (my_ternary,)))  # (unary, binary, ternary)

tree = Node{Float64,3}(; op=1, children=(Node{Float64,3}(; val=1.0), Node{Float64,3}(; val=2.0)))
new_child = Node{Float64,3}(; val=3.0)

left_child = get_child(tree, 1)
right_child = get_child(tree, 2)

set_child!(tree, new_child, 1)

children = get_children(tree)
left, right = get_children(tree, Val(2))  # type stable

# Transform to ternary operation
child1, child2, child3 = Node{Float64,3}(; val=4.0), Node{Float64,3}(; val=5.0), Node{Float64,3}(; val=6.0)
set_children!(tree, (child1, child2, child3))
tree.op = 1  # my_ternary
tree.degree = 3
```

## Graph Nodes

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
julia> get_child(y, 2).val *= 0.9
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

## Expressions

A higher-level user-facing type is the `Expression`:

```@docs
Expression
```

This is a subtype of `AbstractExpression`.

```@docs
AbstractExpression
```

which can be used for defining custom types, such as the `ParametricExpression`:

```@docs
ParametricExpression
ParametricNode
```

Another example is the `StructuredExpression` type, for defining rigid
predefined operations in an expression tree:

```@docs
StructuredExpression
```

You may use operators directly on `AbstractExpression` objects to create a new object
containing the combined expression tree, so long as those objects have identical operators
in their metadata.

You can extract and set contents and metadata with a few utility functions, including:

```@docs
get_contents
with_contents
get_metadata
with_metadata
get_tree
```

To declare a new operator for expressions, you may use:

```@docs
@declare_expression_operator
```

## Interfaces

The interfaces for `AbstractExpression` and `AbstractExpressionNode` are
tested using Interfaces.jl. You can see the interfaces with:

```@docs
DynamicExpressions.ExpressionInterface
DynamicExpressions.NodeInterface
```

You can declare a new type as implementing these with, e.g.,

```julia
using DynamicExpressions: ExpressionInterface, all_ei_methods_except
using Interface: @implements, Arguments, Interface

# Add all optional methods:
valid_optional_methods = all_ei_methods_except(())

@implements ExpressionInterface{valid_optional_methods} MyCustomExpression [Arguments()]
```

You can then test the interface is implemented correctly using, for example,

```julia
@test Interface.test(ExpressionInterface, MyCustomExpression, [ex::MyCustomExpression])
```

Note that this may not flag all potential issues, so be sure to still read the details about
what methods can be implemented and customized.
