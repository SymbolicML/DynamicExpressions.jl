# DynamicExpressions.jl v2.0 Upgrade Guide

DynamicExpressions.jl v2.0 introduces support for n-arity operators (nodes with arbitrary numbers of children),
which required some breaking changes to implement.  This guide will help you migrate your code from v1.x to v2.0.

## Summary

- Types
    - `Node{T}` is now `Node{T,D}` where `D` is the maximum degree
    - `AbstractExpressionNode{T}` is now `AbstractExpressionNode{T,D}`
    - `AbstractNode` is now `AbstractNode{D}`
    - Before, `Node{T}` had fields `l::Node{T}` and `r::Node{T}`.
    - Now, the type is `Node{T,D}`, and it has the field `children::NTuple{D,Nullable{Node{T,D}}}`.
- Accessors
    - You can now access children by index with `get_child(tree, i)`
        - `tree.l` can now be written as `get_child(tree, 1)`
        - `tree.r` can now be written as `get_child(tree, 2)`
        - _note: you can access multiple children with `get_children(tree, Val(degree))`_
    - You can now set children by index with `set_child!(tree, child, i)`
        - `tree.l = child` should now be written as `set_child!(tree, child, 1)`
        - `tree.r = child` should now be written as `set_child!(tree, child, 2)`
        - _note: you can set multiple children with `set_children!(tree, children)`_
- Constructors
    - `Node{T}(; op=1, l=x)` should now be written as `Node{T}(; op=1, children=(x,))`
    - `Node{T}(; op=1, l=x, r=y)` should now be written as `Node{T}(; op=1, children=(x, y))`
    - You may now use `Node{T,D}(; op=1, children=(x,))` to specify degree other than the default of 2.
- OperatorEnum
    - `OperatorEnum` (and `GenericOperatorEnum`) now uses a single `ops` field: this is a tuple of tuples, indexed by arity.
        - `operators.unaops` is now written as `operators.ops[1]`, and `operators.binops` is now written as `operators.ops[2]`.
    - `OperatorEnum(binary_operators=(+, -, *), unary_operators=(sin, cos))` can now be written as `OperatorEnum(2 => (+, -, *), 1 => (sin, cos))`
        - This API permits higher-arity operators: `OperatorEnum(1 => (sin, cos), 2 => (+, -, *), 3 => (fma, max))`.

## Breaking Changes

The main breaking change is that `Node{T}` is now `Node{T,D}` where `D` is the
**maximum degree** of any possible node in the tree. `node.degree` is _still_ the same as before,
and is such that `node.degree <= D`.

Similarly, `AbstractExpressionNode{T}` is now `AbstractExpressionNode{T,D}`,
and `AbstractNode` is now `AbstractNode{D}`.

Before, `Node{T}` had fields `l::Node{T}` and `r::Node{T}`. Now, it has a single combined
field `children::NTuple{D,Nullable{Node{T,D}}}`. This is a tuple of wrapped node objects,
which should be accessed with `get_child(tree, i)` and set with `set_child!(tree, child, i)`.
However, the old getters and setters will still function for binary trees (`.l` and `.r`).

You may now use `Node{T,D}(; op=1, children=(x,))` to specify degree other than the default of 2.
However, the default `Node{T}(; op=1, children=(x,))` is still available and will result
in type `Node{T,2}`.

### Necessary Changes to Your Code

The main breaking change that requires some modifications is patterns that
explicitly match `tree.degree` in conditional logic. The `tree.degree == 0`
branch can be left alone, but higher arity nodes should be generalized.

For code like this:

```julia
# This pattern ONLY works for binary trees (degree ≤ 2)
if tree.degree == 0
    # leaf node
elseif tree.degree == 1  
    # unary operator
else  # tree.degree == 2  # <-- This violates the assumption in 2.0
    # binary operator
end
```

You have two options for upgrading

1. Constrain your type signatures: Use `::AbstractExpressionNode{T,2}` to only
    accept binary trees, and refuse higher-arity nodes

    ```julia
    function my_function(tree::AbstractExpressionNode{T,2}) where T
        if tree.degree == 0
            # leaf
        elseif tree.degree == 1
            # unary  
        else  # tree.degree == 2, guaranteed
            # binary
        end
    end
    ```

2. Rewrite your code to be more generic. (_Note that for recursive algorithms, you can often do things with a `tree_mapreduce`, which already handles the general case._)

    ```julia
    # 2: Handle arbitrary arity
    function my_function(tree::AbstractExpressionNode{T}) where T
        if tree.degree == 0
            # leaf
        else # higher arity
            deg = tree.degree
            for i in 1:deg
                child = get_child(tree, i)
                # process child...
            end
        end
    end
    ```

    However, normally what is done internally for max efficiency for the general approach is to use patterns like:

    ```julia
    @generated function my_function(tree::AbstractExpressionNode{T,D}) where {T,D}
        quote
            deg = tree.degree
            deg == 0 && process_leaf(tree)
            Base.Cartesian.@nif(
                $deg,
                i -> i == deg,
                i -> let children = get_children(tree, Val(i))
                    # Now, `children` is a type-stable tuple of children
                end,
            )
        end
    end
    ```

    Note that the `@generated` is needed to pass `D` to the Cartesian macro.

## Property Access (Non-Breaking)

Note: `.l` and `.r` property access still work and will continue to be supported on types with `D == 2`. However, the generic accessors are more flexible, so upgrading to them is recommended.

```julia
# old_child = tree.l
old_child = get_child(tree, 1)

# tree.r = new_child
set_child!(tree, new_child, 2)
```

This lets you write code that prescribes arbitrary arity.

## Node Construction (Non-Breaking)

For binary trees, you can still use the syntax:

```julia
x = Node{Float64}(; feature=1)
tree = Node{Float64}(; op=1, children=(x,))
```

For higher-arity trees, you may pass `D` to specify the maximum degree in the tree:

```julia
x = Node{Float64,3}(; feature=1)
y = Node{Float64,3}(; feature=2)
z = Node{Float64,3}(; feature=3)
tree = Node{Float64,3}(; op=1, children=(x, y, z))
```

## OperatorEnum redesign (Non-Breaking)

`OperatorEnum` (and `GenericOperatorEnum`) now uses a single `ops` field: this is a tuple of tuples, indexed by arity.
`operators.unaops` is now written as `operators.ops[1]`, and `operators.binops` is now written as `operators.ops[2]`.

However, the properties are aliased, so the old syntax will still work.

Along with this, there is a new API for constructing `OperatorEnum`s:

```julia
# operators = OperatorEnum(binary_operators=(+, -, *), unary_operators=(sin, cos))  # old
operators = OperatorEnum(2 => (+, -, *), 1 => (sin, cos))
```

This API permits higher-arity operators:

```julia
operators = OperatorEnum(1 => (sin, cos), 2 => (+, -, *), 3 => (fma, max))
```

(Note that the order you pass the pairs is not important.)
