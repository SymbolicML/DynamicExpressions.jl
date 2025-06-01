# CHANGELOG

## 2.0.0

- Nodes can now have arbitrary numbers of children, not just binary trees. This was designed in such a way to have identical performance to the previous version. Essentially, the `Node{T,D}` type is a wrapper around a tuple of `D` children.
    - Note that `Node{T}` automatically converts to `Node{T,2}` in many contexts, for backwards compatibility.
- Node type signature changed from `Node{T}` to `Node{T,D}`. Usually `D` is 2.
- Direct property access `.l` and `.r` is automatically forwarded to `get_child(tree, 1)` and `get_child(tree, 2)`,
    but it is recommended to use the generic accessors instead.
- Similarly, `.l = child` should be replaced with `set_child!(tree, child, 1)` and similar for `.r`.
- All internal code migrated to use generic accessors.

### Backwards Compatibility

- Existing `.l` and `.r` access continues to work without warnings

### Breaking Changes

The only breaking change is if:

1. You have any types that are subtyped to `<:AbstractExpressionNode{T}` or `<:AbstractNode{T}`. These should now be subtyped to `<:AbstractExpressionNode{T,2}` or `<:AbstractNode{T,2}`. You may also allow a `D` parameter in case you want to support higher-arity trees.
2. You assume a tree has type, e.g., `=== Node{T}`, rather than `<: Node{T}`. So any methods dispatched to `::Type{Node{T}}` will also break. (To be safe you should always use a form `<: Node{T}` in case of future type changes - in any library.)
3. You assume `tree.degree <= 2` in conditional logic, and your code interacts with a tree that is _not_ a binary tree. For example, the following pattern was common before this change:

    ```julia
    if tree.degree == 0
        #= operations on leaf node =#
    elseif tree.degree == 1
        #= operations on unary node =#
    else
        # BAD: ASSUMED TO BE BINARY
        #= operations on binary node, using `.l` and `.r` only =#
    end
    ```

    This will obviously break if you pass a tree that is not binary, such as `tree::Node{T,3}`.
    - To fix this, you can use the `get_children` function to get the children of the tree as a tuple of `D` children, and then index up to `tree.degree`. 
    - Inside DynamicExpressions, we commonly use `Base.Cartesian.@nif` to generate code for different degrees, to avoid any unstable types.
