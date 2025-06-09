# DynamicExpressions.jl v2.0.0 Upgrade Guide

This guide covers the breaking changes introduced in DynamicExpressions.jl v2.0.0 and how to migrate your code from v1.x.

## Table of Contents

1. [Overview](#overview)
2. [Major Breaking Changes](#major-breaking-changes)
3. [Evaluation Syntax](#evaluation-syntax)
4. [Node Constructor Changes](#node-constructor-changes)
5. [Function and Module Renames](#function-and-module-renames)
6. [OperatorEnum Constructor Changes](#operatorenum-constructor-changes)
7. [Migration Checklist](#migration-checklist)
8. [Examples](#examples)

## Overview

Version 2.0.0 introduces several breaking changes to improve the API consistency, type safety, and performance of DynamicExpressions.jl. The most significant changes involve:

- **Explicit operator passing**: Evaluation now requires explicitly passing operators
- **Simplified constructors**: Node constructors use keyword arguments instead of positional
- **Consistent naming**: Module and function names have been standardized
- **Improved operator enum creation**: More intuitive constructor syntax

## Major Breaking Changes

### 1. Evaluation Syntax

**🚨 BREAKING CHANGE**: Tree evaluation now requires explicitly passing operators.

#### Before (v1.x)
```julia
operators = OperatorEnum(1 => (sin, cos), 2 => (+, -, *, /))
tree = Node{Float64}(; op=1, children=(Node{Float64}(; feature=1),))
X = randn(2, 100)

# Old syntax (deprecated)
result = tree(X)
gradient = tree'(X)
```

#### After (v2.0.0)
```julia
operators = OperatorEnum(1 => (sin, cos), 2 => (+, -, *, /))
tree = Node{Float64}(; op=1, children=(Node{Float64}(; feature=1),))
X = randn(2, 100)

# New syntax (required)
result = tree(X, operators)
gradient = tree'(X, operators)
```

**Why this change?** 
- Eliminates global state dependencies
- Makes operator dependencies explicit
- Improves thread safety
- Enables better type inference

### 2. Node Constructor Changes

**🚨 BREAKING CHANGE**: Node constructors now use keyword-only arguments.

#### Before (v1.x)
```julia
# Constant node
node = Node(1, true, 3.14)
# or with type
node = Node(Float64, 1, true, 3.14)

# Feature node  
node = Node(Float64, 1, false, nothing, 2)

# Unary operation
node = Node(1, false, nothing, 0, 1, child_node)

# Binary operation
node = Node(2, false, nothing, 0, 1, left_child, right_child)
```

#### After (v2.0.0)
```julia
# Constant node
node = Node{Float64}(; val=3.14)

# Feature node
node = Node{Float64}(; feature=2)

# Unary operation
node = Node(; op=1, l=child_node)
# or more explicitly
node = Node(; op=1, children=(child_node,))

# Binary operation  
node = Node(; op=1, l=left_child, r=right_child)
# or more explicitly
node = Node(; op=1, children=(left_child, right_child))
```

**Why this change?**
- More readable and self-documenting
- Eliminates positional argument confusion
- Supports future extensibility
- Follows Julia conventions

### 3. Function and Module Renames

Several functions and modules have been renamed for consistency:

#### Function Renames
```julia
# Before (v1.x) → After (v2.0.0)
simplify_tree(tree, operators) → simplify_tree!(tree, operators)
count_constants(tree) → count_constant_nodes(tree)
index_constants(tree) → index_constant_nodes(tree)
get_constants(tree) → get_scalar_constants(tree)
set_constants!(tree, constants, refs) → set_scalar_constants!(tree, constants, refs)
create_evaluation_helpers!(operators) → set_default_operators!(operators)
```

#### Module Renames
```julia
# Before (v1.x) → After (v2.0.0)
EquationModule → NodeModule
EquationUtilsModule → NodeUtilsModule
EvaluateEquationModule → EvaluateModule
EvaluateEquationDerivativeModule → EvaluateDerivativeModule
SimplifyEquationModule → SimplifyModule
```

### 4. OperatorEnum Constructor Changes

**🚨 BREAKING CHANGE**: OperatorEnum constructor syntax has been modernized.

#### Before (v1.x)
```julia
# Old keyword-based syntax
operators = OperatorEnum(
    binary_operators=[+, -, *, /],
    unary_operators=[sin, cos, exp]
)
```

#### After (v2.0.0)
```julia
# New degree-based syntax
operators = OperatorEnum(
    1 => (sin, cos, exp),
    2 => (+, -, *, /)
)
```

**Why this change?**
- Supports n-ary operators naturally
- More explicit about operator arity
- Consistent with GenericOperatorEnum
- Better performance due to compile-time type stability

## Migration Checklist

Use this checklist to ensure your code is fully migrated:

- [ ] **Update evaluation calls**: Add operators parameter to all `tree(X)` calls
- [ ] **Update gradient calls**: Add operators parameter to all `tree'(X)` calls  
- [ ] **Update Node constructors**: Replace positional arguments with keyword arguments
- [ ] **Update function names**: Replace deprecated function names with new ones
- [ ] **Update module imports**: Replace old module names with new ones
- [ ] **Update OperatorEnum creation**: Use new degree-based syntax
- [ ] **Remove global operator dependencies**: Ensure operators are passed explicitly
- [ ] **Test thoroughly**: Run your test suite to catch any missed changes

## Examples

### Complete Migration Example

#### Before (v1.x)
```julia
using DynamicExpressions
using DynamicExpressions: EvaluateEquationModule

# Create operators (old syntax)
operators = OperatorEnum(
    binary_operators=[+, -, *, /],
    unary_operators=[sin, cos]
)

# Create expression tree (old constructor)
x1 = Node(Float64, 1, false, nothing, 1)  # feature 1
const_node = Node(Float64, 1, true, 3.14)  # constant
tree = Node(2, false, nothing, 0, 1, x1, const_node)  # x1 + 3.14

# Evaluate (old syntax)
X = randn(2, 100)
result = tree(X)
grad = tree'(X)

# Simplify (old function name)
simplified = simplify_tree(tree, operators)
```

#### After (v2.0.0)
```julia
using DynamicExpressions
using DynamicExpressions: EvaluateModule

# Create operators (new syntax)
operators = OperatorEnum(
    1 => (sin, cos),
    2 => (+, -, *, /)
)

# Create expression tree (new constructor)
x1 = Node{Float64}(; feature=1)  # feature 1
const_node = Node{Float64}(; val=3.14)  # constant  
tree = Node(; op=1, l=x1, r=const_node)  # x1 + 3.14 (op=1 is first binary operator: +)

# Evaluate (new syntax)
X = randn(2, 100)
result = tree(X, operators)
grad = tree'(X, operators)

# Simplify (new function name)
simplified = simplify_tree!(copy(tree), operators)
```

### Expression-based API (Recommended)

For new code, consider using the Expression API which handles operators automatically:

```julia
using DynamicExpressions

operators = OperatorEnum(1 => (sin, cos), 2 => (+, -, *, /))
variable_names = ["x1", "x2"]

# Create expressions
x1 = Expression(Node{Float64}(; feature=1); operators, variable_names)
x2 = Expression(Node{Float64}(; feature=2); operators, variable_names)

# Build complex expressions naturally
expr = x1 + sin(x2) * 3.14

# Evaluate without explicitly passing operators
X = randn(2, 100)
result = expr(X)
grad = expr'(X)
```

## Additional Notes

### Deprecation Warnings

Version 2.0.0 includes deprecation warnings for old syntax to help with migration. These warnings will guide you to the correct new syntax:

```julia
# This will show a deprecation warning:
result = tree(X)
# Warning: The `tree(X; kws...)` syntax is deprecated. Use `tree(X, operators; kws...)` instead.
```

### Performance Implications

The new explicit operator passing may seem like it adds overhead, but it actually:
- Eliminates global state lookups
- Enables better compiler optimizations  
- Improves type stability
- Results in faster execution for most use cases

### Backward Compatibility

Version 2.0.0 maintains backward compatibility through deprecation warnings, but these will be removed in a future version. It's recommended to migrate as soon as possible.

## Getting Help

If you encounter issues during migration:

1. Check the deprecation warnings - they provide specific guidance
2. Review the [API documentation](https://symbolicml.org/DynamicExpressions.jl/dev)
3. Open an issue on [GitHub](https://github.com/SymbolicML/DynamicExpressions.jl) if you need assistance
4. Consider using the Expression API for new projects as it's more user-friendly

---

**Happy coding with DynamicExpressions.jl v2.0.0! 🚀**