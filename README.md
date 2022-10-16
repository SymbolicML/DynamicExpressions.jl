<div align="center">

# DynamicExpressions.jl

Blazingly-fast dynamic expressions.

</div>

DynamicExpressions.jl is the backbone of 
[SymbolicRegression.jl](https://github.com/MilesCranmer/SymbolicRegression.jl) and
[PySR](https://github.com/MilesCranmer/PySR).

Example:
```julia
using DynamicExpressions

operators = OperatorEnum(; binary_operators=[+, -, *], unary_operators=[cos])

x1 = Node(; feature=1)
x2 = Node(; feature=2)

expression = x1 * cos(x2 - 3.2)

X = randn(Float64, 2, 1000);
eval_tree_array(expression, X, operators)
```
This evaluation is extremely fast, without needing to compile
the expression!
