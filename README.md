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
expression(X) # 1000-element Vector{Float64}
```

This evaluation is extremely fast, without us having to compile it.


For comparison:

```julia
@btime expression(X)
# 7.667 us
@btime X[1, :] .* cos.(X[2, :] .- 3.2)
# 2.389 us
```

Not bad at all! Only a 3x difference, even though we were not able to compile our expression.

Thus, this data structure makes it viable to dynamically construct expressions and then evaluate them.
This is therefore very useful for optimizing symbolic functional forms.
