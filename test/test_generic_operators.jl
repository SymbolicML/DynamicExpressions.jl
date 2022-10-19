using DynamicExpressions
using Test

## Test operators on strings:

operators = GenericOperatorEnum(;
    binary_operators=(*),
)

x1, x2, x3 = [Node(String; feature=i) for i in 1:3]
tree = x1 * " " * "World!"
@test tree(["Hello"]) == "Hello World!"
