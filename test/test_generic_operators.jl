using DynamicExpressions
using Test

## Test operators on strings:

# Binary operators on strings:
string_concat(x::String, y::String) = "$x($y)"
string_arg(x::String, y::String) = "$x($y)"
string_remove_head(x::String) = x[2:end]
string_remove_tail(x::String) = x[1:(end - 1)]

operators = GenericOperatorEnum(;
    binary_operators=(*, string_concat, string_arg),
    unary_operators=(string_remove_head, string_remove_tail),
    extend_local_operators=true,
)

x1, x2, x3 = [Node(String; feature=i) for i in 1:3]
tree = string_concat(x1, " ") * "World!"
@test repr(tree) == "(string_concat(x1,  ) * World!)"

tree = x1 * " " * "World!"
@test tree(["Hello"]) == "Hello World!"
