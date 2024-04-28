using DynamicExpressions
using Test

function my_custom_op(x, y)
    return x + y
end

operators = OperatorEnum(;
    binary_operators=[+, -, *, /, my_custom_op],
    unary_operators=[cos, sin],
    define_helper_functions=false,
)

# Clear operators
OperatorEnum(; binary_operators=[/])

let
    ex = @parse_expression(
        my_custom_op(x, sin(y) + 0.3), operators = operators, variable_names = ["x", "y"],
    )

    @test typeof(ex) <: Expression

    s = sprint((io, e) -> show(io, MIME("text/plain"), e), ex)
    @test s == "my_custom_op(x, sin(y) + 0.3)"

    # Whereas the regular use would fail
    if VERSION >= v"1.9"
        @test_throws ErrorException @eval let
            x = Node{Float64}(; feature=1)
            y = Node{Float64}(; feature=2)

            my_custom_op(x, sin(y) + 0.3)
        end
        @test_throws "Convenience constructor for operator `sin` is out-of-date" @eval let
            x = Node{Float64}(; feature=1)
            y = Node{Float64}(; feature=2)

            sin(y) + 0.3
        end
    end
end

# Can also parse just a float
let
    ex = @parse_expression(1.0, operators = operators, variable_names = [])
    @test typeof(ex) <: Expression
    @test ex.tree.val == 1.0
    @test typeof(ex.tree) <: Node{Float64}
end

# Or an int
let
    ex = @parse_expression(1, operators = operators, variable_names = [])
    @test typeof(ex.tree) <: Node{Int64}
end

# Or just a variable
let
    ex = @parse_expression(x, operators = operators, variable_names = ["x"])
    @test typeof(ex.tree) <: Node{Float32}
end

# Or, with custom node types
let
    ex = @parse_expression(
        x, operators = operators, variable_names = ["x"], node_type = GraphNode
    )
    @test typeof(ex.tree) <: GraphNode{Float32}
end

# Should work with symbols for variable names too
let
    ex = @parse_expression(
        cos(exp(α)),
        operators = OperatorEnum(; unary_operators=[cos, exp]),
        variable_names = [:α]
    )
    @test typeof(ex.tree) <: Node{Float32}
    s = sprint((io, e) -> show(io, MIME("text/plain"), e), ex)
    @test s == "cos(exp(α))"
end
