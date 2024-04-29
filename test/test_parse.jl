using DynamicExpressions
using Test
using Suppressor

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

# This also works for parsing mixed types
let
    ex = @parse_expression(
        [1, 2, 3] * tan(cos(5 + x)),
        operators = GenericOperatorEnum(;
            binary_operators=[*, +], unary_operators=[tan, cos]
        ),
        variable_names = ["x"],
    )
    @test typeof(ex.tree) === Node{Any}
    @test typeof(ex.operators) <: GenericOperatorEnum
    s = sprint((io, e) -> show(io, MIME("text/plain"), e), ex)
    @test s == "[1, 2, 3] * tan(cos(5.0 + x))"

    # Can also force this to be a union type
    ex = @parse_expression(
        [1, 2, 3] * tan(cos(5 + x)),
        operators = GenericOperatorEnum(;
            binary_operators=[*, +], unary_operators=[tan, cos]
        ),
        variable_names = ["x"],
        node_type = Node{Union{Int,Vector{Int}}}
    )
    @test typeof(ex.tree) === Node{Union{Int,Vector{Int}}}
    @test typeof(ex.operators) <: GenericOperatorEnum
    s = sprint((io, e) -> show(io, MIME("text/plain"), e), ex)
    @test s == "[1, 2, 3] * tan(cos(5 + x))"
    # ^ Note how it does not convert to Float32 anymore

    # Nice error message:
    if VERSION >= v"1.9"
        @test_throws ErrorException ex(randn(1, 1))

        # Goes down to the first failure:
        @test_throws "Failed to evaluate tree [1, 2, 3] * tan(cos(5 + x1))" ex(randn(1, 1))

        stacktrace = try
            ex(randn(1, 1))
            @test false
        catch e
            sprint(show, current_exceptions())
        end
        @test occursin("Failed to evaluate tree [1, 2, 3] * tan(cos(5 + x1))", stacktrace)
        @test occursin("Failed to evaluate tree tan(cos(5 + x1))", stacktrace)
        @test occursin("Failed to evaluate tree 5 + x1", stacktrace)
    end

    # But, we can actually evaluate it for simple input
    @test ex([1.0]) ≈ [1, 2, 3] * tan(cos(5 + 1.0))
end

# Also check with tuple inputs
let
    ex = @parse_expression(
        x * (1.0, 2.0im) - cos(y),
        operators = GenericOperatorEnum(; binary_operators=[*, -], unary_operators=[cos]),
        variable_names = ["x", "y"],
        node_type = Node{Tuple{Float64,ComplexF64}}
    )
    s = sprint((io, e) -> show(io, MIME("text/plain"), e), ex)
    @test s == "(x * ((1.0, 0.0 + 2.0im))) - cos(y)"
    @test typeof(ex.tree) <: Node{Tuple{Float64,ComplexF64}}
end

show_type(x) = (show(typeof(x)); x)

let
    logged_out = @capture_out begin
        ex = @parse_expression(
            x * 2.5 - show_type(cos(y)),
            operators = OperatorEnum(; binary_operators=[*, -], unary_operators=[cos]),
            variable_names = [:x, :y],
            evaluate_on = [show_type],
        )
        s = sprint((io, e) -> show(io, MIME("text/plain"), e), ex)
        @test s == "(x * 2.5) - cos(y)"
    end
    @test logged_out == "Node{Float32}"
end

# Helpful errors for missing operator
let
    operators = OperatorEnum(; unary_operators=[sin])
    @test_throws ArgumentError @parse_expression(
        cos(x), operators = operators, variable_names = [:x]
    )
    if VERSION >= v"1.9"
        @test_throws "Unrecognized operator: `cos` with no matches in `(sin,)`." @parse_expression(
            cos(x), operators = operators, variable_names = [:x]
        )
    end
    operators = OperatorEnum(; binary_operators=[+])
    @test_throws ArgumentError @parse_expression(
        x * y, operators = operators, variable_names = [:x, :y]
    )
    if VERSION >= v"1.9"
        @test_throws "Unrecognized operator: `*` with no matches in `(+,)`." @parse_expression(
            x * y, operators = operators, variable_names = [:x, :y]
        )
    end
    operators = OperatorEnum(; binary_operators=[+])
    if VERSION >= v"1.9"
        @test_throws "Unrecognized operator: `*` with no matches in `(+,)` or `[show]`." @parse_expression(
            x * y, operators = operators, variable_names = [:x, :y], evaluate_on = [show]
        )
    end
    operators = OperatorEnum(; unary_operators=[cos])
    @eval blah(x...) = first(x)
    if VERSION >= v"1.9"
        @test_throws "Unrecognized operator: `blah` with no matches in `[show]`." @parse_expression(
            blah(x, x, y),
            operators = operators,
            variable_names = [:x, :y],
            evaluate_on = [show]
        )
    end
end

# Helpful error for missing function in scope
let
    my_badly_scoped_function(x) = x
    @test_throws ArgumentError begin
        ex = @parse_expression(
            my_badly_scoped_function(x),
            operators = operators,
            variable_names = ["x"],
            evaluate_on = [my_badly_scoped_function]
        )
    end
    if VERSION >= v"1.9"
        @test_throws "Make sure the function is defined in that module." begin
            ex = @parse_expression(
                my_badly_scoped_function(x),
                operators = operators,
                variable_names = ["x"],
                evaluate_on = [my_badly_scoped_function]
            )
        end
    end
end
