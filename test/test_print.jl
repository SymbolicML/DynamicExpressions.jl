using Test
using DynamicExpressions
import DynamicExpressions as DE

include("test_params.jl")

## Test Base.print
operators = OperatorEnum(;
    default_params..., binary_operators=(+, *, /, -), unary_operators=(cos, sin)
)

f = (x1, x2, x3) -> (sin(cos(sin(cos(x1) * x3) * 3.0) * -0.5) + 2.0) * 5.0

tree = f(Node("x1"), Node("x2"), Node("x3"))

s = repr(tree)
true_s = "(sin(cos(sin(cos(x1) * x3) * 3.0) * -0.5) + 2.0) * 5.0"

@test s == true_s

# TODO: Next, we test that custom varMaps work:

s = string_tree(tree, operators; variable_names=["v1", "v2", "v3"])
true_s = "(sin(cos(sin(cos(v1) * v3) * 3.0) * -0.5) + 2.0) * 5.0"
@test s == true_s

for unaop in [safe_log, safe_log2, safe_log10, safe_log1p, safe_sqrt, safe_acosh]
    opts = OperatorEnum(;
        default_params..., binary_operators=(+, *, /, -), unary_operators=(unaop,)
    )
    minitree = Node(1, Node("x1"))
    @test string_tree(minitree, opts) == replace(string(unaop), "safe_" => "") * "(x1)"
end

@isdefined(safe_pow) || @eval begin
    safe_pow(x::T, y::T) where {T<:Number} = (x < 0 && y != round(y)) ? T(NaN) : x^y
    DE.get_op_name(::typeof(safe_pow)) = "^"
end
for binop in [safe_pow, ^]
    opts = OperatorEnum(;
        default_params..., binary_operators=(+, *, /, -, binop), unary_operators=(cos,)
    )
    minitree = Node(5, Node("x1"), Node("x2"))
    @test string_tree(minitree, opts) == "x1 ^ x2"
end

@testset "Test print_tree function" begin
    if VERSION > v"1.8"
        operators = OperatorEnum(;
            binary_operators=(+, *, /, -), unary_operators=(cos, sin)
        )
        x1, x2, x3 = [Node(Float64; feature=i) for i in 1:3]
        tree = x1 * x1 + 0.5
        # Capture stdout to variable:
        pipe = Pipe()
        redirect_stdout(pipe) do
            print_tree(tree, operators)
        end
        close(pipe.in)
        s = read(pipe.out, String)
        @test s == "(x1 * x1) + 0.5\n"
    end
end

@testset "Test printing of complex numbers" begin
    @eval my_custom_op(x, y) = x + y
    operators = OperatorEnum(;
        default_params...,
        binary_operators=(+, *, /, -, my_custom_op),
        unary_operators=(cos, sin),
    )
    @extend_operators operators
    x1, x2, x3 = [Node(; feature=i) for i in 1:3]
    tree = sin(x1 * 1.0)
    @test string_tree(tree, operators) == "sin(x1 * 1.0)"
    x1 = convert(Node{ComplexF64}, x1)
    tree = sin(x1 * (1.0 + 2.0im))
    @test string_tree(tree, operators) == "sin(x1 * (1.0 + 2.0im))"
    tree = my_custom_op(x1, 1.0 + 2.0im)
    @test string_tree(tree, operators) == "my_custom_op(x1, 1.0 + 2.0im)"
end

@testset "Test user-define printing" begin
    operators = OperatorEnum(;
        default_params..., binary_operators=(+, *, /, -), unary_operators=(cos, sin)
    )
    @extend_operators operators
    x1, x2, x3 = [Node(Float64; feature=i) for i in 1:3]
    tree = x1 * x1 + 0.5
    @test string_tree(tree, operators; f_constant=Returns("TEST")) == "(x1 * x1) + TEST"
    @test string_tree(tree, operators; f_variable=Returns("TEST")) == "(TEST * TEST) + 0.5"
    @test string_tree(
        tree, operators; f_variable=Returns("TEST"), f_constant=Returns("TEST2")
    ) == "(TEST * TEST) + TEST2"

    # Try printing with a precision:
    tree = x1 * x1 + π
    f_constant(val::Float64, args...) = string(round(val; digits=2))
    @test string_tree(tree, operators; f_constant=f_constant) == "(x1 * x1) + 3.14"
    f_constant(val::Float64, args...) = string(round(val; digits=4))
    @test string_tree(tree, operators; f_constant=f_constant) == "(x1 * x1) + 3.1416"
end

@testset "Test variable names" begin
    operators = OperatorEnum(; binary_operators=[+, *, /, -], unary_operators=[cos, sin])
    @extend_operators operators
    x1, x2, x3 = [Node(Float64; feature=i) for i in 1:3]
    DynamicExpressions.OperatorEnumConstructionModule.LATEST_VARIABLE_NAMES.x = [
        "k1", "k2", "k3"
    ]
    tree = x1 * x2 + x3
    @test string(tree) == "(k1 * k2) + k3"
    empty!(DynamicExpressions.OperatorEnumConstructionModule.LATEST_VARIABLE_NAMES.x)
    @test string(tree) == "(x1 * x2) + x3"
    # Check if we can pass the wrong number of variable names:
    set_default_variable_names!(["k1"])
    @test string(tree) == "(k1 * x2) + x3"
    empty!(DynamicExpressions.OperatorEnumConstructionModule.LATEST_VARIABLE_NAMES.x)
end

@testset "Test pretty format for operators" begin
    # Define a custom operator with different pretty representation
    @eval begin
        my_pretty_op(x, y) = x + y
        DE.get_op_name(::typeof(my_pretty_op)) = "my_pretty_op"
        DE.get_pretty_op_name(::typeof(my_pretty_op)) = "pretty_op_two"
    end

    operators = OperatorEnum(;
        default_params...,
        binary_operators=(+, *, /, -, my_pretty_op),
        unary_operators=(cos, sin),
    )
    @extend_operators operators

    x1, x2 = [Node(; feature=i) for i in 1:2]

    # Test default format (not pretty)
    tree = my_pretty_op(x1, x2)
    @test string_tree(tree, operators) == "my_pretty_op(x1, x2)"

    # Test pretty format
    @test string_tree(tree, operators; pretty=true) == "pretty_op_two(x1, x2)"

    # Test with nested expressions
    tree = sin(my_pretty_op(x1, x2))
    @test string_tree(tree, operators) == "sin(my_pretty_op(x1, x2))"
    @test string_tree(tree, operators; pretty=true) == "sin(pretty_op_two(x1, x2))"

    # Test with constants
    tree = my_pretty_op(x1, Node(; val=3.14))
    @test string_tree(tree, operators) == "my_pretty_op(x1, 3.14)"
    @test string_tree(tree, operators; pretty=true) == "pretty_op_two(x1, 3.14)"

    # Test that the default implementation of get_pretty_op_name falls back to get_op_name
    tree = sin(x1)
    @test string_tree(tree, operators) == "sin(x1)"
    @test string_tree(tree, operators; pretty=true) == "sin(x1)"

    # Test with a unary operator that has a different pretty name
    @eval begin
        my_unary_op(x) = sin(x)
        DE.get_op_name(::typeof(my_unary_op)) = "my_unary_op"
        DE.get_pretty_op_name(::typeof(my_unary_op)) = "sine"
    end

    operators_with_unary = OperatorEnum(;
        default_params...,
        binary_operators=(+, *, /, -),
        unary_operators=(cos, sin, my_unary_op),
    )
    @extend_operators operators_with_unary

    tree = my_unary_op(x1)
    @test string_tree(tree, operators_with_unary) == "my_unary_op(x1)"
    @test string_tree(tree, operators_with_unary; pretty=true) == "sine(x1)"
end

using DynamicExpressions.StringsModule: get_op_name, needs_brackets
using Base.Broadcast: BroadcastFunction

@testset "Strings.get_op_name - broadcast operator variants" begin
    # single-character operator => leading dot
    @test get_op_name(BroadcastFunction(+)) == ".+"
    # multi-character operator => trailing dot
    @test get_op_name(BroadcastFunction(cos)) == "cos."
end

@testset "behavior of needs_brackets" begin
    @test !needs_brackets(3.14)
    @test !needs_brackets([1, 2, 3])
    @test needs_brackets(1 + 2im)
    @test needs_brackets("symbolic")
end
