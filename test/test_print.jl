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
