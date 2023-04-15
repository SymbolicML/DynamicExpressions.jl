using DynamicExpressions
using Test

# Before defining OperatorEnum, calling the implicit (deprecated)
# syntax should fail:
tree = Node(; feature=1)
try
    tree([1.0 2.0]')
    @test false
catch e
    @test isa(e, ErrorException)
    expected_error_msg = "The `tree(X; kws...)` syntax is deprecated"
    @test occursin(expected_error_msg, e.msg)
end

try
    tree'([1.0 2.0]')
    @test false
catch e
    @test isa(e, ErrorException)
    expected_error_msg = "The `tree'(X; kws...)` syntax is deprecated"
    @test occursin(expected_error_msg, e.msg)
end

# Test that we generate errors:
baseT = Float64
T = Union{baseT,Vector{baseT},Matrix{baseT}}

scalar_add(x::T, y::T) where {T<:Real} = x + y

operators = GenericOperatorEnum(; binary_operators=[scalar_add])

x1, x2, x3 = [Node(T; feature=i) for i in 1:3]

@extend_operators operators
tree = scalar_add(x1, x2)

# With error handling:
try
    eval_tree_array(tree, [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]], operators; throw_errors=true)
    @test false
catch e
    @test isa(e, ErrorException)
    expected_error_msg = "Failed to evaluate tree"
    @test occursin(expected_error_msg, e.msg)
end

# Without error handling:
output, flag = eval_tree_array(
    tree, [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]], operators; throw_errors=false
)
@test output === nothing
@test !flag

# Default is to catch errors:
try
    tree([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]], operators)
    @test false
catch e
    @test isa(e, ErrorException)
end

# But can be overrided:
output = tree([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]], operators; throw_errors=false)
