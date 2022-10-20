using DynamicExpressions
using Test

# Test that we generate errors:
scalar_add(x::T, y::T) where {T<:Real} = x + y
operators = GenericOperatorEnum(; binary_operators=[scalar_add], extend_user_operators=true)
tree = scalar_add(x1, x2)

# With error handling:
try
    eval_tree_array(tree, [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]], operators; catch_errors=true)
    @test false
catch e
    @test isa(e, ErrorException)
    expected_error_msg = "Failed to evaluate tree"
    @test occursin(expected_error_msg, e.msg)
end

# Without error handling:
output, flag = eval_tree_array(
    tree, [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]], operators; catch_errors=false
)
@test output === nothing
@test !flag

# Default is to catch errors:
try
    tree([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])
    @test false
catch e
    @test isa(e, ErrorException)
end

# But can be overrided:
output = tree([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]; catch_errors=false)
