using Test, DynamicExpressions, Zygote

safe_log(x) = x > 0 ? log(x) : convert(eltype(x), NaN)

operators = OperatorEnum(;
    binary_operators=[+, *, -, /], unary_operators=[safe_log, cos], enable_autodiff=true
)

@extend_operators operators

x1, x2, x3 = (i -> Node(Float64; feature=i)).(1:3)

tree = safe_log(x1)

x = zeros(3, 1) .- 1

@test all(isnan, tree(x, operators))

# Normally, Zygote's gradients would return `nothing`.
# However, we wrap the gradient to keep it type-stable.
@test all(isnan, tree'(x, operators; variable=true))
