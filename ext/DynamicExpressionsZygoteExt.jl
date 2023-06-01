module DynamicExpressionsZygoteExt

if isdefined(Base, :get_extension)
    import Zygote: gradient
    import DynamicExpressions: generate_diff_operators
else
    import ..Zygote: gradient
    import ..DynamicExpressions: generate_diff_operators
end

make_diff_bin(op) = (x, y) -> gradient(op, x, y)
make_diff_una(op) = x -> gradient(op, x)[1]

function generate_diff_operators(
    binary_operators::Vector{Function}, unary_operators::Vector{Function}
)
    diff_bin = Function[]
    diff_una = Function[]

    for op in binary_operators
        diff_op = make_diff_bin(op)
        push!(diff_bin, diff_op)
    end
    for op in unary_operators
        diff_op = make_diff_una(op)
        push!(diff_una, diff_op)
    end
    return diff_bin, diff_una
end

end
