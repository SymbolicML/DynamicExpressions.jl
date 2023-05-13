using DynamicExpressions
using Test

operators = OperatorEnum(; binary_operators=(+, *, /, -), unary_operators=(cos, sin, exp))

function run_nan_detection_test(T)
    # Creating a NaN via computation.
    tree = exp(exp(exp(exp(Node("x1") + 1))))
    tree = convert(Node{T}, tree)
    X = ones(T, 1, 10) .* 100
    output, flag = eval_tree_array(tree, X, operators)
    @test !flag

    # Creating a NaN/Inf via division by constant zero.
    tree = cos(Node("x1") / 0.0f0)
    tree = convert(Node{T}, tree)
    output, flag = eval_tree_array(tree, X, operators)
    @test !flag

    # Having a NaN/Inf constants:
    tree = cos(Node("x1") + T(Inf))
    tree = convert(Node{T}, tree)
    output, flag = eval_tree_array(tree, X, operators)
    @test !flag
    tree = cos(Node("x1") + T(NaN))
    tree = convert(Node{T}, tree)
    output, flag = eval_tree_array(tree, X, operators)
    @test !flag
end

@testset "Simple NaN detections" begin
    for T in [Float16, Float32, Float64]
        @testset "NaN detection with $T" begin
            run_nan_detection_test(T)
        end
    end
end

using DynamicExpressions.UtilsModule: is_bad_array
using StaticArrays

function manual_nan_test(
    ::Type{T}, array_size, nan_location, ::Val{static_array}, unroll
) where {T,static_array}
    x = ones(T, array_size)
    x = static_array ? MVector{array_size}(x) : x
    @test !is_bad_array(x, unroll)
    x[nan_location] = T(NaN)
    @test is_bad_array(x, unroll)
end

@testset "Manual NaN tests" begin
    unroll_size = 16
    unroll = Val(unroll_size)
    for T in [Float16, Float32, Float64, ComplexF16, ComplexF32, ComplexF64],
        array_size in 1:(2 * unroll_size + 1),
        nan_location in 1:array_size,
        static_array in [false, true]

        manual_nan_test(
            T, array_size, nan_location, static_array ? Val(true) : Val(false), unroll
        )
    end
end
