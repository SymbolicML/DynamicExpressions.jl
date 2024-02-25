using DynamicExpressions
using CUDA
using Random

ext = Base.get_extension(DynamicExpressions, :DynamicExpressionsCUDAExt)
const FakeCuArray = ext.FakeCuArray

include("tree_gen_utils.jl")

let
    operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[cos, sin]);
    x1, x2, x3 = (i -> Node(Float64; feature=i)).(1:3)

    for T in (Float32, Float64, ComplexF64), num_trees in (1, 2, 3), seed in 0:10
        Random.seed!(seed)
        num_rows = rand(10:30)
        nodes_per = rand(10:25, num_trees)
        trees = ntuple(i -> gen_random_tree_fixed_size(nodes_per[i], operators, 3, T), num_trees)
        @show trees
        X = randn(T, 3, num_rows)
        y, completed = eval_tree_array(trees, X, operators)
        gpu_y, gpu_completed = eval_tree_array(trees, FakeCuArray(X), operators)
        gpu_y = Array.(gpu_y)

        for i in eachindex(completed, gpu_completed)
            @test ((completed[i] && gpu_completed[i]) && (y[i] ≈ gpu_y[i])) || (!completed[i] && !gpu_completed[i])
        end
    end
end
