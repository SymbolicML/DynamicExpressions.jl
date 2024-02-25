using DynamicExpressions
using CUDA
using Random

ext = Base.get_extension(DynamicExpressions, :DynamicExpressionsCUDAExt)
const FakeCuArray = ext.FakeCuArray

include("tree_gen_utils.jl")

safe_sin(x) = isfinite(x) ? sin(x) : convert(eltype(x), NaN)
safe_cos(x) = isfinite(x) ? cos(x) : convert(eltype(x), NaN)

let
    operators = OperatorEnum(;
        binary_operators=[+, -, *, /], unary_operators=[safe_sin, safe_cos]
    )
    x1, x2, x3 = (i -> Node(Float64; feature=i)).(1:3)

    for T in (Float32, Float64, ComplexF64), ntrees in (1, 2, 3), seed in 0:10
        Random.seed!(seed)

        nrow = rand(10:30)
        nnodes = rand(10:25, ntrees)

        buffer = rand(Bool) ? ones(Int32, 8, sum(nnodes)) : nothing
        gpu_buffer = rand(Bool) ? FakeCuArray(ones(Int32, 8, sum(nnodes))) : nothing
        gpu_workspace = rand(Bool) ? FakeCuArray(ones(T, nrow + 1, sum(nnodes))) : nothing

        trees = ntuple(i -> gen_random_tree_fixed_size(nnodes[i], operators, 3, T), ntrees)
        X = randn(T, 3, nrow)
        if ntrees > 1
            y, completed = eval_tree_array(trees, X, operators)
            gpu_y, gpu_completed = eval_tree_array(
                trees, FakeCuArray(X), operators; buffer, gpu_workspace, gpu_buffer
            )

            for i in eachindex(completed, gpu_completed)
                if completed[i]
                    @test y[i] ≈ gpu_y[i]
                end
            end
        else
            y, completed = eval_tree_array(only(trees), X, operators)
            gpu_y, gpu_completed = eval_tree_array(only(trees), FakeCuArray(X), operators)
            if completed
                @test y ≈ gpu_y
            end
        end
    end
end
