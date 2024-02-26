using DynamicExpressions
using DynamicExpressions.AsArrayModule: as_array
using CUDA
using Random

ext = Base.get_extension(DynamicExpressions, :DynamicExpressionsCUDAExt)
const FakeCuArray = ext.FakeCuArray

include("tree_gen_utils.jl")

safe_sin(x) = isfinite(x) ? sin(x) : convert(eltype(x), NaN)
safe_cos(x) = isfinite(x) ? cos(x) : convert(eltype(x), NaN)

@testset "Random evals" begin
    let
        operators = OperatorEnum(;
            binary_operators=[+, -, *, /], unary_operators=[safe_sin, safe_cos]
        )
        x1, x2, x3 = (i -> Node(Float64; feature=i)).(1:3)

        for T in (Float32, Float64, ComplexF64), ntrees in (1, 2, 3), seed in 0:10
            Random.seed!(seed)

            nrow = rand(10:30)
            nnodes = rand(10:25, ntrees)
            use_tuple = rand(Bool)

            buffer = rand(Bool) ? ones(Int32, 8, sum(nnodes)) : nothing
            gpu_buffer = rand(Bool) ? FakeCuArray(ones(Int32, 8, sum(nnodes))) : nothing
            gpu_workspace =
                rand(Bool) ? FakeCuArray(ones(T, nrow + 1, sum(nnodes))) : nothing

            trees = ntuple(
                i -> gen_random_tree_fixed_size(nnodes[i], operators, 3, T), ntrees
            )
            trees = use_tuple ? trees : collect(trees)
            X = randn(T, 3, nrow)
            if ntrees > 1
                y, completed = @inferred eval_tree_array(trees, X, operators)
                gpu_y, gpu_completed = @inferred eval_tree_array(
                    trees, FakeCuArray(X), operators; buffer, gpu_workspace, gpu_buffer
                )

                # Should give same result either way
                for i in eachindex(completed, gpu_completed)
                    if completed[i]
                        @test y[i] ≈ gpu_y[i]
                    end
                end

                # Should return same type as input
                if use_tuple
                    @test y isa Tuple
                    @test gpu_y isa Tuple
                else
                    @test y isa Vector
                    @test gpu_y isa Vector
                end
            else
                y, completed = @inferred eval_tree_array(only(trees), X, operators)
                gpu_y, gpu_completed = @inferred eval_tree_array(
                    only(trees), FakeCuArray(X), operators
                )
                if completed
                    @test y ≈ gpu_y
                end
            end
        end
    end
end

@testset "Evaluation on pre-computed buffers" begin
    let
        operators = OperatorEnum(;
            binary_operators=[+, -, *, /], unary_operators=[sin, cos]
        )
        x1, x2, x3 = (i -> Node(Float64; feature=i)).(1:3)
        Random.seed!(0)
        tree = sin(x1 * 3.1 - x3 * 0.9 + 0.2) * x2 - x3 * x3 * 1.5
        X = randn(Float64, 3, 100)

        y1, _ = eval_tree_array(tree, X, operators)
        y2, _ = eval_tree_array(tree, FakeCuArray(X), operators)

        @test y1 ≈ y2

        (; val, roots, buffer, num_nodes, num_launches) = as_array(Int32, tree)
        gpu_buffer = FakeCuArray(buffer)
        gpu_workspace = FakeCuArray(zeros(Float64, 101, 50))
        copyto!((@view gpu_workspace[end, :]), val)

        # Now, with all buffers:
        y3, _ = eval_tree_array(
            tree,
            FakeCuArray(X),
            operators;
            gpu_workspace,
            gpu_buffer,
            roots,
            num_nodes,
            num_launches,
            update_buffers=Val(false),
        )
        @test y1 ≈ y3

        # Should be able to shift some of the values in this buffer:
        i = findfirst(gpu_workspace[end, :] .== 0.9)
        gpu_workspace[end, i] = 0.8

        # And get the updated results:
        tree_prime = sin(x1 * 3.1 - x3 * 0.8 + 0.2) * x2 - x3 * x3 * 1.5
        y1_prime, _ = eval_tree_array(tree_prime, X, operators)
        y3_prime, _ = eval_tree_array(
            x1, # Doesn't matter what we put here
            FakeCuArray(X),
            operators;
            gpu_workspace,
            gpu_buffer,
            roots,
            num_nodes,
            num_launches,
            update_buffers=Val(false),
        )
        @test y1_prime ≈ y3_prime
    end
end
