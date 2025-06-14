@testitem "Random Evals: Single Tree" begin
    using DynamicExpressions, CUDA, Random
    using DynamicExpressions.AsArrayModule: as_array

    include("tree_gen_utils.jl")

    safe_sin(x) = isfinite(x) ? sin(x) : convert(eltype(x), NaN)
    safe_cos(x) = isfinite(x) ? cos(x) : convert(eltype(x), NaN)

    ext = Base.get_extension(DynamicExpressions, :DynamicExpressionsCUDAExt)
    const FakeCuArray = ext.FakeCuArray

    operators = OperatorEnum(;
        binary_operators=[+, -, *, /], unary_operators=[safe_sin, safe_cos]
    )

    for T in (Float32, Float64, ComplexF64)
        for seed in 0:10
            Random.seed!(seed)
            nrow = rand(10:30)
            nnodes = rand(10:25)
            tree = gen_random_tree_fixed_size(nnodes, operators, 3, T)
            X = randn(T, 3, nrow)

            y, completed = eval_tree_array(tree, X, operators)
            y_gpu, completed_gpu = eval_tree_array(tree, FakeCuArray(X), operators)

            # TODO: Fix this
            # @test completed == completed_gpu
            if completed
                @test y ≈ y_gpu
            end
        end
    end
end

@testitem "Random Evals: Multiple Trees" begin
    using DynamicExpressions, CUDA, Random
    using DynamicExpressions.AsArrayModule: as_array

    include("tree_gen_utils.jl")

    safe_sin(x) = isfinite(x) ? sin(x) : convert(eltype(x), NaN)
    safe_cos(x) = isfinite(x) ? cos(x) : convert(eltype(x), NaN)

    operators = OperatorEnum(;
        binary_operators=[+, -, *, /], unary_operators=[safe_sin, safe_cos]
    )

    ext = Base.get_extension(DynamicExpressions, :DynamicExpressionsCUDAExt)
    const FakeCuArray = ext.FakeCuArray

    for T in (Float32, Float64, ComplexF64), ntrees in (2, 3), seed in 0:10
        Random.seed!(seed)

        nrow = rand(10:30)
        nnodes = rand(10:25, ntrees)
        use_tuple = rand(Bool)

        buffer = rand(Bool) ? ones(Int32, 8, sum(nnodes)) : nothing
        gpu_buffer = rand(Bool) ? FakeCuArray(ones(Int32, 8, sum(nnodes))) : nothing
        gpu_workspace = rand(Bool) ? FakeCuArray(ones(T, nrow + 1, sum(nnodes))) : nothing

        trees = ntuple(i -> gen_random_tree_fixed_size(nnodes[i], operators, 3, T), ntrees)
        trees = use_tuple ? trees : collect(trees)

        X = randn(T, 3, nrow)

        y, completed = eval_tree_array(trees, X, operators)
        gpu_y, gpu_completed = eval_tree_array(
            trees, FakeCuArray(X), operators; buffer, gpu_workspace, gpu_buffer
        )

        # TODO: Fix this
        # @test completed == gpu_completed

        for i in eachindex(completed, gpu_completed)
            if completed[i]
                @test y[i] ≈ gpu_y[i]
            end
        end

        # Check return type matches input type (tuple or vector)
        if use_tuple
            @test y isa Tuple
            @test gpu_y isa Tuple
        else
            @test y isa Vector
            @test gpu_y isa Vector
        end
    end
end

@testitem "Pre-Computed Buffers: Basic Equivalence" begin
    using DynamicExpressions, CUDA, Random
    using DynamicExpressions.AsArrayModule: as_array

    ext = Base.get_extension(DynamicExpressions, :DynamicExpressionsCUDAExt)
    const FakeCuArray = ext.FakeCuArray

    # No random trees here, we define a fixed tree
    x1, x2, x3 = (i -> Node(Float64; feature=i)).(1:3)
    operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[sin, cos])

    Random.seed!(0)
    tree = sin(x1 * 3.1 - x3 * 0.9 + 0.2) * x2 - x3 * x3 * 1.5
    X = randn(Float64, 3, 100)

    y1, _ = eval_tree_array(tree, X, operators)
    y2, _ = eval_tree_array(tree, FakeCuArray(X), operators)
    @test y1 ≈ y2
end

@testitem "Pre-Computed Buffers: Using Provided Buffers" begin
    using DynamicExpressions, CUDA, Random
    using DynamicExpressions.AsArrayModule: as_array

    ext = Base.get_extension(DynamicExpressions, :DynamicExpressionsCUDAExt)
    const FakeCuArray = ext.FakeCuArray

    x1, x2, x3 = (i -> Node(Float64; feature=i)).(1:3)
    operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[sin, cos])

    Random.seed!(0)
    tree = sin(x1 * 3.1 - x3 * 0.9 + 0.2) * x2 - x3 * x3 * 1.5
    X = randn(Float64, 3, 100)

    y1, _ = eval_tree_array(tree, X, operators)

    # Extract arrays
    (; val, roots, buffer, num_nodes, num_launches) = as_array(Int32, tree)
    gpu_buffer = FakeCuArray(buffer)
    gpu_workspace = FakeCuArray(zeros(Float64, size(X, 2) + 1, num_nodes))
    copyto!((@view gpu_workspace[end, :]), val)

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
end

@testitem "Pre-Computed Buffers: Modified Values" begin
    using DynamicExpressions, CUDA, Random
    using DynamicExpressions.AsArrayModule: as_array

    ext = Base.get_extension(DynamicExpressions, :DynamicExpressionsCUDAExt)
    const FakeCuArray = ext.FakeCuArray

    x1, x2, x3 = (i -> Node(Float64; feature=i)).(1:3)
    operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[sin, cos])

    Random.seed!(0)
    tree = sin(x1 * 3.1 - x3 * 0.9 + 0.2) * x2 - x3 * x3 * 1.5
    X = randn(Float64, 3, 100)

    y1, _ = eval_tree_array(tree, X, operators)

    (; val, roots, buffer, num_nodes, num_launches) = as_array(Int32, tree)
    gpu_buffer = FakeCuArray(buffer)
    gpu_workspace = FakeCuArray(zeros(Float64, size(X, 2) + 1, num_nodes))
    gpu_workspace[end, :] .= val

    # Change a constant (0.9 to 0.8)
    i = findfirst(gpu_workspace[end, :] .== 0.9)
    gpu_workspace[end, i] = 0.8

    tree_prime = sin(x1 * 3.1 - x3 * 0.8 + 0.2) * x2 - x3 * x3 * 1.5
    y1_prime, _ = eval_tree_array(tree_prime, X, operators)
    y3_prime, _ = eval_tree_array(
        x1, # dummy tree
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
