using SafeTestsets

VERSION >= v"1.9" && @safetestset "Test Aqua.jl" begin
    include("test_aqua.jl")
end

@safetestset "Initial error handling test" begin
    include("test_initial_errors.jl")
end

@safetestset "Test deprecations" begin
    include("test_deprecations.jl")
end

@safetestset "Test tree construction and scoring" begin
    include("test_tree_construction.jl")
end

@safetestset "Test SymbolicUtils interface" begin
    include("test_symbolic_utils.jl")
end

@safetestset "Test derivatives" begin
    include("test_derivatives.jl")
end

@safetestset "Test undefined derivatives" begin
    include("test_undefined_derivatives.jl")
end

@safetestset "Test simplification" begin
    include("test_simplification.jl")
end

@safetestset "Test printing" begin
    include("test_print.jl")
end

@safetestset "Test validity of expression evaluation" begin
    include("test_evaluation.jl")
end

@safetestset "Test validity of integer expression evaluation" begin
    include("test_integer_evaluation.jl")
end

@safetestset "Test NaN detection in evaluator" begin
    include("test_nan_detection.jl")
end

@safetestset "Test hash of tree" begin
    include("test_hash.jl")
end

@safetestset "Test sharing-preserving copy" begin
    include("test_graphs.jl")
end

@safetestset "Test equation utils" begin
    include("test_utils.jl")
end

@safetestset "Test generic operators" begin
    include("test_generic_operators.jl")
end

@safetestset "Test tensor operators" begin
    include("test_tensor_operators.jl")
end

@safetestset "Test error handling" begin
    include("test_error_handling.jl")
end

@safetestset "Test equality operator" begin
    include("test_equality.jl")
end

@safetestset "Test operators within module" begin
    include("test_custom_operators.jl")
end

@safetestset "Test precompilation" begin
    include("test_precompilation.jl")
end

@safetestset "Test Base" begin
    include("test_base.jl")
end

@safetestset "Test containers preserved" begin
    include("test_container_preserved.jl")
end

@safetestset "Test helpers break upon redefining" begin
    include("test_safe_helpers.jl")
end

@safetestset "Test custom node type" begin
    include("test_custom_node_type.jl")
end

@safetestset "Test random sampling" begin
    include("test_random.jl")
end
