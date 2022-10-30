using SafeTestsets

@safetestset "Test tree construction and scoring" begin
    include("test_tree_construction.jl")
end

@safetestset "Test SymbolicUtils interface" begin
    include("test_symbolic_utils.jl")
end

@safetestset "Test derivatives" begin
    include("test_derivatives.jl")
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

@safetestset "Test topology-preserving copy" begin
    include("test_preserve_multiple_parents.jl")
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
