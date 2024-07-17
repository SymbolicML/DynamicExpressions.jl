using SafeTestsets: @safetestset
using TestItems: @testitem

@testitem "Test Aqua.jl" begin
    include("test_aqua.jl")
end

@safetestset "Initial error handling test" begin
    include("test_initial_errors.jl")
end

# Trigger extensions:
using Zygote, SymbolicUtils, LoopVectorization, Bumper, Optim

@safetestset "Test deprecations" begin
    include("test_deprecations.jl")
end

include("test_optim.jl")

@testitem "Test tree construction and scoring" begin
    include("test_tree_construction.jl")
end

@testitem "Test SymbolicUtils interface" begin
    include("test_symbolic_utils.jl")
end

@testitem "Test derivatives" begin
    include("test_derivatives.jl")
end

@testitem "Test chain rules" begin
    include("test_chainrules.jl")
end

@testitem "Test undefined derivatives" begin
    include("test_undefined_derivatives.jl")
end

@testitem "Test simplification" begin
    include("test_simplification.jl")
end

@testitem "Test printing" begin
    include("test_print.jl")
end

@testitem "Test validity of expression evaluation" begin
    include("test_evaluation.jl")
end

@testitem "Test validity of integer expression evaluation" begin
    include("test_integer_evaluation.jl")
end

@testitem "Test NaN detection in evaluator" begin
    include("test_nan_detection.jl")
end

@testitem "Test OperatorEnum with non-number type" begin
    include("test_non_number_eval_tree_array.jl")
end

@testitem "Test hash of tree" begin
    include("test_hash.jl")
end

@testitem "Test sharing-preserving copy" begin
    include("test_graphs.jl")
end

@testitem "Test equation utils" begin
    include("test_utils.jl")
end

@testitem "Test generic operators" begin
    include("test_generic_operators.jl")
end

@testitem "Test tensor operators" begin
    include("test_tensor_operators.jl")
end

@testitem "Test error handling" begin
    include("test_error_handling.jl")
end

@testitem "Test equality operator" begin
    include("test_equality.jl")
end

@testitem "Test operators within module" begin
    include("test_custom_operators.jl")
end

@testitem "Test precompilation" begin
    include("test_precompilation.jl")
end

@testitem "Test Base" begin
    include("test_base.jl")
end

@testitem "Test extra node fields" begin
    include("test_extra_node_fields.jl")
end

@testitem "Test containers preserved" begin
    include("test_container_preserved.jl")
end

@testitem "Test helpers break upon redefining" begin
    include("test_safe_helpers.jl")
end

@testitem "Test custom node type" begin
    include("test_custom_node_type.jl")
end

@testitem "Test random sampling" begin
    include("test_random.jl")
end

@testitem "Test CUDA" begin
    if VERSION >= v"1.9"
        include("test_cuda.jl")
    end
end

include("test_expressions.jl")
include("test_multi_expression.jl")
include("test_parse.jl")
include("test_parametric_expression.jl")
include("test_operator_construction_edgecases.jl")
include("test_node_interface.jl")
