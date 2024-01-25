using SafeTestsets

# Check if SR_ENZYME_TEST is set in env
test_name = get(ENV, "SR_TEST", "main")

if test_name == "enzyme"
    @safetestset "Test enzyme derivatives" begin
        include("test_enzyme.jl")
    end
elseif test_name == "main"
    @safetestset "Unit tests" begin
        include("unittest.jl")
    end
else
    error("Unknown test name: $test_name")
end
