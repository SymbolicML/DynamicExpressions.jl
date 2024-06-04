using SafeTestsets
using TestItemRunner

# Check if SR_ENZYME_TEST is set in env
test_name = get(ENV, "SR_TEST", "main")

if test_name == "enzyme"
    @safetestset "Test enzyme derivatives" begin
        include("test_enzyme.jl")
    end
elseif test_name == "jet"
    @safetestset "JET" begin
        using Preferences
        set_preferences!("DynamicExpressions", "instability_check" => "disable")
        using JET
        using DynamicExpressions
        if VERSION >= v"1.10"
            JET.test_package(DynamicExpressions; target_defined_modules=true)
        end
    end
elseif test_name == "main"
    include("unittest.jl")
    @run_package_tests
else
    error("Unknown test name: $test_name")
end
