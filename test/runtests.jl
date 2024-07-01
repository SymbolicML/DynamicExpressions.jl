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
        set_preferences!("DynamicExpressions", "instability_check" => "disable"; force=true)
        using JET
        using DynamicExpressions
        if VERSION >= v"1.10"
            struct MyReport end
            function JET.configured_reports(
                ::MyReport, reports::Vector{JET.InferenceErrorReport}
            )
                filter!(reports) do report
                    signature = report.sig
                    return !any(
                        x -> occursin("NonDifferentiableDeclarationsModule", string(x)),
                        signature,
                    )
                end
                return reports
            end
            JET.test_package(
                DynamicExpressions; target_defined_modules=true, report_config=MyReport()
            )
        end
    end
elseif test_name == "main"
    include("unittest.jl")
    @run_package_tests
else
    error("Unknown test name: $test_name")
end
