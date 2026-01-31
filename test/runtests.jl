using SafeTestsets
using TestItemRunner

# Check if SR_ENZYME_TEST is set in env
test_name = split(get(ENV, "SR_TEST", "main"), ",")

unknown_tests = filter(Base.Fix2(∉, ["enzyme", "jet", "main", "narity"]), test_name)

if !isempty(unknown_tests)
    error("Unknown test names: $unknown_tests")
end

if "enzyme" in test_name
    @safetestset "Test enzyme derivatives" begin
        include("test_enzyme.jl")
    end
end
if "jet" in test_name
    @safetestset "JET" begin
        using Preferences
        set_preferences!(
            "DynamicExpressions", "dispatch_doctor_mode" => "disable"; force=true
        )
        using JET
        using DynamicExpressions

        # JET v0.11 uses the ReportMatcher + match_report API.
        struct MyIgnoredModule <: JET.ReportMatcher
            mod::Module
        end
        function JET.match_report(
            m::MyIgnoredModule, @nospecialize(report::JET.InferenceErrorReport)
        )
            s_mod = string(m.mod)
            any(report.vst) do vst
                occursin(s_mod, string(JET.linfomod(vst.linfo)))
            end
        end

        JET.test_package(
            DynamicExpressions;
            target_modules=(DynamicExpressions,),
            ignored_modules=(
                MyIgnoredModule(DynamicExpressions.NonDifferentiableDeclarationsModule),
            ),
        )
    end
end
if "main" in test_name
    include("unittest.jl")
    @run_package_tests
end
