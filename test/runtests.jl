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

# Compatibility shim for JET 0.10 vs 0.11 matcher API
# - JET <= 0.10: JET.match_module
# - JET >= 0.11: JET.match_report + ReportMatcher
const _has_match_report = isdefined(JET, :match_report)

function _jet_run(mod::Module; target_modules=Tuple{Module}())
    if _has_match_report
        matcher = JET.ReportMatcher(; target_modules=target_modules)
        return JET.match_report(matcher, mod)
    else
        return JET.match_module(mod; target_modules=target_modules)
    end
end
        using DynamicExpressions

        ignored_mod = DynamicExpressions.NonDifferentiableDeclarationsModule

        if _has_match_report
            # JET >= 0.11
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
                ignored_modules=(MyIgnoredModule(ignored_mod),),
            )
        else
            # JET <= 0.10 (no ReportMatcher API)
            JET.test_package(DynamicExpressions; target_modules=(DynamicExpressions,))
        end
    end
end
if "main" in test_name
    include("unittest.jl")
    @run_package_tests
end
