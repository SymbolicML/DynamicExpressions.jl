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

        ignored_mod = DynamicExpressions.NonDifferentiableDeclarationsModule

        if isdefined(JET, :match_report) && isdefined(JET, :ReportMatcher) &&
           isdefined(JET, :AnyFrameModule)
            # JET >= 0.11
            JET.test_package(
                DynamicExpressions;
                target_modules=(DynamicExpressions,),
                ignored_modules=(JET.AnyFrameModule(ignored_mod),),
            )
        else
            # JET <= 0.10: old matcher API
            struct MyIgnoredModule
                mod::Module
            end
            function JET.match_module(
                mod::MyIgnoredModule, @nospecialize(report::JET.InferenceErrorReport)
            )
                s_mod = string(mod.mod)
                any(report.vst) do vst
                    occursin(s_mod, string(JET.linfomod(vst.linfo)))
                end
            end
            JET.test_package(
                DynamicExpressions;
                target_defined_modules=true,
                ignored_modules=(MyIgnoredModule(ignored_mod),),
            )
            # TODO: Hack to get JET to ignore modules
            # https://github.com/aviatesk/JET.jl/issues/570#issuecomment-2199167755
        end
    end
end
if "main" in test_name
    include("unittest.jl")
    @run_package_tests
end
