using SafeTestsets
using TestItemRunner

# Control which test groups run.
#
# Accepts a comma-separated list in SR_TEST (default: "main").
#
# - "main": full test suite (testitems)
# - "optim": Optim-specific testitems only
# - "narity": n-ary testitems only
# - "jet": JET analysis
# - "enzyme": Enzyme tests

test_names = split(get(ENV, "SR_TEST", "main"), ",")

allowed = ["enzyme", "jet", "main", "narity", "optim"]
unknown_tests = filter(Base.Fix2(∉, allowed), test_names)

if !isempty(unknown_tests)
    error("Unknown test names: $unknown_tests")
end

if "enzyme" in test_names
    @safetestset "Test enzyme derivatives" begin
        include("test_enzyme.jl")
    end
end

if "jet" in test_names
    @safetestset "JET" begin
        using Preferences
        set_preferences!(
            "DynamicExpressions", "dispatch_doctor_mode" => "disable"; force=true
        )
        using JET
        using DynamicExpressions

        ignored_mod = DynamicExpressions.NonDifferentiableDeclarationsModule

        if isdefined(JET, :match_report) &&
            isdefined(JET, :ReportMatcher) &&
            isdefined(JET, :AnyFrameModule)
            # JET >= 0.11
            JET.test_package(
                DynamicExpressions;
                target_defined_modules=true,
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
            # On JET 0.10, `target_defined_modules` is not available and also
            # can cause spurious possible-error reports when analyzing beyond
            # the package's own modules. Restrict to the DynamicExpressions module.
            JET.test_package(
                DynamicExpressions;
                target_modules=(DynamicExpressions,),
                ignored_modules=(MyIgnoredModule(ignored_mod),),
            )
            # TODO: Hack to get JET to ignore modules
            # https://github.com/aviatesk/JET.jl/issues/570#issuecomment-2199167755
        end
    end
end

# Collect testitem definitions to load, then run them once.
testitem_files = String[]

if "main" in test_names
    push!(testitem_files, "unittest.jl")
else
    if "optim" in test_names
        push!(testitem_files, "test_optim.jl")
    end
    if "narity" in test_names
        push!(testitem_files, "test_n_arity_nodes.jl")
    end
end

for f in testitem_files
    include(f)
end

if !isempty(testitem_files)
    @run_package_tests
end
