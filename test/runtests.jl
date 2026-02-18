using SafeTestsets
using TestItemRunner

# Check if SR_ENZYME_TEST is set in env
test_name = split(get(ENV, "SR_TEST", "main"), ",")

unknown_tests = filter(Base.Fix2(∉, ["enzyme", "jet", "main"]), test_name)

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
        set_preferences!("DynamicExpressions", "instability_check" => "disable"; force=true)
        using JET
        using DynamicExpressions

        if VERSION >= v"1.10"
            # JET's keyword API has changed across versions.
            # Prefer the older (but still supported) configuration first.
            try
                JET.test_package(
                    DynamicExpressions;
                    target_defined_modules=true,
                    ignored_modules=(
                        DynamicExpressions.NonDifferentiableDeclarationsModule,
                        DynamicExpressions.ValueInterfaceModule,
                    ),
                )
            catch err
                if err isa MethodError
                    # Newer JET prefers explicit target_modules.
                    JET.test_package(
                        DynamicExpressions;
                        target_modules=(DynamicExpressions,),
                        ignored_modules=(
                            DynamicExpressions.NonDifferentiableDeclarationsModule,
                            DynamicExpressions.ValueInterfaceModule,
                        ),
                    )
                else
                    rethrow()
                end
            end
        end
    end
end
if "main" in test_name
    include("unittest.jl")
    @run_package_tests
end
