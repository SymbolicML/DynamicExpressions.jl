using SafeTestsets
using TestItemRunner

# Control which test groups run.
#
# Accepts a comma-separated list in SR_TEST (default: "main").
#
# - "main": full test suite (testitems)
# - "optim": Optim-specific testitems only
# - "jet": JET analysis
# - "enzyme": Enzyme tests

test_names = split(get(ENV, "SR_TEST", "main"), ",")

allowed = ["enzyme", "jet", "main", "optim"]
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
                        DynamicExpressions.OperatorEnumConstructionModule,
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
                            DynamicExpressions.OperatorEnumConstructionModule,
                        ),
                    )
                else
                    rethrow()
                end
            end
        end
    end
end

# TestItemRunner's `@run_package_tests` scans *all* `.jl` files under the package root,
# so we must filter to only the testitem files we actually want to run.

testitem_suffixes = String[]

if "main" in test_names
    push!(testitem_suffixes, joinpath("test", "unittest.jl"))
    push!(testitem_suffixes, joinpath("test", "test_optim.jl"))
end
if "optim" in test_names
    push!(testitem_suffixes, joinpath("test", "test_optim.jl"))
end

if !isempty(testitem_suffixes)
    @run_package_tests filter =
        ti -> any(suf -> endswith(ti.filename, suf), testitem_suffixes)
end
