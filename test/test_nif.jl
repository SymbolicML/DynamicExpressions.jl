# Copied from https://github.com/JuliaLang/julia/pull/55093
@testitem "nif" begin
    using DynamicExpressions.UtilsModule: nif

    x = (0, -1, 1, 0)
    @test nif(d -> x[d] > 0, d -> d, Val(4)) == 3

    @test nif(d -> d > 1, d -> "A", d -> "B", Val(1)) == "B"
    @test nif(d -> d > 3, d -> "A", d -> "B", Val(3)) == "B"

    # Test with N = 0
    @test nif(d -> d > 0, d -> "", d -> "A", Val(0)) == "A"

    # Specific branch true
    @test nif(d -> d == 2, d -> d, d -> "else", Val(3)) == 2

    # Test with condition only true for last branch
    @test nif(d -> d == 5, d -> "A", d -> "B", Val(5)) == "B"

    # Test with bad input:
    @test_throws ArgumentError("if statement length should be ≥ 0, got -1") nif(
        identity, identity, Val(-1)
    )

    # Non-Int64 also throws
    @test_throws TypeError nif(identity, identity, Val(1.5))

    # Make sure all conditions are actually evaluated
    result = let c = Ref(0)
        nif(d -> (c[] += 1; false), d -> 1, Val(4))
        c[]
    end
    @test result == 3

    # Test inference is good
    t = ("i am not an int", ntuple(d -> d, Val(10))...)
    function extract_from_tuple(t::Tuple, i)
        return nif(
            d -> d == i,
            d -> t[d + 1],  # We skip the non-integer element
            Val(length(t) - 1),
        )
    end
    # Normally, had we used getindex here, inference would have
    # not been able to infer that the return type never includes
    # the first element. But since we used an `nif`, the compiler
    # knows all possible branches and can infer the correct type.
    @test @inferred(extract_from_tuple(t, 3)) == 3
end
