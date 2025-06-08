@testitem "operator empty all globals" begin
    using DynamicExpressions

    operators = OperatorEnum(2 => [+, -])
    @test DynamicExpressions.OperatorEnumConstructionModule.LATEST_OPERATORS.x == operators
    DynamicExpressions.OperatorEnumConstructionModule.empty_all_globals!()
    @test isnothing(DynamicExpressions.OperatorEnumConstructionModule.LATEST_OPERATORS.x)
end
@testitem "`get_op` with no operators for degree" begin
    using DynamicExpressions
    using DynamicExpressions.EvaluateModule: get_op

    @test_throws ErrorException get_op(OperatorEnum(2 => [+]), Val(1), Val(1))
end

@testitem "OperatorEnumConstruction helper internals & edge cases" begin
    using DynamicExpressions.OperatorEnumConstructionModule:
        _unpack_broadcast_function,
        OperatorEnum,
        empty_all_globals!
    using Base.Broadcast: BroadcastFunction
    using Test

    # _unpack_broadcast_function
    sym, ex = _unpack_broadcast_function(BroadcastFunction(+))
    @test sym == :+
    @test occursin("BroadcastFunction", string(ex))

    sym2, ex2 = _unpack_broadcast_function(sin)
    @test sym2 == :sin && ex2 == :sin

    # constructing an OperatorEnum that mixes broadcasted and un‐broadcasted versions of
    # the same operator in one degree must fail
    @test_throws ArgumentError GenericOperatorEnum(2 => (+, BroadcastFunction(+)))

    # degree < 1 must throw
    @test_throws ArgumentError OperatorEnum(-1 => (+,))

    # tidy the package-level globals so later tests aren't affected
    empty_all_globals!()
end
