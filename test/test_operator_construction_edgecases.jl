@testitem "operator empty all globals" begin
    using DynamicExpressions

    operators = OperatorEnum(binary_operators=[+, -])
    @test DynamicExpressions.OperatorEnumConstructionModule.LATEST_OPERATORS.x == operators
    DynamicExpressions.OperatorEnumConstructionModule.empty_all_globals!()
    @test DynamicExpressions.OperatorEnumConstructionModule.LATEST_OPERATORS.x == nothing
end
