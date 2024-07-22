@testitem "Value interface" begin
    using DynamicExpressions: ValueInterface
    using Interfaces: Interfaces

    @test Interfaces.test(ValueInterface, Number, [1, 2, 1.0, -1.0f0, 1im + 2.0, 1//2])
end
