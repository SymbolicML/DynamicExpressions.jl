@testitem "Value interface" begin
    using DynamicExpressions: ValueInterface
    using Interfaces: Interfaces

    @test Interfaces.test(ValueInterface, Number, [1, 2, 1.0, -1.0f0, 1im + 2.0, 1//2])
end

@testitem "ValueInterface generic helpers" begin
    using DynamicExpressions.ValueInterfaceModule:
        is_valid,
        count_scalar_constants,
        get_number_type,
        pack_scalar_constants!,
        unpack_scalar_constants
    using Test

    # generic `is_valid` (non-number) falls back to `true`
    @test is_valid("hello") === true
    # numeric specialisation
    @test is_valid(3.0) === true
    @test is_valid(Inf) === false
    @test is_valid(NaN) === false

    # simple scalar utilities
    @test count_scalar_constants(42) == 1
    @test get_number_type(Float32) == Float32

    # error paths: fallback methods should throw a helpful error
    @test_throws ErrorException get_number_type(String)
    @test_throws ArgumentError pack_scalar_constants!(Float64[], 1, "hello")
    @test_throws ArgumentError unpack_scalar_constants(Float64[1.0], 1, "hello")

    # internal interface self-checks: verify we handle bad types gracefully
    @test_logs (:error,) begin
        @test DynamicExpressions.ValueInterfaceModule._check_get_number_type("hello") == false
    end
    @test DynamicExpressions.ValueInterfaceModule._check_pack_scalar_constants!("hello") == false
    @test DynamicExpressions.ValueInterfaceModule._check_unpack_scalar_constants("hello") == false
end
