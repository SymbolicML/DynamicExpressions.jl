@testitem "ZygoteGradient string representation" begin
    using DynamicExpressions
    using DynamicExpressions.ExtensionInterfaceModule: _zygote_gradient
    using Zygote

    # Test unary gradient
    f(x) = x^2
    @test repr(_zygote_gradient(f, Val(1))) == "∂f"

    # Test binary gradient (both partials)
    g(x, y) = x * y
    @test repr(_zygote_gradient(g, Val(2))) == "∂g"

    # Test binary gradient (first partial)
    @test repr(_zygote_gradient(g, Val(2), Val(1))) == "∂₁g"

    # Test binary gradient (second partial)
    @test repr(_zygote_gradient(g, Val(2), Val(2))) == "∂₂g"

    # Test with standard operators
    @test repr(_zygote_gradient(+, Val(2))) == "∂+"
    @test repr(_zygote_gradient(*, Val(2), Val(1))) == "∂₁*"
    @test repr(_zygote_gradient(*, Val(2), Val(2))) == "∂₂*"

    first_partial = _zygote_gradient(log, Val(2), Val(1))
    nested = _zygote_gradient(first_partial, Val(1))
    @test repr(nested) == "∂∂₁log"

    # Also should work with text/plain
    @test repr("text/plain", nested) == "∂∂₁log"
end

@testitem "ZygoteGradient evaluation" begin
    using DynamicExpressions
    using DynamicExpressions.ExtensionInterfaceModule: _zygote_gradient
    using Zygote

    x = 2.0
    y = 3.0

    # Test unary gradient
    f(x) = x^2
    @test (_zygote_gradient(f, Val(1)))(x) == 4.0

    # Test binary gradient (both partials)
    g(x, y) = x * y
    @test (_zygote_gradient(g, Val(2)))(x, y) == (3.0, 2.0)

    # Test binary gradient (first partial)
    @test (_zygote_gradient(g, Val(2), Val(1)))(x, y) == 3.0

    # Test second partial
    @test (_zygote_gradient(g, Val(2), Val(2)))(x, y) == 2.0
end
