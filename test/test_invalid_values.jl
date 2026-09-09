using DynamicExpressions: DynamicExpressions as DE
using Interfaces: Interfaces

struct TextValue
    text::String
end
DE.is_valid(x::TextValue) = !isempty(x.text)
DE.invalid_value(::Type{TextValue}) = TextValue("")
inner(x::TextValue) = TextValue(x.text == "bad" ? "" : "ok")
function inner2(x::TextValue, y::TextValue)
    return TextValue(x.text == "bad" || y.text == "bad" ? "" : "ok")
end
outer(x::TextValue) = TextValue("outer")

@testset "Invalid intermediates without scalar constructors" begin
    ops = DE.OperatorEnum(;
        unary_operators=(outer, inner),
        binary_operators=(inner2,),
        define_helper_functions=false,
    )
    leaf = DE.Node{TextValue}(; feature=1)
    c = DE.Node{TextValue}(; val=TextValue("ok"))
    branches = [
        DE.Node{TextValue}(; op=2, l=leaf),
        DE.Node{TextValue}(; op=1, l=c, r=leaf),
        DE.Node{TextValue}(; op=1, l=leaf, r=c),
        DE.Node{TextValue}(; op=1, l=leaf, r=leaf),
    ]
    bad = reshape([TextValue("ok"), TextValue("bad")], 1, :)
    good = reshape([TextValue("ok"), TextValue("ok")], 1, :)
    for fused in (false, true), branch in branches
        ctx = DE.EvalContext(; use_fused=fused)
        tree = DE.Node{TextValue}(; op=1, l=branch)
        _, complete = DE.eval_tree_array(tree, bad, ops; eval_context=ctx)
        @test !complete
        result, complete = DE.eval_tree_array(tree, good, ops; eval_context=ctx)
        @test complete
        @test all(x -> x.text == "outer", result)
        parent = DE.Node{TextValue}(; op=1, l=tree)
        _, complete = DE.eval_tree_array(parent, bad, ops; eval_context=ctx)
        @test !complete
        @test all(x -> !DE.is_valid(x), tree(bad, ops; eval_context=ctx))
    end
    invalid_constant = DE.Node{TextValue}(; val=TextValue(""))
    for buffer in (nothing, DE.ArrayBuffer(Vector{TextValue}[], Ref(0)))
        ctx = DE.EvalContext(; buffer)
        @test all(x -> !DE.is_valid(x), invalid_constant(good, ops; eval_context=ctx))
    end
end

struct Force
    x::Float64
    y::Float64
    z::Float64
end
DE.is_valid(f::Force) = all(isfinite, (f.x, f.y, f.z))
DE.invalid_value(::Type{Force}) = Force(NaN, NaN, NaN)

@testset "Optional invalid values" begin
    for T in (Float16, Float32, Float64, ComplexF32, ComplexF64, Force, TextValue)
        value = @inferred DE.invalid_value(T)
        @test value isa T
        @test !DE.is_valid(value)
    end
    @test Interfaces.test(DE.ValueInterface{(:invalid_value,)}, Float64, [1.0])
    @test Interfaces.test(DE.ValueInterface{(:invalid_value,)}, ComplexF64, [1.0 + 2.0im])
    @test !applicable(DE.invalid_value, Int)
    @test !applicable(DE.invalid_value, Bool)
    int_ops = DE.OperatorEnum(; binary_operators=(+,), define_helper_functions=false)
    t = DE.Node{Int}(; op=1, l=DE.Node{Int}(; feature=1), r=DE.Node{Int}(; val=2))
    @test DE.eval_tree_array(t, reshape([1, 2], 1, :), int_ops) == ([3, 4], true)
    force_ops = DE.OperatorEnum(;
        unary_operators=(identity,), define_helper_functions=false
    )
    t_force = DE.Node{Force}(; val=DE.invalid_value(Force))
    output = t_force(reshape([Force(1, 2, 3)], 1, :), force_ops)
    @test all(x -> x isa Force && !DE.is_valid(x), output)
end

@testset "Numeric fused invalid propagation" begin
    kernel = DE.EvaluateModule._fused_binary3
    @test isnan(kernel(+, +, NaN, 1.0, 2.0, Val(:left), Val(true)))
    @test isnan(kernel(+, +, 1.0, 2.0, NaN, Val(:left), Val(true)))
    @test isnan(kernel(+, +, NaN, 1.0, 2.0, Val(:right), Val(true)))
    @test isnan(kernel(+, +, 1.0, 2.0, NaN, Val(:right), Val(true)))
end
