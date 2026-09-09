@testitem "Invalid intermediates without scalar constructors" begin
    using DynamicExpressions: DynamicExpressions as DE

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

@testitem "Optional invalid values" begin
    using DynamicExpressions: DynamicExpressions as DE
    using Interfaces: Interfaces

    struct TextValue
        text::String
    end
    DE.is_valid(x::TextValue) = !isempty(x.text)
    DE.invalid_value(::Type{TextValue}) = TextValue("")

    struct Force
        x::Float64
        y::Float64
        z::Float64
    end
    DE.is_valid(f::Force) = all(isfinite, (f.x, f.y, f.z))
    DE.invalid_value(::Type{Force}) = Force(NaN, NaN, NaN)

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

@testitem "Numeric invalid intermediates without scalar constructors" begin
    using DynamicExpressions: DynamicExpressions as DE

    struct NumericValue <: Number
        data::Tuple{Float64}
    end
    Base.zero(::Type{NumericValue}) = NumericValue((0.0,))
    DE.is_valid(x::NumericValue) = isfinite(only(x.data))
    DE.is_valid_array(x::AbstractArray{NumericValue}) = all(DE.is_valid, x)
    inner(x::NumericValue, y::NumericValue) = NumericValue((x.data == y.data ? NaN : 1.0,))
    outer(x::NumericValue, y::NumericValue) = NumericValue((1.0,))

    ops = DE.OperatorEnum(; binary_operators=(outer, inner), define_helper_functions=false)
    leaf = DE.Node{NumericValue}(; feature=1)
    c = DE.Node{NumericValue}(; val=NumericValue((2.0,)))
    branch = DE.Node{NumericValue}(; op=2, l=leaf, r=c)
    for tree in (
        DE.Node{NumericValue}(; op=1, l=branch, r=c),
        DE.Node{NumericValue}(; op=1, l=c, r=branch),
    )
        for fused in (false, true)
            ctx = DE.EvalContext(; use_fused=fused)
            _, complete = DE.eval_tree_array(
                tree, fill(NumericValue((2.0,)), 1, 1), ops; eval_context=ctx
            )
            @test !complete
            output, complete = DE.eval_tree_array(
                tree, fill(NumericValue((3.0,)), 1, 1), ops; eval_context=ctx
            )
            @test complete
            @test only(output).data == (1.0,)
        end
    end
end
