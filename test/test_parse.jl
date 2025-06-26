
@testitem "custom operator" begin
    using DynamicExpressions

    function my_custom_op(x, y)
        return x + y
    end

    operators = OperatorEnum(;
        binary_operators=[+, -, *, /, my_custom_op],
        unary_operators=[cos, sin],
        define_helper_functions=false,
    )

    # Clears operators (which should not affect the outcome!)
    OperatorEnum(; binary_operators=[/])

    ex = @parse_expression(
        $(my_custom_op)(x, sin(y) + 0.3),
        operators = operators,
        variable_names = ["x", "y"],
    )
    @test typeof(ex) <: Expression

    s = sprint((io, e) -> show(io, MIME("text/plain"), e), ex)
    @test s == "my_custom_op(x, sin(y) + 0.3)"

    @test string_tree(ex) == "my_custom_op(x, sin(y) + 0.3)"

    # Whereas the regular use would fail
    @test_throws ErrorException @eval let
        x = Node{Float64}(; feature=1)
        y = Node{Float64}(; feature=2)

        my_custom_op(x, sin(y) + 0.3)
    end
    @test_throws "Convenience constructor for operator `sin` for degree 1 is out-of-date" @eval let
        x = Node{Float64}(; feature=1)
        y = Node{Float64}(; feature=2)

        sin(y) + 0.3
    end
end

@testitem "Can also parse just a float" begin
    using DynamicExpressions
    operators = OperatorEnum()  # Tests empty operators
    ex = @parse_expression(1.0, operators = operators, variable_names = [])
    @test typeof(ex) <: Expression
    @test ex.tree.val == 1.0
    @test typeof(ex.tree) <: Node{Float64}
end

@testitem "Or an int" begin
    using DynamicExpressions
    operators = OperatorEnum()
    ex = @parse_expression(1, operators = operators, variable_names = [])
    @test typeof(ex.tree) <: Node{Int64}
end

@testitem "Or just a variable" begin
    using DynamicExpressions
    operators = OperatorEnum()
    ex = @parse_expression(x, operators = operators, variable_names = ["x"])
    @test typeof(ex.tree) <: Node{Float32}
end

@testitem "Or, with custom node types" begin
    using DynamicExpressions
    operators = OperatorEnum()
    ex = @parse_expression(
        x, operators = operators, variable_names = ["x"], node_type = GraphNode
    )
    @test typeof(ex.tree) <: GraphNode{Float32}
end

@testitem "With GraphNode" begin
    using DynamicExpressions

    node_type = GraphNode
    operators = OperatorEnum()
    ex = @parse_expression(x, operators = operators, variable_names = ["x"], node_type)
    @test typeof(ex.tree) <: GraphNode{Float32}
end

@testitem "Should work with symbols for variable names too" begin
    using DynamicExpressions
    ex = @parse_expression(
        cos(exp(α)),
        operators = OperatorEnum(; unary_operators=[cos, exp]),
        variable_names = [:α]
    )
    @test typeof(ex.tree) <: Node{Float32}
    s = sprint((io, e) -> show(io, MIME("text/plain"), e), ex)
    @test s == "cos(exp(α))"
end

@testitem "This also works for parsing mixed types" begin
    using DynamicExpressions
    using DispatchDoctor

    v = [1, 2, 3]
    ex = allow_unstable() do
        @parse_expression(
            $v * tan(cos(5 + x)),
            operators = GenericOperatorEnum(;
                binary_operators=[*, +], unary_operators=[tan, cos]
            ),
            variable_names = ["x"],
        )
    end

    @test typeof(ex.tree) <: Node{Any}
    @test typeof(ex.metadata.operators) <: GenericOperatorEnum
    s = sprint((io, e) -> show(io, MIME("text/plain"), e), ex)
    @test s == "[1, 2, 3] * tan(cos(5.0 + x))"

    # Can also force this to be a union type
    v = [1, 2, 3]
    ex = @parse_expression(
        $v * tan(cos(5 + x)),
        operators = GenericOperatorEnum(;
            binary_operators=[*, +], unary_operators=[tan, cos]
        ),
        variable_names = ["x"],
        node_type = Node{Union{Int,Vector{Int}}}
    )
    @test typeof(ex.tree) <: Node{Union{Int,Vector{Int}}}
    @test typeof(ex.metadata.operators) <: GenericOperatorEnum
    s = sprint((io, e) -> show(io, MIME("text/plain"), e), ex)
    @test s == "[1, 2, 3] * tan(cos(5 + x))"
    # ^ Note how it does not convert to Float32 anymore

    # However, we refuse to parse tuples and vectors
    let operators = GenericOperatorEnum(;
            binary_operators=[*, +], unary_operators=[tan, cos]
        ),
        variable_names = ["x"]

        @test_throws ArgumentError @parse_expression(
            [1, 2, 3] * tan(cos(5 + x)), operators, variable_names
        )
        @test_throws(
            "Unrecognized expression type: `Expr(:vect, ...)`. ",
            parse_expression(
                :([1, 2, 3] * tan(cos(5 + x)));
                operators,
                variable_names,
                calling_module=@__MODULE__
            )
        )
    end

    # But, we can actually evaluate it for simple input
    @test ex([1.0]) ≈ [1, 2, 3] * tan(cos(5 + 1.0))
end

@testitem "Also check with tuple inputs" begin
    tu = (1.0, 2.0im)
    ex = @parse_expression(
        x * $tu - cos(y),
        operators = GenericOperatorEnum(; binary_operators=[*, -], unary_operators=[cos]),
        variable_names = ["x", "y"],
        node_type = Node{Tuple{Float64,ComplexF64}}
    )
    s = sprint((io, e) -> show(io, MIME("text/plain"), e), ex)

    @test s == "(x * ((1.0, 0.0 + 2.0im))) - cos(y)"
    @test typeof(ex.tree) <: Node{Tuple{Float64,ComplexF64}}
end

@testitem "interpolating custom function" begin
    using DynamicExpressions
    using Suppressor
    show_type(x) = (show(typeof(x)); x)

    logged_out = @capture_out begin
        ex = @parse_expression(
            x * 2.5 - $(show_type)(cos(y)),
            operators = OperatorEnum(; binary_operators=[*, -], unary_operators=[cos]),
            variable_names = [:x, :y],
            evaluate_on = [show_type],
        )
        s = sprint((io, e) -> show(io, MIME("text/plain"), e), ex)
        @test s == "(x * 2.5) - cos(y)"
    end
    @test contains(logged_out, "Node{Float32")
end

@testitem "Helpful errors for missing operator" begin
    using DynamicExpressions
    operators = OperatorEnum(; unary_operators=[sin])
    @test_throws ArgumentError @parse_expression(
        cos(x), operators = operators, variable_names = [:x]
    )
    @test_throws "Unrecognized operator: `cos` with no matches in `(sin,)`." parse_expression(
        :(cos(x)); operators=operators, variable_names=[:x], calling_module=@__MODULE__
    )
    operators = OperatorEnum(; binary_operators=[+])
    @test_throws ArgumentError parse_expression(
        :(x * y); operators=operators, variable_names=[:x, :y], calling_module=@__MODULE__
    )
    @test_throws "Unrecognized operator: `*` with no matches in `(+,)`." parse_expression(
        :(x * y); operators=operators, variable_names=[:x, :y], calling_module=@__MODULE__
    )
    operators = OperatorEnum(; binary_operators=[+])
    @test_throws "Unrecognized operator: `*` with no matches in `(+,)` or `[show]`." parse_expression(
        :(x * y);
        operators=operators,
        variable_names=[:x, :y],
        evaluate_on=[show],
        calling_module=@__MODULE__
    )

    let evaluate_on = [show]
        @test_throws "Unrecognized operator: `*` with no matches in `(+,)` or `[show]`." parse_expression(
            :(x * y);
            operators=operators,
            variable_names=[:x, :y],
            evaluate_on=evaluate_on,
            calling_module=@__MODULE__
        )
    end
    operators = OperatorEnum(; unary_operators=[cos])
    @eval blah(x...) = first(x)
    @test_throws "Unrecognized operator: `blah` with no matches in `[show]`." parse_expression(
        :($blah(x, x, y));
        operators=operators,
        variable_names=[:x, :y],
        evaluate_on=[show],
        calling_module=@__MODULE__
    )
end

@testitem "Helpful error for missing function in scope" begin
    using DynamicExpressions
    operators = OperatorEnum(;
        binary_operators=[+, -, *, /],
        unary_operators=[cos, sin],
        define_helper_functions=false,
    )
    my_badly_scoped_function(x) = x
    @test_throws ArgumentError begin
        ex = @parse_expression(
            my_badly_scoped_function(x),
            operators = operators,
            variable_names = ["x"],
            evaluate_on = [my_badly_scoped_function]
        )
    end
    @test_throws "Tried to interpolate function `my_badly_scoped_function` but failed." begin
        ex = @parse_expression(
            my_badly_scoped_function(x),
            operators = operators,
            variable_names = ["x"],
            evaluate_on = [my_badly_scoped_function]
        )
    end
end

@testitem "Helpful error for missing variable name" begin
    using DynamicExpressions
    operators = OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[cos, sin])
    @test_throws ArgumentError @parse_expression(
        x + y, operators = operators, variable_names = ["x"],
    )
    # "Variable $(ex) not found in `variable_names`. " *
    @test_throws "Variable `y` not found in `variable_names`." parse_expression(
        :(x + y); operators=operators, variable_names=["x"], calling_module=@__MODULE__
    )
end

@testitem "Helpful error for bad keyword" begin
    using DynamicExpressions
    @test_throws LoadError @eval @parse_expression(x, variable_names = [:x], bad_arg = true)
end

@testitem "Call function explicitly to get coverage" begin
    import DynamicExpressions as DE
    operators = DE.OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])
    d = 1
    ex = DE.parse_expression(
        :(a + $d * b * b * b * b + 1.5 * c + identity(sin(a)));
        evaluate_on=[identity],
        operators,
        variable_names=[:a, :b, :c],
        calling_module=@__MODULE__,
    )
    @test DE.string_tree(ex) == "((a + ((((1.0 * b) * b) * b) * b)) + (1.5 * c)) + sin(a)"
end

@testitem "Test parsing of kws" begin
    using DynamicExpressions
    import DynamicExpressions as DE
    kws = [
        :(operators = OperatorEnum(; binary_operators=[+, *], unary_operators=[sin])),
        :(variable_names = [:x, :y]),
        :(node_type = GraphNode),
        :(evaluate_on = [show]),
    ]
    result = DE.ParseModule._parse_kws(kws)
    @test result.operators ==
        :(OperatorEnum(; binary_operators=[+, *], unary_operators=[sin]))
    @test result.variable_names == :([:x, :y])
    @test result.node_type == :(GraphNode)
    @test result.evaluate_on == :([show])
    kws = [:(operators), :(variable_names), :(node_type), :(evaluate_on)]
    result = DE.ParseModule._parse_kws(kws)
    @test result.operators == :(operators)
    @test result.variable_names == :(variable_names)
    @test result.node_type == :(node_type)
    @test result.evaluate_on == :(evaluate_on)

    @test_throws "Unrecognized argument: `bad_keyword`" DE.ParseModule._parse_kws([
        :bad_keyword
    ])
end

@testitem "Test parsing convenience functionality" begin
    using DynamicExpressions

    ex = @parse_expression(
        x * x - cos(0.3 * y - 0.9),
        binary_operators = [+, *, -],
        unary_operators = [cos],
        variable_names = [:x, :y]
    )

    s = sprint((io, e) -> show(io, MIME"text/plain"(), e), ex)
    @test s == "(x * x) - cos((0.3 * y) - 0.9)"
end

@testitem "Misc tests" begin
    using DynamicExpressions
    ex = parse_expression(
        :(x);
        operators=OperatorEnum(; binary_operators=[+, -, *, /]),
        variable_names=["x"],
        calling_module=@__MODULE__,
    )
    @test string_tree(ex) == "x"
end
