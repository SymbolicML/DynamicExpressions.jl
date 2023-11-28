using DynamicExpressions
using Test
include("test_params.jl")

@testset "Constructing trees with shared nodes" begin
    operators = OperatorEnum(;
        binary_operators=(+, -, *, ^, /, greater), unary_operators=(cos, exp, sin)
    )
    x1, x2, x3 = [GraphNode(Float64; feature=i) for i in 1:3]

    base_tree = cos(x1 - 3.2 * x2) - x1^3.2
    tree = sin(base_tree) + base_tree

    # The base tree is exactly the same:
    @test tree.l.l === tree.r
    @test hash(tree.l.l) == hash(tree.r)

    # Now, let's change something in the base tree:
    old_tree = deepcopy(tree)
    base_tree.l.l = x3 * x2 - 1.5

    # Should change:
    @test string_tree(tree, operators) != string_tree(old_tree, operators)

    # But the linkage should be preserved:
    @test tree.l.l === tree.r
    @test hash(tree.l.l) == hash(tree.r)

    # When we copy with the normal copy, the sharing breaks:
    copy_without_sharing = copy_node(tree; break_sharing=Val(true))
    @test !(copy_without_sharing.l.l === copy_without_sharing.r)

    # But with the sharing preserved in the copy, it should be the same:
    copy_with_sharing = copy_node(tree)
    @test copy_with_sharing.l.l === copy_with_sharing.r

    # We can also tweak the new tree, and the edits should be propagated:
    copied_base_tree = copy_with_sharing.l.l
    # (First, assert that it is the same as the old base tree)
    @test string_tree(copied_base_tree, operators) == string_tree(base_tree, operators)

    # Now, let's tweak the new tree's base tree:
    copied_base_tree.l.l = x1 * x2 * 5.2 - exp(x3)
    # "exp" should appear *twice* now:
    copy_with_sharing
    @test length(collect(eachmatch(r"exp", string_tree(copy_with_sharing, operators)))) == 2
    @test copy_with_sharing.l.l === copy_with_sharing.r
    @test hash(copy_with_sharing.l.l) == hash(copy_with_sharing.r)
    @test string_tree(copy_with_sharing.l.l, operators) != string_tree(base_tree, operators)

    # We also test whether `convert` breaks shared children.
    # The node type here should be Float64.
    @test typeof(tree).parameters[1] == Float64
    # Let's convert to Float32:
    float32_tree = convert(GraphNode{Float32}, tree)
    @test typeof(float32_tree).parameters[1] == Float32
    # The linkage should be kept:
    @test float32_tree.l.l === float32_tree.r
end

@testset "Macro tests" begin
    # We also do tests of the macros related to generating functions that preserve
    # sharing:
    @eval begin
        expr_eql(x::LineNumberNode, y::LineNumberNode) = true  # Ignore line numbers
        expr_eql(x::QuoteNode, y::QuoteNode) =
            x == y ? true : (println(x, " and ", y, " are not equal"); false)
        expr_eql(x::Number, y::Number) =
            x == y ? true : (println(x, " and ", y, " are not equal"); false)
        function expr_eql(x::Symbol, y::Symbol)
            if x == y
                return true
            else
                sx = string(x)
                sy = string(y)
                result = if startswith(sx, r"##")
                    occursin(sy, sx)
                elseif startswith(sy, r"##")
                    occursin(sx, sy)
                else
                    false
                end
                !result && println(x, " and ", y, " are not equal")
                return result
            end
        end
        function expr_eql(x::Expr, y::Expr)
            # Remove line numbers from the arguments:
            x.args = filter(c -> !isa(c, LineNumberNode), x.args)
            y.args = filter(c -> !isa(c, LineNumberNode), y.args)

            if expr_eql(x.head, y.head) &&
                length(x.args) == length(y.args) &&
                all(expr_eql.(x.args, y.args))
                return true
            else
                println(x, " and ", y, " are not equal")
                return false
            end
        end
        expr_eql(x, y) = error("Unexpected type: $(typeof(x)) or $(typeof(y))")
    end

    @testset "Macro testing utils" begin
        # First, assert this test actually works:
        @test !expr_eql(
            :(_convert(Node{T1}, tree)),
            :(_convert(Node{T1}, tree, IdDict{Node{T2},Node{T1}}())),
        )
    end

    @testset "@with_memoize" begin
        ex = @macroexpand DynamicExpressions.UtilsModule.@with_memoize(
            _convert(Node{T1}, tree), IdDict{Node{T2},Node{T1}}()
        )
        true_ex = quote
            _convert(Node{T1}, tree, IdDict{Node{T2},Node{T1}}())
        end

        @test expr_eql(ex, true_ex)
    end

    @testset "@memoize_on" begin
        ex = @macroexpand DynamicExpressions.UtilsModule.@memoize_on tree ((x, _) -> x) function _copy_node(
            tree::Node{T}
        )::Node{T} where {T}
            if tree.degree == 0
                if tree.constant
                    Node(; val=copy(tree.val::T))
                else
                    Node(T; feature=copy(tree.feature))
                end
            elseif tree.degree == 1
                Node(copy(tree.op), _copy_node(tree.l))
            else
                Node(copy(tree.op), _copy_node(tree.l), _copy_node(tree.r))
            end
        end
        true_ex = quote
            function _copy_node(tree::Node{T})::Node{T} where {T}
                if tree.degree == 0
                    if tree.constant
                        Node(; val=copy(tree.val::T))
                    else
                        Node(T; feature=copy(tree.feature))
                    end
                elseif tree.degree == 1
                    Node(copy(tree.op), _copy_node(tree.l))
                else
                    Node(copy(tree.op), _copy_node(tree.l), _copy_node(tree.r))
                end
            end
            function _copy_node(tree::Node{T}, id_map::AbstractDict;)::Node{T} where {T}
                key = objectid(tree)
                is_memoized = haskey(id_map, key)
                result = get!(id_map, key) do
                    begin
                        if tree.degree == 0
                            if tree.constant
                                Node(; val=copy(tree.val::T))
                            else
                                Node(T; feature=copy(tree.feature))
                            end
                        elseif tree.degree == 1
                            Node(copy(tree.op), _copy_node(tree.l, id_map))
                        else
                            Node(
                                copy(tree.op),
                                _copy_node(tree.l, id_map),
                                _copy_node(tree.r, id_map),
                            )
                        end
                    end
                end
                return (((x, _) -> begin
                    x
                end)(result, is_memoized))
            end
        end
        @test expr_eql(ex, true_ex)
    end
end

@testset "Operations on graphs" begin
    operators = OperatorEnum(;
        binary_operators=(+, -, *, ^, /), unary_operators=(cos, exp, sin)
    )
    function make_tree()
        x1, x2 = GraphNode(Float64; feature=1), GraphNode(Float64; feature=2)
        base_tree =
            cos(x1 - 3.2 * x2) - x1^3.5 +
            GraphNode(3, GraphNode(; val=0.3), GraphNode(; val=0.9))
        tree = sin(base_tree) + base_tree
        return base_tree, tree
    end

    @testset "Strings" begin
        x1 = GraphNode(Float64; feature=1)
        n = x1 + x1
        @test string_tree(copy_node(n; break_sharing=Val(true)), operators) == "x1 + x1"
        @test string_tree(n, operators) == "x1 + {x1}"

        # Copying the node explicitly changes the behavior:
        x1 = GraphNode(Float64; feature=1)
        n = x1 + copy(x1)
        @test string_tree(n, operators) == "x1 + x1"

        # But, note that if we do a type conversion, the connection is also lost:
        x1 = GraphNode(Float64; feature=1)
        n = x1 + 3.5 * x1
        @test_skip string_tree(n, operators) == "x1 + (3.5 * {x1})"
        # TODO: Try to fix this if we can

        base_tree, tree = make_tree()

        s = string_tree(copy_node(base_tree; break_sharing=Val(true)), operators)
        @test s == "(cos(x1 - (3.2 * x2)) - (x1 ^ 3.5)) + (0.3 * 0.9)"
        s = string_tree(base_tree, operators)
        @test s == "(cos(x1 - (3.2 * x2)) - ({x1} ^ 3.5)) + (0.3 * 0.9)"
        s = string_tree(tree, operators)
        @test s ==
            "sin((cos(x1 - (3.2 * x2)) - ({x1} ^ 3.5)) + (0.3 * 0.9)) + {((cos(x1 - (3.2 * x2)) - ({x1} ^ 3.5)) + (0.3 * 0.9))}"
        # ^ Note the {} indicating shared subexpression
    end

    @testset "Counting nodes" begin
        base_tree, tree = make_tree()

        @test count_nodes(base_tree; break_sharing=Val(true)) == 14
        @test count_nodes(tree; break_sharing=Val(true)) == 30

        # One shared node, so -1:
        @test count_nodes(base_tree) == 13

        # sin and the +, so +2 from above:
        @test count_nodes(tree) == 15
    end
end
