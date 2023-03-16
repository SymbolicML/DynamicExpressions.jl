using DynamicExpressions
using Test
include("test_params.jl")

@testset "Trees with shared nodes" begin
    operators = OperatorEnum(;
        binary_operators=(+, -, *, ^, /, greater), unary_operators=(cos, exp, sin)
    )
    x1, x2, x3 = Node("x1"), Node("x2"), Node("x3")

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

    # When we copy with the normal copy, the topology breaks:
    copy_without_topology = copy_node(tree)
    @test !(copy_without_topology.l.l === copy_without_topology.r)

    # But with the topology preserved in the copy, it should be the same:
    copy_with_topology = copy_node(tree; preserve_topology=true)
    @test copy_with_topology.l.l === copy_with_topology.r

    # We can also tweak the new tree, and the edits should be propagated:
    copied_base_tree = copy_with_topology.l.l
    # (First, assert that it is the same as the old base tree)
    @test string_tree(copied_base_tree, operators) == string_tree(base_tree, operators)

    # Now, let's tweak the new tree's base tree:
    copied_base_tree.l.l = x1 * x2 * 5.2 - exp(x3)
    # "exp" should appear *twice* now:
    copy_with_topology
    @test length(collect(eachmatch(r"exp", string_tree(copy_with_topology, operators)))) ==
        2
    @test copy_with_topology.l.l === copy_with_topology.r
    @test hash(copy_with_topology.l.l) == hash(copy_with_topology.r)
    @test string_tree(copy_with_topology.l.l, operators) !=
        string_tree(base_tree, operators)

    # We also test whether `convert` breaks shared children.
    # The node type here should be Float64.
    @test typeof(tree).parameters[1] == Float64
    # Let's convert to Float32:
    float32_tree = convert(Node{Float32}, tree; preserve_topology=true)
    @test typeof(float32_tree).parameters[1] == Float32
    # The linkage should be kept:
    @test float32_tree.l.l === float32_tree.r
end

# We also do tests of the macros related to generating functions that preserve
# topology:
expr_eql(x::LineNumberNode, y::LineNumberNode) = true  # Ignore line numbers
expr_eql(x::QuoteNode, y::QuoteNode) = x == y
expr_eql(x::Number, y::Number) = x == y
expr_eql(x::Symbol, y::Symbol) = x == y
function expr_eql(x::Expr, y::Expr)
    # Remove line numbers from the arguments:
    x.args = filter(c -> !isa(c, LineNumberNode), x.args)
    y.args = filter(c -> !isa(c, LineNumberNode), y.args)

    return expr_eql(x.head, y.head) &&
           length(x.args) == length(y.args) &&
           all(expr_eql.(x.args, y.args))
end
expr_eql(x, y) = error("Unexpected type: $(typeof(x)) or $(typeof(y))")

@testset "Macro testing utils" begin
    # First, assert this test actually works:
    @test !expr_eql(
        :(_convert(Node{T1}, tree)),
        :(_convert(Node{T1}, tree, IdDict{Node{T2},Node{T1}}())),
    )
end

@testset "@use_idmap" begin
    ex = @macroexpand DynamicExpressions.UtilsModule.@use_idmap(
        _convert(Node{T1}, tree), IdDict{Node{T2},Node{T1}}()
    )
    true_ex = quote
        _convert(Node{T1}, tree, IdDict{Node{T2},Node{T1}}())
    end

    @test expr_eql(ex, true_ex)
end

@testset "@generate_idmap" begin
    ex = @macroexpand DynamicExpressions.UtilsModule.@generate_idmap tree function _copy_node(
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
        function _copy_node(tree::Node{T}, id_map::IdDict;)::Node{T} where {T}
            get!(id_map, tree) do
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
        end
    end
    @test expr_eql(ex, true_ex)
end
