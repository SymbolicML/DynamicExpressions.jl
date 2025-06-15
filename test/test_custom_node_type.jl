using DynamicExpressions
using DynamicExpressions: Nullable
using Test

mutable struct MyCustomNode{A,B} <: AbstractNode{2}
    degree::Int
    val1::A
    val2::B
    children::NTuple{2,Nullable{MyCustomNode{A,B}}}

    MyCustomNode(val1, val2) = new{typeof(val1),typeof(val2)}(0, val1, val2)
    function MyCustomNode(val1, val2, l)
        n = MyCustomNode(val1, val2)
        poison = n
        n.degree = 1
        set_children!(n, (l, poison))
        return n
    end
    function MyCustomNode(val1, val2, l, r)
        n = new{typeof(val1),typeof(val2)}(2, val1, val2)
        set_children!(n, (l, r))
        return n
    end
end

node1 = MyCustomNode(1.0, 2)

@test typeof(node1) == MyCustomNode{Float64,Int}
@test node1.degree == 0
@test count_depth(node1) == 1
@test count_nodes(node1) == 1

node2 = MyCustomNode(1.5, 3, node1)

@test typeof(node2) == MyCustomNode{Float64,Int}
@test node2.degree == 1
@test get_child(node2, 1).degree == 0
@test count_depth(node2) == 2
@test count_nodes(node2) == 2

node2 = MyCustomNode(1.5, 3, node1, node1)

@test count_depth(node2) == 2
@test count_nodes(node2) == 3
@test sum(t -> t.val1, node2) == 1.5 + 1.0 + 1.0
@test sum(t -> t.val2, node2) == 3 + 2 + 2
@test count(t -> t.degree == 0, node2) == 2

# If we have a bad definition, it should get caught with a helpful message
mutable struct MyCustomNode2{T} <: AbstractExpressionNode{T,2}
    degree::UInt8
    constant::Bool
    val::T
    feature::UInt16
    op::UInt8
    children::NTuple{2,Nullable{MyCustomNode2{T}}}
end

@test_throws ErrorException MyCustomNode2()
@test_throws ErrorException MyCustomNode2{Float64}()

@test_throws "Encountered the call for" MyCustomNode2()
@test_throws "Encountered the call for" MyCustomNode2{Float64}()
