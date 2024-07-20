using DynamicExpressions
using Test

import DynamicExpressions: with_type_parameters

mutable struct MyCustomNode{A,B} <: AbstractNode{2}
    degree::Int
    val1::A
    val2::B
    children::NTuple{2,Base.RefValue{MyCustomNode{A,B}}}

    MyCustomNode(val1, val2) = new{typeof(val1),typeof(val2)}(0, val1, val2)
    function MyCustomNode(val1, val2, l)
        return new{typeof(val1),typeof(val2)}(
            1, val1, val2, (Ref(l), Ref{MyCustomNode{typeof(val1),typeof(val2)}}())
        )
    end
    function MyCustomNode(val1, val2, l, r)
        return new{typeof(val1),typeof(val2)}(2, val1, val2, (Ref(l), Ref(r)))
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
@test node2.children[1][].degree == 0
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
    children::NTuple{2,Base.RefValue{MyCustomNode2{T}}}
end

@test_throws ErrorException MyCustomNode2()
@test_throws ErrorException MyCustomNode2{Float64}()

if VERSION >= v"1.9"
    @test_throws "Encountered the call for" MyCustomNode2()
    @test_throws "Encountered the call for" MyCustomNode2{Float64}()
end
