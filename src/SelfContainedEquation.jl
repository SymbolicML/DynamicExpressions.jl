module SelfContainedEquationModule

import ..OperatorEnumModule: AbstractOperatorEnum
import ..EquationModule: Node, string_tree
import ..UtilsModule: mustfindfirst

struct SelfContainedNode{T,OP<:AbstractOperatorEnum}
    tree::Node{T}
    operators::OP

    function SelfContainedNode(
        tree::N, operators::_OP
    ) where {_T,N<:Node{_T},_OP<:AbstractOperatorEnum}
        return new{_T,_OP}(tree, operators)
    end
    function SelfContainedNode{_T,_OP}(
        tree::N, operators::_OP
    ) where {_T,N<:Node{_T},_OP<:AbstractOperatorEnum}
        return new{_T,_OP}(tree, operators)
    end
end

Base.one(x::SelfContainedNode) = SelfContainedNode(one(x.tree), x.operators)
Base.zero(x::SelfContainedNode) = SelfContainedNode(zero(x.tree), x.operators)

function Base.promote(a::S, b::Number) where {TS,S<:SelfContainedNode{TS}}
    n = Node(TS; val=b)
    return (a, S(n, a.operators))
end
function Base.promote(a::S, b::N) where {TS,S<:SelfContainedNode{TS},TN,N<:Node{TN}}
    T = promote_type(TS, TN)
    n_a = convert(Node{T}, a.tree)
    n_b = convert(Node{T}, b)
    return (S(n_a, a.operators), S(n_b, a.operators))
end
Base.promote(a::S, b::S) where {S<:SelfContainedNode} = (a, b)
Base.promote(a::T, b::S) where {S<:SelfContainedNode,T} = reverse(promote(b, a))

function Base.show(io::IO, m::MIME"text/plain", x::SelfContainedNode)
    print(io, "SelfContainedNode(\n")
    print(io, " "^4, "tree=", string_tree(x.tree, x.operators), ",\n")
    print(io, " "^4, "operators=", x.operators, "\n")
    return print(io, ")")
end

for binop in (:(Base.:/), :(Base.:*), :(Base.:+), :(Base.:-), :(Base.:^))
    @eval function $(binop)(a::S, b::S) where {S<:SelfContainedNode}
        op_index = mustfindfirst($binop, a.operators.binops)
        return S(Node(op_index, a.tree, b.tree), a.operators)
    end
    @eval function $(binop)(a::SelfContainedNode, b)
        return $(binop)(promote(a, b)...)
    end
    @eval function $(binop)(a, b::SelfContainedNode)
        return $(binop)(promote(a, b)...)
    end
end
for unaop in (
    :(Base.sin),
    :(Base.cos),
    :(Base.exp),
    :(Base.tan),
    :(Base.log),
    :(Base.sqrt),
    :(Base.:-),
)
    @eval function $(unaop)(a::S) where {S<:SelfContainedNode}
        op_index = mustfindfirst($unaop, a.operators.unaops)
        return S(Node(op_index, a.tree), a.operators)
    end
end

Base.:+(a::SelfContainedNode) = a

end