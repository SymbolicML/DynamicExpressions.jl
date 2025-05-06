module ReadOnlyNodeModule

using DispatchDoctor: @unstable

using ..NodeModule: AbstractExpressionNode, Node
import ..NodeModule: default_allocator, with_type_parameters, constructorof, children

abstract type AbstractReadOnlyNode{T,D,N<:AbstractExpressionNode{T,D},IS_REF} <:
              AbstractExpressionNode{T,D} end

"""A type of expression node that prevents writing to the inner node"""
struct ReadOnlyNode{T,D,N,IS_REF} <: AbstractReadOnlyNode{T,D,N,IS_REF}
    _inner::N

    function ReadOnlyNode(
        n::N, ::Val{IS_REF}
    ) where {T,D,N<:AbstractExpressionNode{T,D},IS_REF}
        return new{T,D,N,IS_REF}(n)
    end
    function ReadOnlyNode(n::N) where {T,D,N<:AbstractExpressionNode{T,D}}
        return ReadOnlyNode(n, Val(false))
    end
    function ReadOnlyNode(n::AbstractReadOnlyNode)
        return n
    end
    function ReadOnlyNode(n::Ref{<:AbstractExpressionNode})
        return ReadOnlyNode(n[], Val(true))
    end
end
@inline inner(n::AbstractReadOnlyNode) = getfield(n, :_inner)
@unstable constructorof(::Type{<:ReadOnlyNode}) = ReadOnlyNode
Base.getindex(n::AbstractReadOnlyNode{T,D,N,true} where {T,D,N}) = n
@inline function Base.getproperty(n::AbstractReadOnlyNode, s::Symbol)
    out = getproperty(inner(n), s)
    if out isa Union{AbstractExpressionNode,Ref{<:AbstractExpressionNode}}
        return ReadOnlyNode(out)
    else
        return out
    end
end
@inline function children(node::AbstractReadOnlyNode, ::Val{n}) where {n}
    return map(ReadOnlyNode, children(inner(node), Val(n)))
end
function Base.setproperty!(::AbstractReadOnlyNode, ::Symbol, v)
    return error("Cannot set properties on a ReadOnlyNode")
end
Base.propertynames(n::AbstractReadOnlyNode) = propertynames(getfield(n, :_inner))
Base.copy(n::AbstractReadOnlyNode) = ReadOnlyNode(copy(getfield(n, :_inner)))

end
