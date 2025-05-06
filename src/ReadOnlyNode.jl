module ReadOnlyNodeModule

using ..NodeModule: AbstractExpressionNode, Node
import ..NodeModule: default_allocator, with_type_parameters, constructorof, children

abstract type AbstractReadOnlyNode{T,D,N<:AbstractExpressionNode{T,D}} <:
              AbstractExpressionNode{T,D} end

"""A type of expression node that prevents writing to the inner node"""
struct ReadOnlyNode{T,D,N} <: AbstractReadOnlyNode{T,D,N}
    _inner::N

    ReadOnlyNode(n::N) where {T,D,N<:AbstractExpressionNode{T,D}} = new{T,D,N}(n)
end
constructorof(::Type{<:ReadOnlyNode}) = ReadOnlyNode
@inline function Base.getproperty(n::AbstractReadOnlyNode, s::Symbol)
    out = getproperty(getfield(n, :_inner), s)
    if out isa AbstractExpressionNode
        return ReadOnlyNode(out)
    else
        return out
    end
end
@inline function children(node::AbstractReadOnlyNode)
    return map(ReadOnlyNode, children(node))
end
function Base.setproperty!(::AbstractReadOnlyNode, ::Symbol, v)
    return error("Cannot set properties on a ReadOnlyNode")
end
Base.propertynames(n::AbstractReadOnlyNode) = propertynames(getfield(n, :_inner))
Base.copy(n::AbstractReadOnlyNode) = ReadOnlyNode(copy(getfield(n, :_inner)))

end
