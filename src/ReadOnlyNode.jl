module ReadOnlyNodeModule

using ..NodeModule: AbstractExpressionNode, Node
import ..NodeModule: default_allocator, with_type_parameters, constructorof

abstract type AbstractReadOnlyNode{T,N<:AbstractExpressionNode{T}} <:
              AbstractExpressionNode{T} end

"""A type of expression node that prevents writing to the inner node"""
struct ReadOnlyNode{T,N} <: AbstractReadOnlyNode{T,N}
    _inner::N

    ReadOnlyNode(n::N) where {T,N<:AbstractExpressionNode{T}} = new{T,N}(n)
end
constructorof(::Type{<:ReadOnlyNode}) = ReadOnlyNode
@inline function Base.getproperty(n::AbstractReadOnlyNode, s::Symbol)
    out = getproperty(getfield(n, :_inner), s)
    if out isa AbstractExpressionNode
        return constructorof(typeof(n))(out)
    else
        return out
    end
end
function Base.setproperty!(::AbstractReadOnlyNode, ::Symbol, v)
    return error("Cannot set properties on a ReadOnlyNode")
end
Base.propertynames(n::AbstractReadOnlyNode) = propertynames(getfield(n, :_inner))
Base.copy(n::AbstractReadOnlyNode) = ReadOnlyNode(copy(getfield(n, :_inner)))

end
