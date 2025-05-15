module ReadOnlyNodeModule

using DispatchDoctor: @unstable

using ..NodeModule: AbstractExpressionNode, Node, max_degree
import ..NodeModule: default_allocator, with_type_parameters, constructorof, get_children

abstract type AbstractReadOnlyNode{T,D,N<:AbstractExpressionNode{T,D}} <:
              AbstractExpressionNode{T,D} end

@inline inner(n::AbstractReadOnlyNode) = getfield(n, :_inner)
@inline function Base.getproperty(n::AbstractReadOnlyNode, s::Symbol)
    out = getproperty(inner(n), s)
    if out isa AbstractExpressionNode
        return constructorof(typeof(n))(out)
    else
        return out
    end
end
@inline function get_children(node::AbstractReadOnlyNode)
    return map(constructorof(typeof(node)), get_children(inner(node)))
end
function Base.setproperty!(::AbstractReadOnlyNode, ::Symbol, v)
    return error("Cannot set properties on a ReadOnlyNode")
end
Base.propertynames(n::AbstractReadOnlyNode) = propertynames(inner(n))
Base.copy(n::AbstractReadOnlyNode) = constructorof(typeof(n))(copy(inner(n)))

"""A type of expression node that prevents writing to the inner node"""
struct ReadOnlyNode{T,D,N} <: AbstractReadOnlyNode{T,D,N}
    _inner::N

    ReadOnlyNode(n::N) where {T,N<:AbstractExpressionNode{T}} = new{T,max_degree(N),N}(n)
end
@unstable constructorof(::Type{<:ReadOnlyNode}) = ReadOnlyNode

end
