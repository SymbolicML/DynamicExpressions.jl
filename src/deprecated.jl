import Base: @deprecate
import .EquationModule: Node

@deprecate set_constants set_constants!
@deprecate simplify_tree simplify_tree!

function Node(d::Integer, c::Bool, v::T) where {T}
    Base.depwarn("Node(d, c, v) is deprecated. Use Node{T}(val=v) instead.", :Node)
    @assert d == 1
    @assert c == true
    return Node{T}(; val=v)
end
function Node(::Type{T}, d::Integer, c::Bool, v::_T) where {T,_T}
    Base.depwarn("Node(T, d, c, v) is deprecated. Use Node{T}(val=v) instead.", :Node)
    @assert d == 1
    @assert c == true
    return Node{T}(; val=v)
end
function Node(::Type{T}, d::Integer, c::Bool, ::Nothing, f::Integer) where {T}
    Base.depwarn(
        "Node(T, d, c, v, f) is deprecated. Use Node{T}(feature=f) instead.", :Node
    )
    @assert d == 1
    @assert c == false
    return Node{T}(; feature=f)
end
function Node(d::Integer, ::Bool, ::Nothing, ::Integer, o::Integer, l::Node)
    Base.depwarn(
        "Node(d, c, v, f, o, l) is deprecated. Use Node(operator=o, left=l) instead.", :Node
    )
    @assert d == 1
    return Node(; op=o, l=l)
end
function Node(
    d::Integer, ::Bool, ::Nothing, ::Integer, o::Integer, l::Node{T}, r::Node{T}
) where {T}
    Base.depwarn(
        "Node(d, c, v, f, o, l, r) is deprecated. Use Node(op=o, l=l, r=r) instead.", :Node
    )
    @assert d == 2
    return Node(; op=o, l=l, r=r)
end
