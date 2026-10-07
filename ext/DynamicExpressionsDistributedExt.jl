module DynamicExpressionsDistributedExt

using DynamicExpressions: Node, count_nodes
using Distributed: Distributed
using Serialization: Serialization

struct PackedNode{T,D}
    degree::Vector{UInt8}
    constant::Vector{Bool}
    val::Vector{T}
    feature::Vector{UInt16}
    op::Vector{UInt8}
end

function pack(tree::Node{T,D}) where {T,D}
    n = count_nodes(tree)
    packed = PackedNode{T,D}(
        Vector{UInt8}(undef, n),
        Vector{Bool}(undef, n),
        Vector{T}(undef, n),
        Vector{UInt16}(undef, n),
        Vector{UInt8}(undef, n),
    )
    i = Ref(0)
    num_constants = Ref(0)
    foreach(tree) do node  # depth-first, parent before children
        k = (i[] += 1)
        is_leaf = node.degree == 0
        is_constant = is_leaf && node.constant
        packed.degree[k] = node.degree
        packed.constant[k] = is_constant
        packed.feature[k] = is_leaf && !is_constant ? node.feature : UInt16(0)
        packed.op[k] = is_leaf ? UInt8(0) : node.op
        is_constant && (packed.val[num_constants[] += 1] = node.val)
    end
    resize!(packed.val, num_constants[])
    return packed
end

function unpack(packed::PackedNode{T,D}) where {T,D}
    stack = Vector{Node{T,D}}(undef, length(packed.degree))
    top = 0
    val_index = length(packed.val)
    for i in length(packed.degree):-1:1
        degree = packed.degree[i]
        node = if degree == 0
            if packed.constant[i]
                val = packed.val[val_index]
                val_index -= 1
                Node{T,D}(; val)
            else
                Node{T,D}(; feature=packed.feature[i])
            end
        else
            children = ntuple(j -> stack[top - j + 1], Int(degree))
            top -= degree
            Node{T,D}(; op=packed.op[i], children)
        end
        stack[top += 1] = node
    end
    return stack[1]
end

function Serialization.serialize(
    s::Distributed.ClusterSerializer, tree::Node{T,D}
) where {T,D}
    return Serialization.serialize(s, pack(tree))
end

function Serialization.deserialize(
    s::Distributed.ClusterSerializer, ::Type{P}
) where {P<:PackedNode}
    # Reuse Serialization's standard struct reader, then rebuild the Node.
    return unpack(
        invoke(
            Serialization.deserialize,
            Tuple{Serialization.AbstractSerializer,DataType},
            s,
            P,
        ),
    )
end

end
