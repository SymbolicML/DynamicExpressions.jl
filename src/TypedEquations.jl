module TypedEquationsModule

using ..OperatorEnumModule: OperatorEnum
using ..EquationModule: Node
using ..UtilsModule: @maybe_turbo

abstract type TypedNode end
struct BinaryNode{Op,L,R} <: TypedNode
    op::Op
    l::L
    r::R
end
struct UnaryNode{Op,L} <: TypedNode
    op::Op
    l::L
end
struct ConstNode{T} <: TypedNode
    val::T
end
struct FeatureNode <: TypedNode
    feature::Int
end

function Base.convert(
    ::Type{TypedNode}, tree::Node{T}, operators::OperatorEnum
)::TypedNode where {T}
    if tree.degree == 0
        if tree.constant
            return ConstNode(tree.val::T)
        else
            return FeatureNode(tree.feature)
        end
    elseif tree.degree == 1
        return UnaryNode(operators.unaops[tree.op], convert(TypedNode, tree.l, operators))
    else
        return BinaryNode(
            operators.binops[tree.op],
            convert(TypedNode, tree.l, operators),
            convert(TypedNode, tree.r, operators),
        )
    end
end

"""
This function converts a tree to `TypedNode`, creating a fully-specialized kernel.
    
The only thing not specialized on are the constant values and feature indices.
"""
function _eval_tree_array_typed(
    tree::Node{T}, X::AbstractMatrix{T}, operators::OperatorEnum; turbo::Bool=false
)::AbstractVector{T} where {T}
    typed_tree = convert(TypedNode, tree, operators)
    return _eval_typed_array(typed_tree, X, turbo ? Val(true) : Val(false))
end

@generated function _eval_typed_array(
    tree::TN, X::AbstractMatrix{T}, ::Val{turbo}
)::AbstractVector{T} where {T,TN<:TypedNode,turbo}
    kernel = gen_evaluation_code(tree, :X, :i, :tree)
    return quote
        result = Vector{T}(undef, size(X, 2))
        @maybe_turbo $(turbo) for i in axes(X, 2)
            result[i] = $kernel
        end
        return result
    end
end
function gen_evaluation_code(
    ::Type{BinaryNode{Op,L,R}}, x::Symbol, i::Symbol, tree
) where {Op,L,R}
    l = gen_evaluation_code(L, x, i, :($(tree).l))
    r = gen_evaluation_code(R, x, i, :($(tree).r))
    return :($(Op.instance)($(l), $(r)))
end
function gen_evaluation_code(
    ::Type{UnaryNode{Op,L}}, x::Symbol, i::Symbol, tree
) where {Op,L}
    l = gen_evaluation_code(L, x, i, :($(tree).l))
    return :($(Op.instance)($(l)))
end
function gen_evaluation_code(::Type{ConstNode{T}}, x::Symbol, i::Symbol, tree) where {T}
    return :($(tree).val::$(T))
end
function gen_evaluation_code(::Type{FeatureNode}, x::Symbol, i::Symbol, tree)
    return :($(x)[$(tree).feature, $(i)])
end

end
