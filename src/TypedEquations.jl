module TypedEquationsModule

import LoopVectorization: @turbo, indices
using ..OperatorEnumModule: OperatorEnum
using ..EquationModule: Node
using ..UtilsModule: @maybe_turbo

abstract type TypedNode end
struct BinaryNode{Op,L<:TypedNode,R<:TypedNode} <: TypedNode
    op::Op
    l::L
    r::R
end
struct UnaryNode{Op,L<:TypedNode} <: TypedNode
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
    preamble, kernel = gen_evaluation_code(tree, :X, :i, :tree, Ref(0), Ref(0))
    output = quote
        result = Vector{T}(undef, size(X, 2))
        @maybe_turbo $(turbo) for i in indices((X, result), (2, 1))
            result[i] = $kernel
        end
        return result
    end
    pushfirst!(output.args, preamble...)
    return output
end
function gen_evaluation_code(
    ::Type{BinaryNode{Op,L,R}}, x::Symbol, i::Symbol, tree, numconst, numvars
) where {Op,L,R}
    preamble_l, l = gen_evaluation_code(L, x, i, :($(tree).l), numconst, numvars)
    preamble_r, r = gen_evaluation_code(R, x, i, :($(tree).r), numconst, numvars)
    return vcat(preamble_l, preamble_r), :($(Op.instance)($(l), $(r)))
end
function gen_evaluation_code(
    ::Type{UnaryNode{Op,L}}, x::Symbol, i::Symbol, tree, numconst, numvars
) where {Op,L}
    preamble, l = gen_evaluation_code(L, x, i, :($(tree).l), numconst, numvars)
    return preamble, :($(Op.instance)($(l)))
end
function gen_evaluation_code(
    ::Type{ConstNode{T}}, x::Symbol, i::Symbol, tree, numconst, numvars
) where {T}
    const_name = Symbol("__constant_", numconst.x += 1)
    preamble = [:($(const_name) = $(tree).val::$(T))]
    return preamble, const_name
end
function gen_evaluation_code(
    ::Type{FeatureNode}, x::Symbol, i::Symbol, tree, numconst, numvars
)
    # return Expr[], :($(x)[$(tree).feature, $(i)])
    varname = Symbol("__var_", numvars.x += 1)
    preamble = [:($(varname) = $(tree).feature)]
    return preamble, :($(x)[$(varname), $(i)])
end

end
