module DynamicExpressionsTermInterfaceExt

if isdefined(Base, :get_extension)
    import TermInterface:
        istree, exprhead, operation, arguments, similarterm, unsorted_arguments, symtype
    import DynamicExpressions.EquationModule: Node
    import DynamicExpressions.SelfContainedEquationModule: SelfContainedNode

else
    import ..TermInterface:
        istree, exprhead, operation, arguments, similarterm, unsorted_arguments, symtype
    import ..DynamicExpressions.EquationModule: Node
    import ..DynamicExpressions.SelfContainedEquationModule: SelfContainedNode
end

istree(x::SelfContainedNode) = x.tree.degree > 0
exprhead(::SelfContainedNode) = :call
symtype(::S) where {T,S<:SelfContainedNode{T}} = T
function operation(x::SelfContainedNode)
    if x.tree.degree == 1
        return x.operators.unaops[x.tree.op]
    else # x.tree.degree == 2
        return x.operators.binops[x.tree.op]
    end
end
function arguments(x::S) where {S<:SelfContainedNode}
    if x.tree.degree == 1
        return [S(x.tree.l, x.operators)]
    else # x.tree.degree == 2
        return [S(x.tree.l, x.operators), S(x.tree.r, x.operators)]
    end
end
function similarterm(t::S, f, args, symtype=T) where {T,OP,S<:SelfContainedNode{T,OP}}
    #
    if length(args) == 0
        error("Unexpected input.")
    elseif length(args) == 1
        op_index = findfirst(==(f), t.operators.unaops)::Integer
        new_node = convert(Node{T}, Node(op_index, only(args)))
        return S(new_node, t.operators)
    elseif length(args) == 2
        op_index = findfirst(==(f), t.operators.binops)::Integer
        new_node = convert(Node{T}, Node(op_index, args[1], args[2]))
        return S(new_node, t.operators)
    else
        l = similarterm(t, f, args[begin:(begin + 1)], symtype)
        return similarterm(t, f, [l, args[(begin + 2):end]...], symtype)
    end
end

end