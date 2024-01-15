module AsArrayModule

using .DynamicExpressions.EquationModule: AbstractExpressionNode, tree_mapreduce

function as_array(
    tree::N, index_type::Type{I}=Int64
) where {T,N<:AbstractExpressionNode{T},I}
    num_nodes = length(tree)
    degree = Array{fieldtype(N, :degree)}(undef, num_nodes)
    constant = Array{fieldtype(N, :constant)}(undef, num_nodes)
    val = Array{T}(undef, num_nodes)
    feature = Array{fieldtype(N, :feature)}(undef, num_nodes)
    op = Array{fieldtype(N, :op)}(undef, num_nodes)
    execution_order = Array{I}(undef, num_nodes)
    idx_self = Array{I}(undef, num_nodes)
    idx_l = Array{I}(undef, num_nodes)
    idx_r = Array{I}(undef, num_nodes)

    cursor = Ref(zero(I))
    tree_mapreduce(
        leaf -> begin
            self = (cursor[] += one(I))
            idx_self[self] = self
            degree[self] = 0
            execution_order[self] = one(I)
            constant[self] = leaf.constant
            if leaf.constant
                val[self] = leaf.val::T
            else
                feature[self] = leaf.feature
            end

            (id=self, order=one(I))
        end,
        branch -> begin
            self = (cursor[] += one(I))
            idx_self[self] = self
            op[self] = branch.op
            degree[self] = branch.degree

            (id=self, order=one(I))  # this order is unused
        end,
        ((parent, children::Vararg{Any,M}) where {M}) -> begin
            idx_l[parent.id] = children[1].id
            if M == 2
                idx_r[parent.id] = children[2].id
            end
            parent_execution_order = if M == 1
                children[1].order + one(I)
            else
                max(children[1].order, children[2].order) + one(I)
            end
            execution_order[parent.id] = parent_execution_order

            (id=parent.id, order=parent_execution_order)
        end,
        tree;
        break_sharing=Val(true),
    )

    return (; degree, constant, val, feature, op, execution_order, idx_self, idx_l, idx_r)
end

end
