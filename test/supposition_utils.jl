using DynamicExpressions
using Supposition

operators = OperatorEnum(; binary_operators=(+, -, *, /), unary_operators=(abs, cos))

T = Float64
features = 1:5
val_gen = Data.Floats{T}(; nans=true, infs=true)
feature_gen = Data.SampledFrom(features)
operator_gen = Data.SampledFrom(
    vcat(
        [(; op=0, degree=0)],
        map(op -> (; op, degree=1), eachindex(operators.unaops)),
        map(op -> (; op, degree=2), eachindex(operators.binops)),
    ),
)

val_node_gen = map(val -> Node{T}(; val), val_gen)
feature_node_gen = map(feature -> Node{T}(; feature), feature_gen)
leaf_node_gen = val_node_gen | feature_node_gen

branch_gen = @composed function _branch_gen(
    l=leaf_node_gen, r=leaf_node_gen, operator=operator_gen
)
    if operator.degree == 0
        return l
    elseif operator.degree == 1
        return Node{T}(; operator.op, l)
    else # degree == 2
        return Node{T}(; operator.op, l, r)
    end
end

Data.recursive(branch_gen; max_layers=3) do root
    leafs = filter(t -> t.degree == 0, root)
    leafs_gen = Data.SampledFrom(leafs)

    # ?
    # Want to call `branch_gen` here with the new leafs.
end
