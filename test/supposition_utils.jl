using DynamicExpressions
using Supposition

function create_tree_gen(;
    binary_operators=(+, -, *, /),
    unary_operators=(abs, exp, cos),
    operators=OperatorEnum(; binary_operators, unary_operators),
    features=1:5,
    type::Type{T}=Float64,
    val_gen=Data.Floats{type}(; nans=true, infs=true),
) where {T}
    unaop_gen = Data.SampledFrom(eachindex(operators.unaops))
    binop_gen = Data.SampledFrom(eachindex(operators.binops))

    val_node_gen = map(val -> Node{T}(; val), val_gen)

    feature_gen = Data.SampledFrom(features)
    feature_node_gen = map(feature -> Node{T}(; feature), feature_gen)

    leaf_node_gen = val_node_gen | feature_node_gen

    tree_gen = Data.recursive(leaf_node_gen; max_layers=20) do childgen
        deg0_node_gen = childgen
        deg1_node_gen = map(
            cs -> let
                (op, (l,)) = only(cs)
                Node{T}(; op, l)
            end,
            Data.Dicts(
                unaop_gen,
                Data.Vectors(childgen; min_size=1, max_size=1);
                min_size=1,
                max_size=1,
            ),
        )
        deg2_node_gen = map(
            cs -> let
                (op, (l, r)) = only(cs)
                Node{T}(; op, l, r)
            end,
            Data.Dicts(
                binop_gen,
                Data.Vectors(childgen; min_size=2, max_size=2);
                min_size=1,
                max_size=1,
            ),
        )
        ws = Data.WeightedSample(
            (deg0_node_gen, deg1_node_gen, deg2_node_gen), Float64[1, 3, 3]
        )
        map(produce!, ws)
    end

    return tree_gen
end
