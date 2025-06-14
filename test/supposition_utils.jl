# supposition_utils.jl
#
# Helper that builds a Supposition generator returning fully-random
# DynamicExpressions.Expression objects whose node type is Node{T,D}.
# D is inferred from `operators`.

module SuppositionUtils

using Supposition: Data
using DynamicExpressions: Node, Expression, OperatorEnum
using DynamicExpressions.OperatorEnumConstructionModule: empty_all_globals!
empty_all_globals!()

function make_expression_generator(
    ::Type{T};
    num_features::Int=5,
    operators::OperatorEnum=OperatorEnum(1 => (abs, cos), 2 => (+, -, *, /)),
    max_layers::Int=3,
) where {T}
    D = length(operators.ops)

    val_gen = Data.Floats{T}(; nans=false, infs=false)
    val_node_gen = map(v -> Node{T,D}(; val=v), val_gen)

    feature_gen = Data.SampledFrom(1:num_features)
    feature_node_gen = map(i -> Node{T,D}(; feature=i), feature_gen)

    leaf_gen = val_node_gen | feature_node_gen

    wrapper_funcs = ntuple(
        degree -> let op_list = operators[degree]
            op_gen = Data.SampledFrom(1:length(op_list))

            child -> map(
                (op_idx, args...) -> Node{T,D}(; op=op_idx, children=args),
                op_gen,
                ntuple(_ -> child, degree)...,
            )
        end,
        Val(D),
    )
    expr_wrap(child) = foldl(|, (w(child) for w in wrapper_funcs))
    tree_gen = Data.Recursive(leaf_gen, expr_wrap; max_layers)
    return map(
        t -> Expression(t; operators, variable_names=["x$i" for i in 1:num_features]),
        tree_gen,
    )
end

# inside module SuppositionUtils
function make_input_matrix_generator(
    ::Type{T}=Float64; n_features::Int=5, min_batch::Int=1, max_batch::Int=16
) where {T}
    elem_gen = Data.Floats{T}(; nans=false, infs=false)
    batch_gen = Data.Integers(min_batch, max_batch)

    Data.bind(batch_gen) do bs
        vec_len = n_features * bs
        vec_gen = Data.Vectors(elem_gen; min_size=vec_len, max_size=vec_len)
        Data.map(v -> reshape(v, n_features, bs), vec_gen)
    end
end

end

using .SuppositionUtils: make_input_matrix_generator, make_expression_generator
