module DynamicExpressionsBumperExt

using Bumper: @no_escape, @alloc
using DynamicExpressions:
    OperatorEnum, AbstractExpressionNode, tree_mapreduce, is_valid_array, EvalOptions
using DynamicExpressions.UtilsModule: ResultOk, counttuple

import DynamicExpressions.ExtensionInterfaceModule: bumper_eval_tree_array, bumper_kern!

function bumper_eval_tree_array(
    tree::AbstractExpressionNode{T},
    cX::AbstractMatrix{T},
    operators::OperatorEnum,
    eval_options::EvalOptions{turbo,true,early_exit},
) where {T,turbo,early_exit}
    result = similar(cX, axes(cX, 2))
    n = size(cX, 2)
    all_ok = Ref(false)
    @no_escape begin
        _result_ok = tree_mapreduce(
            # Leaf nodes, we create an allocation and fill
            # it with the value of the leaf:
            leaf_node -> begin
                ar = @alloc(T, n)
                ok = if leaf_node.constant
                    v = leaf_node.val
                    ar .= v
                    early_exit ? isfinite(v) : true
                else
                    ar .= view(cX, leaf_node.feature, :)
                    true
                end
                ResultOk(ar, ok)
            end,
            # Branch nodes, we simply pass them to the evaluation kernel:
            branch_node -> branch_node,
            # In the evaluation kernel, we combine the branch nodes
            # with the arrays created by the leaf nodes:
            KernelDispatcher(operators, eval_options),
            tree;
            break_sharing=Val(true),
        )
        x = _result_ok.x
        result .= x
        all_ok[] = _result_ok.ok
    end
    return (result, all_ok[])
end

struct KernelDispatcher{O<:OperatorEnum,E<:EvalOptions{<:Any,true,<:Any}} <: Function
    operators::O
    eval_options::E
end

@generated function (kd::KernelDispatcher{<:Any,<:EvalOptions{<:Any,true,early_exit}})(
    branch_node, inputs::Vararg{Any,degree}
) where {degree,early_exit}
    quote
        Base.Cartesian.@nexprs($degree, i -> inputs[i].ok || return inputs[i])
        cumulators = Base.Cartesian.@ntuple($degree, i -> inputs[i].x)
        out = dispatch_kerns!(kd.operators, branch_node, cumulators, kd.eval_options)
        return ResultOk(out, early_exit ? is_valid_array(out) : true)
    end
end
@generated function dispatch_kerns!(
    operators::OperatorEnum{OPS},
    branch_node,
    cumulators::Tuple{Vararg{Any,degree}},
    eval_options::EvalOptions,
) where {OPS,degree}
    nops = length(OPS.types[degree].types)
    quote
        op_idx = branch_node.op
        Base.Cartesian.@nif(
            $nops,
            i -> i == op_idx,
            i -> bumper_kern!(operators[$degree][i], cumulators, eval_options)
        )
    end
end

function bumper_kern!(
    op::F, cumulators::Tuple{Vararg{Any,degree}}, ::EvalOptions{false,true,early_exit}
) where {F,degree,early_exit}
    cumulator_1 = first(cumulators)
    @. cumulator_1 = op(cumulators...)
    return cumulator_1
end

end
