module DynamicExpressions

include("Utils.jl")
include("OperatorEnum.jl")
include("Equation.jl")
include("EquationUtils.jl")
include("EvaluateEquation.jl")
include("EvaluateEquationDerivative.jl")
include("InterfaceSymbolicUtils.jl")
include("SimplifyEquation.jl")
include("OperatorEnumConstruction.jl")

using Reexport
@reexport import .EquationModule: Node, string_tree, copy_node, set_node!
@reexport import .EquationUtilsModule:
    count_nodes,
    count_depth,
    NodeIndex,
    index_constants,
    has_constants,
    get_constants,
    set_constants
@reexport import .OperatorEnumConstructionModule: OperatorEnum
@reexport import .EvaluateEquationModule: eval_tree_array, differentiable_eval_tree_array
@reexport import .EvaluateEquationDerivativeModule:
    eval_diff_tree_array, eval_grad_tree_array
@reexport import .InterfaceSymbolicUtilsModule: node_to_symbolic, symbolic_to_node

end
