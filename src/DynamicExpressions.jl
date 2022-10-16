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
@reexport import .EquationModule: Node
@reexport import .OperatorEnumConstructionModule: OperatorEnum
@reexport import .EvaluateEquationModule: eval_tree_array
@reexport import .EvaluateEquationDerivativeModule: eval_diff_tree_array, eval_grad_tree_array
@reexport import .InterfaceSymbolicUtilsModule: node_to_symbolic


end