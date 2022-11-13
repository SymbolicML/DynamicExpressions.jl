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
@reexport import .EquationModule: Node, string_tree, print_tree, copy_node, set_node!
@reexport import .EquationUtilsModule:
    count_nodes,
    count_nodes_with_stack,
    count_constants,
    count_depth,
    NodeIndex,
    index_constants,
    has_operators,
    has_constants,
    get_constants,
    set_constants
@reexport import .OperatorEnumModule: AbstractOperatorEnum
@reexport import .OperatorEnumConstructionModule:
    OperatorEnum, GenericOperatorEnum, @extend_operators
@reexport import .EvaluateEquationModule: eval_tree_array, differentiable_eval_tree_array
@reexport import .EvaluateEquationDerivativeModule:
    eval_diff_tree_array, eval_grad_tree_array
@reexport import .InterfaceSymbolicUtilsModule: node_to_symbolic, symbolic_to_node
@reexport import .SimplifyEquationModule: combine_operators, simplify_tree

import TOML: parsefile

const PACKAGE_VERSION = let
    project = parsefile(joinpath(pkgdir(@__MODULE__), "Project.toml"))
    VersionNumber(project["version"])
end

macro ignore(args...) end
# To get LanguageServer to register library within tests
@ignore include("../test/runtests.jl")

end
