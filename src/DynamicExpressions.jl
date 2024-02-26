module DynamicExpressions

include("Utils.jl")
include("ExtensionInterface.jl")
include("OperatorEnum.jl")
include("Equation.jl")
include("EquationUtils.jl")
include("Strings.jl")
include("EvaluateEquation.jl")
include("EvaluateEquationDerivative.jl")
include("EvaluationHelpers.jl")
include("SimplifyEquation.jl")
include("OperatorEnumConstruction.jl")
include("Random.jl")
include("AsArray.jl")

import PackageExtensionCompat: @require_extensions
import Reexport: @reexport
@reexport import .EquationModule:
    AbstractNode,
    AbstractExpressionNode,
    GraphNode,
    Node,
    copy_node,
    set_node!,
    tree_mapreduce,
    filter_map,
    filter_map!
import .EquationModule: constructorof, preserve_sharing
@reexport import .EquationUtilsModule:
    count_nodes,
    count_constants,
    count_depth,
    NodeIndex,
    index_constants,
    has_operators,
    has_constants,
    get_constants,
    set_constants!,
    get_constant_refs,
    set_constant_refs!
@reexport import .StringsModule: string_tree, print_tree
@reexport import .OperatorEnumModule: AbstractOperatorEnum
@reexport import .OperatorEnumConstructionModule:
    OperatorEnum, GenericOperatorEnum, @extend_operators, set_default_variable_names!
@reexport import .EvaluateEquationModule: eval_tree_array, differentiable_eval_tree_array
@reexport import .EvaluateEquationDerivativeModule:
    eval_diff_tree_array, eval_grad_tree_array
@reexport import .SimplifyEquationModule: combine_operators, simplify_tree!
@reexport import .EvaluationHelpersModule
@reexport import .ExtensionInterfaceModule: node_to_symbolic, symbolic_to_node
@reexport import .RandomModule: NodeSampler
@reexport import .AsArrayModule: as_array

function __init__()
    @require_extensions
end

include("deprecated.jl")

import TOML: parsefile

const PACKAGE_VERSION = let
    project = parsefile(joinpath(pkgdir(@__MODULE__), "Project.toml"))
    VersionNumber(project["version"])
end

macro ignore(args...) end
# To get LanguageServer to register library within tests
@ignore include("../test/runtests.jl")

include("precompile.jl")
do_precompilation(; mode=:precompile)

end
