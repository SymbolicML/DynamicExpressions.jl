module DynamicExpressions

include("Utils.jl")
include("ExtensionInterface.jl")
include("OperatorEnum.jl")
include("Node.jl")
Base.@deprecate_binding EquationModule NodeModule
include("NodeUtils.jl")
Base.@deprecate_binding EquationUtilsModule NodeUtilsModule
include("Strings.jl")
include("Evaluate.jl")
Base.@deprecate_binding EvaluateEquationModule EvaluateModule
include("EvaluateDerivative.jl")
Base.@deprecate_binding EvaluateEquationDerivativeModule EvaluateDerivativeModule
include("EvaluationHelpers.jl")
include("Simplify.jl")
Base.@deprecate_binding SimplifyEquationModule SimplifyModule
include("OperatorEnumConstruction.jl")
include("Random.jl")

import PackageExtensionCompat: @require_extensions
import Reexport: @reexport
@reexport import .NodeModule:
    AbstractNode,
    AbstractExpressionNode,
    GraphNode,
    Node,
    copy_node,
    set_node!,
    tree_mapreduce,
    filter_map,
    filter_map!
import .NodeModule: constructorof, preserve_sharing
@reexport import .NodeUtilsModule:
    count_nodes,
    count_constants,
    count_depth,
    NodeIndex,
    index_constants,
    has_operators,
    has_constants,
    get_constants,
    set_constants!
@reexport import .StringsModule: string_tree, print_tree
@reexport import .OperatorEnumModule: AbstractOperatorEnum
@reexport import .OperatorEnumConstructionModule:
    OperatorEnum, GenericOperatorEnum, @extend_operators, set_default_variable_names!
@reexport import .EvaluateModule: eval_tree_array, differentiable_eval_tree_array
@reexport import .EvaluateDerivativeModule:
    eval_diff_tree_array, eval_grad_tree_array
@reexport import .SimplifyModule: combine_operators, simplify_tree!
@reexport import .EvaluationHelpersModule
@reexport import .ExtensionInterfaceModule: node_to_symbolic, symbolic_to_node
@reexport import .RandomModule: NodeSampler

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
