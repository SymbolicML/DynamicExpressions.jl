module DynamicExpressions

using DispatchDoctor: @stable, @unstable

@stable default_mode="disable" begin
    include("Utils.jl")
    include("ExtensionInterface.jl")
    include("OperatorEnum.jl")
    include("Node.jl")
    include("NodeUtils.jl")
    include("Strings.jl")
    include("Evaluate.jl")
    include("EvaluateDerivative.jl")
    include("ChainRules.jl")
    include("EvaluationHelpers.jl")
    include("Simplify.jl")
    include("OperatorEnumConstruction.jl")
    include("Random.jl")
    include("Expression.jl")
    include("Parse.jl")
end

import PackageExtensionCompat: @require_extensions
import Reexport: @reexport
macro ignore(args...) end

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
import .NodeModule:
    constructorof,
    with_type_parameters,
    preserve_sharing,
    leaf_copy,
    branch_copy,
    leaf_hash,
    branch_hash,
    leaf_equal,
    branch_equal
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
@reexport import .EvaluateDerivativeModule: eval_diff_tree_array, eval_grad_tree_array
@reexport import .ChainRulesModule: NodeTangent
@reexport import .SimplifyModule: combine_operators, simplify_tree!
@reexport import .EvaluationHelpersModule
@reexport import .ExtensionInterfaceModule: node_to_symbolic, symbolic_to_node
@reexport import .RandomModule: NodeSampler
@reexport import .ExpressionModule: AbstractExpression, Expression
# Not for export; just for overloading
import .ExpressionModule: get_tree, get_operators, get_variable_names, Metadata
@reexport import .ParseModule: @parse_expression, parse_expression
import .ParseModule: parse_leaf

function __init__()
    @require_extensions
end

include("deprecated.jl")

import TOML: parsefile

const PACKAGE_VERSION = let
    project = parsefile(joinpath(pkgdir(@__MODULE__), "Project.toml"))
    VersionNumber(project["version"])
end

# To get LanguageServer to register library within tests
@ignore include("../test/runtests.jl")

include("precompile.jl")
do_precompilation(; mode=:precompile)
end
