module DynamicExpressions

include("Utils.jl")
include("OperatorEnum.jl")
include("Equation.jl")
include("ExtensionInterface.jl")
include("EquationUtils.jl")
include("EvaluateEquation.jl")
include("EvaluateEquationDerivative.jl")
include("EvaluationHelpers.jl")
include("SimplifyEquation.jl")
include("OperatorEnumConstruction.jl")

import Requires: @init, @require
import Reexport: @reexport
@reexport import .EquationModule:
    Node, string_tree, print_tree, copy_node, set_node!, tree_mapreduce, filter_map
@reexport import .EquationUtilsModule:
    count_nodes,
    count_constants,
    count_depth,
    NodeIndex,
    index_constants,
    has_operators,
    has_constants,
    get_constants,
    set_constants!
@reexport import .OperatorEnumModule: AbstractOperatorEnum
@reexport import .OperatorEnumConstructionModule:
    OperatorEnum, GenericOperatorEnum, @extend_operators
@reexport import .EvaluateEquationModule: eval_tree_array, differentiable_eval_tree_array
@reexport import .EvaluateEquationDerivativeModule:
    eval_diff_tree_array, eval_grad_tree_array
@reexport import .SimplifyEquationModule: combine_operators, simplify_tree
@reexport import .EvaluationHelpersModule
@reexport import .ExtensionInterfaceModule:
    node_to_symbolic, symbolic_to_node, generate_diff_operators

#! format: off
if !isdefined(Base, :get_extension)
    @init @require SymbolicUtils = "d1185830-fcd6-423d-90d6-eec64667417b" include("../ext/DynamicExpressionsSymbolicUtilsExt.jl")
    @init @require Zygote = "e88e6eb3-aa80-5325-afca-941959d7151f" include("../ext/DynamicExpressionsZygoteExt.jl")
end
#! format: on

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
