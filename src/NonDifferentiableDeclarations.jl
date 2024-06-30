module NonDifferentiableDeclarationsModule

using ChainRulesCore: @non_differentiable
import ..NodeUtilsModule: tree_mapreduce
import ..ExpressionModule: get_operators, get_variable_names, _validate_input

@non_differentiable tree_mapreduce(args...)
@non_differentiable get_operators(ex, operators)
@non_differentiable get_variable_names(ex, variable_names)
@non_differentiable _validate_input(ex, X, operators)

end
