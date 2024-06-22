module InterfacesModule

using Interfaces: @interface
using ..OperatorEnumModule: AbstractOperatorEnum, OperatorEnum
using ..NodeModule:
    AbstractExpressionNode,
    preserve_sharing,
    constructorof,
    with_type_parameters,
    leaf_copy,
    leaf_hash,
    leaf_equal,
    branch_copy,
    branch_hash,
    branch_equal,
    tree_mapreduce,
    copy_node,
    count_nodes,
    copy_node
using ..NodeUtilsModule:
    count_constants,
    count_depth,
    index_constants,
    has_operators,
    has_constants,
    get_constants,
    set_constants!
using ..StringsModule: string_tree
using ..EvaluateModule: eval_tree_array
using ..EvaluateDerivativeModule: eval_grad_tree_array
using ..OperatorEnumConstructionModule: _grad_evaluator
using ..ExpressionModule:
    AbstractExpression,
    get_tree,
    get_operators,
    get_variable_names,
    with_tree,
    default_node

check_get_tree(ex::AbstractExpression{T,N}) where {T,N} = get_tree(ex) isa N
check_get_operators(ex::AbstractExpression, operators::Union{AbstractOperatorEnum,Nothing}) = get_operators(ex, operators) isa AbstractOperatorEnum
check_get_variable_names(ex::AbstractExpression, variable_names::Union{AbstractVector{<:AbstractString},Nothing}) = get_variable_names(ex, variable_names) isa AbstractVector{<:AbstractString}

check_count_nodes(ex) = count_nodes(ex) isa Int
check_count_constants(ex) = count_constants(ex) isa Int
check_count_depth(ex) = count_depth(ex) isa Int
check_index_constants(ex) = index_constants(ex) isa AbstractVector{UInt16}
check_has_operators(ex) = has_operators(ex) isa Bool
check_has_constants(ex) = has_constants(ex) isa Bool
check_get_constants(ex::AbstractExpression{T}) where {T} = (output = get_constants(ex); first(output) isa AbstractVector{T} && length(output) == 2)
check_set_constants(ex) = ((x, refs) = get_constants(ex); x2 = copy(x) .* 2; set_constants!(ex, x2, refs); first(get_constants(ex)) ≈ x2)
check_string_tree(ex) = string_tree(ex) isa String
check_default_node(ex) = default_node(Base.typename(typeof(ex)).wrapper) isa AbstractExpressionNode
check_preserve_sharing(ex) = preserve_sharing(ex) isa Bool
check_constructorof(ex) = constructorof(typeof(ex)) isa Base.Callable
check_with_type_parameters(ex, params) = with_type_parameters(ex, params) isa AbstractExpressionNode
check_tree_mapreduce(ex, func) = tree_mapreduce(func, ex) isa Any

components = (
    mandatory = (
        get_tree = "extracts the expression tree from `AbstractExpression`" => check_get_tree,
        get_operators = "returns the operators used in the expression (or pass `operators` explicitly to override)" => check_get_operators,
        get_variable_names = "returns the variable names used in the expression (or pass `variable_names` explicitly to override)" => check_get_variable_names,
    ),
    optional = (
        count_nodes = "counts the number of nodes in the expression tree" => check_count_nodes,
        count_constants = "counts the number of constants in the expression tree" => check_count_constants,
        count_depth = "calculates the depth of the expression tree" => check_count_depth,
        index_constants = "indexes constants in the expression tree" => check_index_constants,
        has_operators = "checks if the expression has operators" => check_has_operators,
        has_constants = "checks if the expression has constants" => check_has_constants,
        get_constants = "gets constants from the expression tree" => check_get_constants,
        set_constants = "sets constants in the expression tree" => check_set_constants,
        string_tree = "returns a string representation of the expression tree" => check_string_tree,
        default_node = "returns the default node type for the expression" => check_default_node,
        preserve_sharing = "checks if the expression preserves sharing" => check_preserve_sharing,
        constructorof = "gets the constructor function for a type" => check_constructorof,
        with_type_parameters = "applies type parameters to an expression node" => check_with_type_parameters,
        tree_mapreduce = "applies a function across the tree" => check_tree_mapreduce
    )
)

description = (
    "Defines a generic interface for user-facing expression types, "
    "which can store operators, extra parameters, functional forms, "
    "variable names, etc."
)

@interface ExpressionInterface AbstractExpression components description

# check_eval_tree_array(ex) = eval_tree_array(ex) isa AbstractArray
# check_eval_grad_tree_array(ex) = eval_grad_tree_array(ex) isa AbstractArray
# check_grad_evaluator(ex) = _grad_evaluator(ex) isa Function
# eval_tree_array = "evaluates the expression tree as an array" => check_eval_tree_array,
# eval_grad_tree_array = "evaluates the gradient of the expression tree as an array" => check_eval_grad_tree_array,
# grad_evaluator = "evaluates the gradient of the expression" => check_grad_evaluator,
# @interface ExpressionEvaluationInterface

# leaf_copy = "copies a leaf node" => check_leaf_copy,
# leaf_hash = "computes the hash of a leaf node" => check_leaf_hash,
# leaf_equal = "checks equality of two leaf nodes" => check_leaf_equal,
# branch_copy = "copies a branch node" => check_branch_copy,
# branch_hash = "computes the hash of a branch node" => check_branch_hash,
# branch_equal = "checks equality of two branch nodes" => check_branch_equal,
# @interface NodeInterface
# @interface NodeEvaluationInterface



end
