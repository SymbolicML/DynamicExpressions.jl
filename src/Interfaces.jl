module InterfacesModule

using Interfaces: Interfaces, @interface
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
    AbstractExpression, get_tree, get_operators, get_variable_names, with_tree, default_node

# ExpressionInterface

## mandatory
function _check_get_tree(ex::AbstractExpression{T,N}) where {T,N}
    return get_tree(ex) isa N
end
function _check_get_operators(ex::AbstractExpression)
    return get_operators(ex) isa AbstractOperatorEnum
end
function _check_get_variable_names(ex::AbstractExpression)
    return get_variable_names(ex) isa AbstractVector{<:AbstractString}
end

## optional
function _check_count_nodes(ex)
    return count_nodes(ex) isa Int
end
function _check_count_constants(ex)
    return count_constants(ex) isa Int
end
function _check_count_depth(ex)
    return count_depth(ex) isa Int
end
function _check_index_constants(ex)
    return index_constants(ex) isa AbstractVector{UInt16}
end
function _check_has_operators(ex)
    return has_operators(ex) isa Bool
end
function _check_has_constants(ex)
    return has_constants(ex) isa Bool
end
function _check_get_constants(ex::AbstractExpression{T}) where {T}
    output = get_constants(ex)
    return first(output) isa AbstractVector{T} && length(output) == 2
end
function _check_set_constants(ex)
    (x, refs) = get_constants(ex)
    x2 = copy(x) .* 2
    set_constants!(ex, x2, refs)
    return first(get_constants(ex)) ≈ x2
end
function _check_string_tree(ex)
    return string_tree(ex) isa String
end
function _check_default_node(ex)
    E = Base.typename(typeof(ex)).wrapper
    return default_node(E) <: AbstractExpressionNode
end
function _check_constructorof(ex)
    return constructorof(typeof(ex)) isa Base.Callable
end
function _check_tree_mapreduce(ex::AbstractExpression{T,N}) where {T,N}
    return tree_mapreduce(node -> [node], vcat, ex) isa Vector{N}
end

#! format: off
expression_interface_components = (
    mandatory = (
        get_tree = "extracts the expression tree from `AbstractExpression`" => _check_get_tree,
        get_operators = "returns the operators used in the expression (or pass `operators` explicitly to override)" => _check_get_operators,
        get_variable_names = "returns the variable names used in the expression (or pass `variable_names` explicitly to override)" => _check_get_variable_names,
    ),
    optional = (
        count_nodes = "counts the number of nodes in the expression tree" => _check_count_nodes,
        count_constants = "counts the number of constants in the expression tree" => _check_count_constants,
        count_depth = "calculates the depth of the expression tree" => _check_count_depth,
        index_constants = "indexes constants in the expression tree" => _check_index_constants,
        has_operators = "checks if the expression has operators" => _check_has_operators,
        has_constants = "checks if the expression has constants" => _check_has_constants,
        get_constants = "gets constants from the expression tree" => _check_get_constants,
        set_constants = "sets constants in the expression tree" => _check_set_constants,
        string_tree = "returns a string representation of the expression tree" => _check_string_tree,
        default_node = "returns the default node type for the expression" => _check_default_node,
        constructorof = "gets the constructor function for a type" => _check_constructorof,
        tree_mapreduce = "applies a function across the tree" => _check_tree_mapreduce
    )
)
expression_interface_description = (
    "Defines a generic interface for user-facing expression types, "
    * "which can store operators, extra parameters, functional forms, "
    * "variable names, etc."
)
#! format: on

@interface(
    ExpressionInterface,
    AbstractExpression,
    expression_interface_components,
    expression_interface_description
)

function check_expression_interface(expressions, skip_methods::Tuple=())
    all_methods = keys(expression_interface_components.optional)
    methods_to_test = Tuple(setdiff(all_methods, skip_methods))
    E = Base.typename(eltype(expressions)).wrapper
    return Interfaces.test(ExpressionInterface{methods_to_test}, E, expressions)
end

# _check_eval_tree_array(ex) = eval_tree_array(ex) isa AbstractArray
# _check_eval_grad_tree_array(ex) = eval_grad_tree_array(ex) isa AbstractArray
# _check_grad_evaluator(ex) = _grad_evaluator(ex) isa Function
# _check_preserve_sharing(ex) = preserve_sharing(ex) isa Bool

# eval_tree_array = "evaluates the expression tree as an array" => check_eval_tree_array,
# eval_grad_tree_array = "evaluates the gradient of the expression tree as an array" => check_eval_grad_tree_array,
# grad_evaluator = "evaluates the gradient of the expression" => check_grad_evaluator,
# preserve_sharing = "checks if the expression preserves sharing" => _check_preserve_sharing,
# @interface ExpressionEvaluationInterface

# with_type_parameters
# leaf_copy = "copies a leaf node" => check_leaf_copy,
# leaf_hash = "computes the hash of a leaf node" => check_leaf_hash,
# leaf_equal = "checks equality of two leaf nodes" => check_leaf_equal,
# branch_copy = "copies a branch node" => check_branch_copy,
# branch_hash = "computes the hash of a branch node" => check_branch_hash,
# branch_equal = "checks equality of two branch nodes" => check_branch_equal,
# @interface NodeInterface
# @interface NodeEvaluationInterface

end
