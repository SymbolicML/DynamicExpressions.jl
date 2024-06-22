"""Check that interfaces are implemented correctly."""
module InterfacesModule

using Interfaces: Interfaces, @interface, @implements, Arguments
using ..OperatorEnumModule: AbstractOperatorEnum, OperatorEnum
using ..NodeModule:
    Node,
    GraphNode,
    AbstractExpressionNode,
    preserve_sharing,
    constructorof,
    default_allocator,
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
    set_node!,
    filter_map,
    filter_map!
using ..NodeUtilsModule:
    NodeIndex,
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
    Expression,
    AbstractExpression,
    get_tree,
    get_operators,
    get_variable_names,
    with_tree,
    default_node
using ..ParametricExpressionModule: ParametricExpression, ParametricNode

###############################################################################
# ExpressionInterface #########################################################
###############################################################################

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
function _check_copy(ex::AbstractExpression)
    cex = copy(ex)
    preserves = typeof(cex) === typeof(ex) && cex == ex
    # TODO: Could include checks for aliasing here
    return preserves
end

## optional
function _check_count_nodes(ex::AbstractExpression)
    return count_nodes(ex) isa Int64
end
function _check_count_constants(ex::AbstractExpression)
    return count_constants(ex) isa Int64
end
function _check_count_depth(ex::AbstractExpression)
    return count_depth(ex) isa Int64
end
function _check_index_constants(ex::AbstractExpression)
    return index_constants(ex) isa NodeIndex{UInt16}
end
function _check_has_operators(ex::AbstractExpression)
    return has_operators(ex) isa Bool
end
function _check_has_constants(ex::AbstractExpression)
    return has_constants(ex) isa Bool
end
function _check_get_constants(ex::AbstractExpression{T}) where {T}
    output = get_constants(ex)
    return first(output) isa AbstractVector{T} && length(output) == 2
end
function _check_set_constants!(ex::AbstractExpression)
    (x, refs) = get_constants(ex)
    x2 = map(xi -> xi * 2, x)
    set_constants!(ex, x2, refs)
    return first(get_constants(ex)) ≈ x2
end
function _check_string_tree(ex::AbstractExpression)
    return string_tree(ex) isa String
end
function _check_default_node(ex::AbstractExpression)
    E = Base.typename(typeof(ex)).wrapper
    return default_node(E) <: AbstractExpressionNode
end
function _check_constructorof(ex::AbstractExpression)
    return constructorof(typeof(ex)) isa Base.Callable
end
function _check_tree_mapreduce(ex::AbstractExpression{T,N}) where {T,N}
    return tree_mapreduce(node -> [node], vcat, ex) isa Vector{N}
end

#! format: off
ei_components = (
    mandatory = (
        get_tree = "extracts the expression tree from `AbstractExpression`" => _check_get_tree,
        get_operators = "returns the operators used in the expression (or pass `operators` explicitly to override)" => _check_get_operators,
        get_variable_names = "returns the variable names used in the expression (or pass `variable_names` explicitly to override)" => _check_get_variable_names,
        copy = "returns a copy of the expression" => _check_copy,
    ),
    optional = (
        count_nodes = "counts the number of nodes in the expression tree" => _check_count_nodes,
        count_constants = "counts the number of constants in the expression tree" => _check_count_constants,
        count_depth = "calculates the depth of the expression tree" => _check_count_depth,
        index_constants = "indexes constants in the expression tree" => _check_index_constants,
        has_operators = "checks if the expression has operators" => _check_has_operators,
        has_constants = "checks if the expression has constants" => _check_has_constants,
        get_constants = "gets constants from the expression tree" => _check_get_constants,
        set_constants! = "sets constants in the expression tree" => _check_set_constants!,
        string_tree = "returns a string representation of the expression tree" => _check_string_tree,
        default_node = "returns the default node type for the expression" => _check_default_node,
        constructorof = "gets the constructor function for a type" => _check_constructorof,
        tree_mapreduce = "applies a function across the tree" => _check_tree_mapreduce
    )
)
ei_description = (
    "Defines a generic interface for user-facing expression types, "
    * "which can store operators, extra parameters, functional forms, "
    * "variable names, etc."
)
all_ei_methods_except(t) = Tuple(setdiff(keys(ei_components.optional), t))

@interface(
    ExpressionInterface,
    AbstractExpression,
    ei_components,
    ei_description
)

@implements(
    ExpressionInterface{all_ei_methods_except(())},
    Expression,
    [Arguments()]
)
@implements(
    ExpressionInterface{all_ei_methods_except((:count_constants, :index_constants, :has_constants))},
    ParametricExpression,
    [Arguments()]
)
#! format: on

###############################################################################
# NodeInterface ###############################################################
###############################################################################

## mandatory
function _check_create_node(tree::AbstractExpressionNode)
    N = typeof(tree)
    NT = with_type_parameters(N, Float16)
    return NT() isa NT
end
function _check_copy(tree::AbstractExpressionNode)
    return copy(tree) isa typeof(tree)
end
function _check_hash(tree::AbstractExpressionNode)
    return hash(tree) isa UInt64
end
function _check_any(tree::AbstractExpressionNode)
    return any(_ -> false, tree) isa Bool && any(_ -> true, tree)
end
function _check_equality(tree::AbstractExpressionNode)
    return (tree == copy(tree)) && (tree == tree)
end
function _check_preserve_sharing(tree::AbstractExpressionNode)
    return preserve_sharing(tree) isa Bool
end
function _check_constructorof(tree::AbstractExpressionNode)
    return constructorof(typeof(tree)) isa Base.Callable
end
function _check_eltype(tree::AbstractExpressionNode{T}) where {T}
    return eltype(typeof(tree)) == eltype(tree) == T
end
function _check_with_type_parameters(tree::AbstractExpressionNode{T}) where {T}
    N = typeof(tree)
    NT = with_type_parameters(Base.typename(N).wrapper, eltype(tree))
    return NT == typeof(tree)
end
function _check_default_allocator(tree::AbstractExpressionNode)
    N = Base.typename(typeof(tree)).wrapper
    return default_allocator(N, Float64) isa with_type_parameters(N, Float64)
end
function _check_set_node!(tree::AbstractExpressionNode)
    new_tree = copy(tree)
    set_node!(tree, new_tree)
    return tree == new_tree
end
function _check_count_nodes(tree::AbstractExpressionNode)
    return count_nodes(tree) isa Int64
end
function _check_tree_mapreduce(tree::AbstractExpressionNode)
    return tree_mapreduce(_ -> 1, +, tree, Int64) ==
           tree_mapreduce(_ -> 1, _ -> 1, +, tree, Int64) ==
           count_nodes(tree)
end

## optional
function _check_leaf_copy(tree::AbstractExpressionNode)
    tree.degree != 0 && return true
    return leaf_copy(tree) isa typeof(tree)
end
function _check_leaf_hash(tree::AbstractExpressionNode)
    tree.degree != 0 && return true
    return leaf_hash(UInt(0), tree) isa UInt64
end
function _check_leaf_equal(tree::AbstractExpressionNode)
    tree.degree != 0 && return true
    return leaf_equal(tree, copy(tree))
end
function _check_branch_copy(tree::AbstractExpressionNode)
    if tree.degree == 0
        return true
    elseif tree.degree == 1
        return branch_copy(tree, tree.l) isa typeof(tree)
    else
        return branch_copy(tree, tree.l, tree.r) isa typeof(tree)
    end
end
function _check_branch_hash(tree::AbstractExpressionNode)
    tree.degree == 0 && return true
    return branch_hash(UInt64(0), tree) isa UInt64
end
function _check_branch_equal(tree::AbstractExpressionNode)
    tree.degree == 0 && return true
    return branch_equal(tree, copy(tree))
end
function _check_count_depth(tree::AbstractExpressionNode)
    return count_depth(tree) isa Int64
end
function _check_count_constants(tree::AbstractExpressionNode)
    return count_constants(tree) isa Int64
end
function _check_filter_map(tree::AbstractExpressionNode)
    return filter_map(_ -> true, identity, tree, typeof(tree)) isa Vector{typeof(tree)}
end
function _check_has_constants(tree::AbstractExpressionNode)
    return has_constants(tree) isa Bool
end
function _check_get_constants(tree::AbstractExpressionNode{T}) where {T}
    output = get_constants(tree)
    return first(output) isa AbstractVector{T} && length(output) == 2
end
function _check_set_constants!(tree::AbstractExpressionNode)
    constants, refs = get_constants(tree)
    new_constants = map(x -> x * 2, constants)
    set_constants!(tree, new_constants, refs)
    return get_constants(tree)[1] == new_constants
end
function _check_index_constants(tree::AbstractExpressionNode)
    return index_constants(tree) isa NodeIndex{UInt16}
end
function _check_has_operators(tree::AbstractExpressionNode)
    return has_operators(tree) isa Bool
end

#! format: off
ni_components = (
    mandatory = (
        create_node = "creates a new instance of the node type" => _check_create_node,
        copy = "returns a copy of the tree" => _check_copy,
        hash = "returns the hash of the tree" => _check_hash,
        any = "checks if any element of the tree satisfies a condition" => _check_any,
        equality = "checks equality of the tree with itself and its copy" => _check_equality,
        preserve_sharing = "checks if the node type preserves sharing" => _check_preserve_sharing,
        constructorof = "gets the constructor function for a node type" => _check_constructorof,
        eltype = "gets the element type of the node" => _check_eltype,
        with_type_parameters = "applies type parameters to the node type" => _check_with_type_parameters,
        default_allocator = "gets the default allocator for the node type" => _check_default_allocator,
        set_node! = "sets the node's value" => _check_set_node!,
        count_nodes = "counts the number of nodes in the tree" => _check_count_nodes,
        tree_mapreduce = "applies a function across the tree" => _check_tree_mapreduce
    ),
    optional = (
        leaf_copy = "copies a leaf node" => _check_leaf_copy,
        leaf_hash = "computes the hash of a leaf node" => _check_leaf_hash,
        leaf_equal = "checks equality of two leaf nodes" => _check_leaf_equal,
        branch_copy = "copies a branch node" => _check_branch_copy,
        branch_hash = "computes the hash of a branch node" => _check_branch_hash,
        branch_equal = "checks equality of two branch nodes" => _check_branch_equal,
        count_depth = "calculates the depth of the tree" => _check_count_depth,
        count_constants = "counts the number of constants" => _check_count_constants,
        filter_map = "applies a filter and map function to the tree" => _check_filter_map,
        has_constants = "checks if the tree has constants" => _check_has_constants,
        get_constants = "gets constants from the tree" => _check_get_constants,
        set_constants! = "sets constants in the tree" => _check_set_constants!,
        index_constants = "indexes constants in the tree" => _check_index_constants,
        has_operators = "checks if the tree has operators" => _check_has_operators
    )
)

ni_description = (
    "Defines a generic interface for node types, "
    * "which can include various operations such as copying, hashing, and checking equality, "
    * "as well as tree-specific operations like map-reduce and node manipulation."
)

all_ni_methods_except(t) = Tuple(setdiff(keys(ni_components.optional), t))

@interface(
    NodeInterface,
    AbstractExpressionNode,
    ni_components,
    ni_description
)

@implements(
    NodeInterface{all_ni_methods_except(())},
    Node,
    [Arguments()]
)
@implements(
    NodeInterface{all_ni_methods_except(())},
    GraphNode,
    [Arguments()]
)
@implements(
    NodeInterface{all_ni_methods_except(())},
    ParametricNode,
    [Arguments()]
)

#! format: on

# TODO: Create an interface for evaluation

end
