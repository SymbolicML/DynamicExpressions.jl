"""Check that interfaces are implemented correctly."""
module InterfacesModule

using Interfaces: Interfaces, @interface, @implements, Arguments
using DispatchDoctor: @unstable
using ..UtilsModule: Nullable
using ..OperatorEnumModule: AbstractOperatorEnum, OperatorEnum
using ..NodeModule:
    Node,
    GraphNode,
    AbstractExpressionNode,
    preserve_sharing,
    constructorof,
    default_allocator,
    with_type_parameters,
    with_max_degree,
    max_degree,
    has_max_degree,
    unsafe_get_children,
    get_children,
    leaf_copy,
    leaf_convert,
    leaf_hash,
    leaf_equal,
    branch_copy,
    branch_convert,
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
    is_node_constant,
    count_constant_nodes,
    count_depth,
    index_constant_nodes,
    has_operators,
    has_constants,
    get_scalar_constants,
    set_scalar_constants!
using ..NodePreallocationModule:
    copy_into!, leaf_copy_into!, branch_copy_into!, allocate_container
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
    get_contents,
    get_metadata,
    with_contents,
    with_metadata,
    default_node_type
using ..ParametricExpressionModule: ParametricExpression, ParametricNode
using ..StructuredExpressionModule: StructuredExpression

###############################################################################
# ExpressionInterface #########################################################
###############################################################################

## mandatory
function _check_get_contents(ex::AbstractExpression)
    new_ex = with_contents(ex, get_contents(ex))
    return new_ex == ex && new_ex isa typeof(ex)
end
function _check_get_metadata(ex::AbstractExpression)
    new_ex = with_metadata(ex, get_metadata(ex))
    return new_ex == ex && new_ex isa typeof(ex)
end
function _check_get_tree(
    ex::AbstractExpression{T,N}
) where {T,D,N<:AbstractExpressionNode{T,D}}
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
function _check_with_contents(ex::AbstractExpression)
    new_ex = with_contents(ex, get_contents(ex))
    new_ex2 = with_contents(ex, ex)
    return new_ex == ex && new_ex isa typeof(ex) && new_ex2 == ex && new_ex2 isa typeof(ex)
end
function _check_with_metadata(ex::AbstractExpression)
    new_ex = with_metadata(ex, get_metadata(ex))
    return new_ex == ex && new_ex isa typeof(ex)
end

## optional
function _check_copy_into!(ex::AbstractExpression)
    container = allocate_container(ex)
    prealloc_ex = copy_into!(container, ex)
    return container !== nothing && prealloc_ex == ex && prealloc_ex !== ex
end
function _check_count_nodes(ex::AbstractExpression)
    return count_nodes(ex) isa Int64
end
function _check_count_constant_nodes(ex::AbstractExpression)
    return count_constant_nodes(ex) isa Int64
end
function _check_count_depth(ex::AbstractExpression)
    return count_depth(ex) isa Int64
end
function _check_index_constant_nodes(ex::AbstractExpression)
    return index_constant_nodes(ex) isa NodeIndex{UInt16}
end
function _check_has_operators(ex::AbstractExpression)
    return has_operators(ex) isa Bool
end
function _check_has_constants(ex::AbstractExpression)
    return has_constants(ex) isa Bool
end
function _check_get_constants(ex::AbstractExpression{T}) where {T}
    output = get_scalar_constants(ex)
    return first(output) isa AbstractVector{T} && length(output) == 2
end
function _check_set_constants!(ex::AbstractExpression)
    (x, refs) = get_scalar_constants(ex)
    x2 = map(xi -> xi * 2, x)
    set_scalar_constants!(ex, x2, refs)
    return first(get_scalar_constants(ex)) ≈ x2
end
function _check_string_tree(ex::AbstractExpression)
    return string_tree(ex) isa AbstractString &&
           string_tree(ex; pretty=false) isa AbstractString &&
           string_tree(ex; pretty=true) isa AbstractString
end
function _check_default_node(ex::AbstractExpression{T}) where {T}
    ET = typeof(ex)
    E = Base.typename(ET).wrapper
    return default_node_type(E) <: AbstractExpressionNode &&
           default_node_type(ET) <: AbstractExpressionNode{T} &&
           !has_max_degree(default_node_type(ET))
end
function _check_constructorof(ex::AbstractExpression)
    return constructorof(typeof(ex)) isa Base.Callable
end
function _check_tree_mapreduce(
    ex::AbstractExpression{T,N}
) where {T,D,N<:AbstractExpressionNode{T,D}}
    return tree_mapreduce(node -> [node], vcat, ex) isa Vector{<:N}
end

#! format: off
ei_components = (
    mandatory = (
        get_contents = "extracts the runtime contents of an expression" => _check_get_contents,
        get_metadata = "extracts the runtime metadata of an expression" => _check_get_metadata,
        get_tree = "extracts the expression tree from [`AbstractExpression`](@ref)" => _check_get_tree,
        get_operators = "returns the operators used in the expression (or pass `operators` explicitly to override)" => _check_get_operators,
        get_variable_names = "returns the variable names used in the expression (or pass `variable_names` explicitly to override)" => _check_get_variable_names,
        copy = "returns a copy of the expression" => _check_copy,
        with_contents = "returns the expression with different tree" => _check_with_contents,
        with_metadata = "returns the expression with different metadata" => _check_with_metadata,
    ),
    optional = (
        copy_into! = "copies an expression into a preallocated container" => _check_copy_into!,
        count_nodes = "counts the number of nodes in the expression tree" => _check_count_nodes,
        count_constant_nodes = "counts the number of constant nodes in the expression tree" => _check_count_constant_nodes,
        count_depth = "calculates the depth of the expression tree" => _check_count_depth,
        index_constant_nodes = "indexes constants in the expression tree" => _check_index_constant_nodes,
        has_operators = "checks if the expression has operators" => _check_has_operators,
        has_constants = "checks if the expression has constants" => _check_has_constants,
        get_scalar_constants = ("gets constants from the expression tree, returning a tuple of: " *
                        "(1) a flat vector of the constants, and (2) an reference object that " *
                        "can be used by `set_scalar_constants!` to efficiently set them back") => _check_get_constants,
        set_scalar_constants! = ("sets constants in the expression tree, given: " *
                        "(1) a flat vector of constants, (2) the expression, and " *
                        "(3) the reference object produced by `get_scalar_constants`") => _check_set_constants!,
        string_tree = "returns a string representation of the expression tree" => _check_string_tree,
        default_node_type = "returns the default node type for the expression" => _check_default_node,
        constructorof = "gets the constructor function for a type" => _check_constructorof,
        tree_mapreduce = "applies a function across the tree" => _check_tree_mapreduce,
    )
)
ei_description = (
    "Defines the interface of [`AbstractExpression`](@ref) for user-facing expression types, "
    * "which can store operators, extra parameters, functional forms, "
    * "variable names, etc."
)
@unstable all_ei_methods_except(t) = Tuple(setdiff(keys(ei_components.optional), t))

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
    ExpressionInterface{all_ei_methods_except((:count_constant_nodes, :index_constant_nodes, :has_constants))},
    ParametricExpression,
    [Arguments()]
)
@implements(
    ExpressionInterface{all_ei_methods_except(())},
    StructuredExpression,
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
function _check_get_children(tree::AbstractExpressionNode{T,D}) where {T,D}
    tree.degree == 0 && return true
    return unsafe_get_children(tree) isa NTuple{D,Nullable{typeof(tree)}} &&
           get_children(tree, tree.degree) isa Tuple &&
           length(get_children(tree, tree.degree)) == tree.degree &&
           length(get_children(tree, Val(1))) == 1
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
    Nf16 = with_type_parameters(N, Float16)
    return Nf16 <: AbstractExpressionNode{Float16}
end
function _check_with_max_degree(tree::AbstractExpressionNode)
    N = typeof(tree)
    new_D = max_degree(N) + 1
    N2 = with_max_degree(N, Val(new_D))
    return N2 <: AbstractExpressionNode && max_degree(N2) == new_D
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
function _check_copy_into!(tree::AbstractExpressionNode)
    container = allocate_container(tree)
    prealloc_tree = copy_into!(container, tree)
    return container !== nothing && prealloc_tree == tree && prealloc_tree !== container
end
function _check_leaf_copy(tree::AbstractExpressionNode)
    tree.degree != 0 && return true
    return leaf_copy(tree) isa typeof(tree)
end
function _check_leaf_copy_into!(tree::AbstractExpressionNode{T}) where {T}
    tree.degree != 0 && return true
    new_leaf = constructorof(typeof(tree))(; val=zero(T))
    ret = leaf_copy_into!(new_leaf, tree)
    return new_leaf == tree && ret === new_leaf
end
function _check_leaf_convert(tree::AbstractExpressionNode)
    tree.degree != 0 && return true
    return leaf_convert(typeof(tree), tree) isa typeof(tree) &&
           leaf_convert(typeof(tree), tree) == tree
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
    tree.degree == 0 && return true
    return branch_copy(tree, get_children(tree, Val(tree.degree))...) isa typeof(tree)
end
function _check_branch_copy_into!(tree::AbstractExpressionNode{T}) where {T}
    tree.degree == 0 && return true
    new_branch = constructorof(typeof(tree))(; val=zero(T))
    ret = branch_copy_into!(
        new_branch, tree, map(copy, get_children(tree, Val(tree.degree)))...
    )
    return new_branch == tree && ret === new_branch
end
function _check_branch_convert(tree::AbstractExpressionNode)
    tree.degree == 0 && return true
    return branch_convert(typeof(tree), tree, get_children(tree, Val(tree.degree))...) isa
           typeof(tree)
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
function _check_is_node_constant(tree::AbstractExpressionNode)
    return is_node_constant(tree) isa Bool
end
function _check_count_constant_nodes(tree::AbstractExpressionNode)
    return count_constant_nodes(tree) isa Int64
end
function _check_filter_map(tree::AbstractExpressionNode)
    return filter_map(_ -> true, identity, tree, typeof(tree)) isa Vector{typeof(tree)}
end
function _check_has_constants(tree::AbstractExpressionNode)
    return has_constants(tree) isa Bool
end
function _check_get_constants(tree::AbstractExpressionNode{T}) where {T}
    output = get_scalar_constants(tree)
    return first(output) isa AbstractVector{T} && length(output) == 2
end
function _check_set_constants!(tree::AbstractExpressionNode)
    constants, refs = get_scalar_constants(tree)
    new_constants = map(x -> x * 2, constants)
    set_scalar_constants!(tree, new_constants, refs)
    return get_scalar_constants(tree)[1] == new_constants
end
function _check_index_constant_nodes(tree::AbstractExpressionNode)
    return index_constant_nodes(tree) isa NodeIndex{UInt16}
end
function _check_has_operators(tree::AbstractExpressionNode)
    return has_operators(tree) isa Bool
end

#! format: off
ni_components = (
    mandatory = (
        create_node = "creates a new instance of the node type" => _check_create_node,
        get_children = "returns the children of the node" => _check_get_children,
        copy = "returns a copy of the tree" => _check_copy,
        hash = "returns the hash of the tree" => _check_hash,
        any = "checks if any element of the tree satisfies a condition" => _check_any,
        equality = "checks equality of the tree with itself and its copy" => _check_equality,
        preserve_sharing = "checks if the node type preserves sharing" => _check_preserve_sharing,
        constructorof = "gets the constructor function for a node type" => _check_constructorof,
        eltype = "gets the element type of the node" => _check_eltype,
        with_type_parameters = "applies type parameters to the node type" => _check_with_type_parameters,
        with_max_degree = "changes the maximum degree of a node type" => _check_with_max_degree,
        default_allocator = "gets the default allocator for the node type" => _check_default_allocator,
        set_node! = "sets the node's value" => _check_set_node!,
        count_nodes = "counts the number of nodes in the tree" => _check_count_nodes,
        tree_mapreduce = "applies a function across the tree" => _check_tree_mapreduce
    ),
    optional = (
        copy_into! = "copies a node into a preallocated container" => _check_copy_into!,
        leaf_copy = "copies a leaf node" => _check_leaf_copy,
        leaf_copy_into! = "copies a leaf node in-place" => _check_leaf_copy_into!,
        leaf_convert = "converts a leaf node" => _check_leaf_convert,
        leaf_hash = "computes the hash of a leaf node" => _check_leaf_hash,
        leaf_equal = "checks equality of two leaf nodes" => _check_leaf_equal,
        branch_copy = "copies a branch node" => _check_branch_copy,
        branch_copy_into! = "copies a branch node in-place" => _check_branch_copy_into!,
        branch_convert = "converts a branch node" => _check_branch_convert,
        branch_hash = "computes the hash of a branch node" => _check_branch_hash,
        branch_equal = "checks equality of two branch nodes" => _check_branch_equal,
        count_depth = "calculates the depth of the tree" => _check_count_depth,
        is_node_constant = "checks if the node is a constant" => _check_is_node_constant,
        count_constant_nodes = "counts the number of constant nodes" => _check_count_constant_nodes,
        filter_map = "applies a filter and map function to the tree" => _check_filter_map,
        has_constants = "checks if the tree has constants" => _check_has_constants,
        get_scalar_constants = ("gets constants from the tree, returning a tuple of: " *
                        "(1) a flat vector of the constants, and (2) a reference object that " *
                        "can be used by `set_scalar_constants!` to efficiently set them back") => _check_get_constants,
        set_scalar_constants! = ("sets constants in the tree, given: " *
                        "(1) a flat vector of constants, (2) the tree, and " *
                        "(3) the reference object produced by `get_scalar_constants`") => _check_set_constants!,
        index_constant_nodes = "indexes constants in the tree" => _check_index_constant_nodes,
        has_operators = "checks if the tree has operators" => _check_has_operators,
    )
)

ni_description = (
    "Defines the interface for [`AbstractExpressionNode`](@ref) "
    * "which can include various operations such as copying, hashing, and checking equality, "
    * "as well as tree-specific operations like map-reduce and node manipulation."
)

@unstable all_ni_methods_except(t) = Tuple(setdiff(keys(ni_components.optional), t))

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

# TODO: Create an interface for evaluation and `extract_gradient`
# extract_gradient = ("given a Zygote-computed gradient with respect to the tree constants, " *
#                     "extracts a flat vector in the same order as `get_scalar_constants`") => _check_extract_gradient,

end
