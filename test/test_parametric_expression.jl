"""Test what happens if we create an expression with a parametric field."""

using Test
using DynamicExpressions
using DynamicExpressions: DynamicExpressions as DE
using DynamicExpressions: Metadata
using DynamicExpressions: with_type_parameters

"""A type of expression node that also stores a parameter index"""
mutable struct ParametricNode{T} <: AbstractExpressionNode{T}
    degree::UInt8
    constant::Bool
    val::T
    feature::UInt16

    is_parameter::Bool  # Whether this is a parameter node
    parameter::UInt16  # Stores index of per-class parameter

    op::UInt8
    l::ParametricNode{T}
    r::ParametricNode{T}

    function ParametricNode{_T}() where {_T}
        n = new{_T}()
        n.is_parameter = false
        n.parameter = UInt16(0)
        return n
    end
end
"""An expression to store parameters for a tree"""
struct ParametricExpression{T,N<:AbstractExpressionNode{T},D<:NamedTuple} <:
       AbstractExpression{T}
    tree::N
    metadata::Metadata{D}

    function ParametricExpression(
        tree::_N;
        operators,
        variable_names,
        parameters::AbstractMatrix{_T2},
        parameter_names,
    ) where {_T1,_T2,_N<:AbstractExpressionNode{_T1}}
        _T = promote_type(_T1, _T2)
        NT = with_type_parameters(_N, _T)
        let tree = _T === _T1 ? tree : convert(NT, tree),
            parameters = _T === _T2 ? parameters : _T.(parameters)
            # Assume parameters to have shape (n_parameters, n_fitted_classes)
            @assert size(parameters, 1) == length(parameter_names)
            d = (; operators, variable_names, parameters, parameter_names)
            return new{_T,NT,typeof(d)}(tree, Metadata(d))
        end
    end
end

###############################################################################
# Abstract expression node interface ##########################################
###############################################################################
DE.preserve_sharing(t::ParametricNode{T}) where {T} = false # TODO: Change this?
function DE.leaf_copy(t::ParametricNode{T}) where {T}
    out = if t.constant
        constructorof(typeof(t))(; val=t.val)
    elseif !t.is_parameter
        constructorof(typeof(t))(T; feature=t.feature)
    else
        n = constructorof(typeof(t))(T)
        n.is_parameter = true
        n.parameter = t.parameter
        n
    end
    return out
end
function DE.leaf_hash(h::UInt, t::ParametricNode)
    if t.constant
        hash((:constant, t.val), h)
    else
        if t.is_parameter
            hash((:parameter, t.parameter), h)
        else
            hash((:feature, t.feature), h)
        end
    end
end
function DE.leaf_equal(a::ParametricNode, b::ParametricNode)
    if a.constant
        return b.constant && a.val == b.val
    else
        if a.feature
            return b.feature && a.feature == b.feature
        else
            return b.is_parameter && a.parameter == b.parameter
        end
    end
end
###############################################################################

###############################################################################
# Abstract expression interface ###############################################
###############################################################################
function DE.get_tree(ex::ParametricExpression)
    return ex.tree
end
function DE.get_operators(ex::ParametricExpression, operators)
    return operators === nothing ? ex.metadata.operators : operators
end
function DE.get_variable_names(ex::ParametricExpression, variable_names)
    return variable_names === nothing ? ex.metadata.variable_names : variable_names
end
function Base.copy(ex::ParametricExpression; break_sharing::Val=Val(false))
    return ParametricExpression(
        copy(ex.tree; break_sharing=break_sharing);
        operators=copy(ex.metadata.operators),
        variable_names=copy(ex.metadata.variable_names),
        parameters=copy(ex.metadata.parameters),
        parameter_names=copy(ex.metadata.parameter_names),
    )
end
function Base.hash(ex::ParametricExpression, h::UInt)
    return hash(ex.tree, hash(ex.metadata, h))
end
function Base.:(==)(x::ParametricExpression, y::ParametricExpression)
    return x.tree == y.tree && x.metadata == y.metadata
end
###############################################################################

###############################################################################
# Extra utilities for parametric-specific behavior ############################
###############################################################################
## As explained in AbstractExpressionNode, we can implement custom behavior for
## the parametric expression by implementing the following methods:
# - `count_nodes`
# - `count_constants`
# - `count_depth`
# - `index_constants`
# - `has_operators`
# - `has_constants`
# - `get_constants`
# - `set_constants!`
# - `string_tree`
# - `max_feature`
# - `eval_tree_array`
# - `eval_grad_tree_array`
# - `_grad_evaluator`

## For a parametric struct, we only wish to implement the following

#! format: off

struct InterfaceError <: Exception
    msg::Union{String,Nothing}
end
_interface_error() = throw(InterfaceError(nothing))
Base.showerror(io::IO, e::InterfaceError) = print(io,
    if e === nothing
        "This function should not have been called. Rewrite the calling function instead to work with parametric expressions."
    else
        e.msg
    end
)
DE.count_constants(::ParametricExpression; kws...) = _interface_error()
DE.index_constants(::ParametricExpression, ::Type{T}=UInt16) where {T} = _interface_error()
DE.has_operators(::ParametricExpression) = _interface_error()
DE.has_constants(::ParametricExpression) = _interface_error()
#! format: on

# TODO: Should we avoid flattening the array each time?
# Seems like it will result in extra copies.
_flatten(ar::AbstractMatrix) = reduce(vcat, eachrow(ar))

function DE.get_constants(ex::ParametricExpression{T}) where {T}
    return vcat(_flatten(ex.metadata.parameters), get_constants(DE.get_tree(ex)))
end
function DE.set_constants!(
    ex::ParametricExpression{T}, constants::AbstractVector{T}
) where {T}
    total_parameters = prod(size(ex.metadata.parameters))
    ex.metadata.parameters .= @view(constants[1:total_parameters])
    set_constants!(DE.get_tree(ex), @view(constants[(total_parameters + 1):end]))
    # ^ The @view is just to reduce memory usage; it's just a slice though
    return nothing
end

function _to_node(ex::ParametricExpression{T}) where {T}
    num_params = UInt16(size(ex.metadata.parameters, 1))
    return DE.tree_mapreduce(
        leaf -> if leaf.constant
            Node(; val=leaf.val)
        elseif leaf.is_parameter
            Node(T; feature=leaf.parameter)
        else
            Node(T; feature=leaf.feature + num_params)
        end,
        branch -> branch.op,
        (op, children...) -> Node(; op, children),
        DE.get_tree(ex),
        Node{T},
    )
end
function DE.eval_tree_array(
    ::ParametricExpression{T}, ::AbstractMatrix{T}, operators; kws...
) where {T}
    return error(
        "Incorrect call. You must pass the `classes::Vector` argument when calling `eval_tree_array`.",
    )
end
function DE.eval_tree_array(
    ex::ParametricExpression{T},
    X::AbstractMatrix{T},
    classes::AbstractVector{<:Integer},
    operators=nothing;
    kws...,
) where {T}
    @assert length(classes) == size(X, 2)
    @assert maximum(classes) <= size(ex.metadata.parameters, 2)  # TODO: Remove when comfortable
    parameters = ex.metadata.parameters
    indexed_parameters = parameters[:, classes]
    params_and_X = vcat(indexed_parameters, X)
    # Then, we create a normal `Node{T}` type from the `ParametricNode{T}`,
    # with `feature` set to the parameter index + num_features
    regular_tree = _to_node(ex)
    return DE.eval_tree_array(
        regular_tree, params_and_X, DE.get_operators(ex, operators); kws...
    )
end
function DE.string_tree(
    ex::ParametricExpression, operators=nothing; variable_names=nothing, kws...
)
    variable_names2 = DE.get_variable_names(ex, variable_names)
    num_params = UInt16(size(ex.metadata.parameters, 1))
    max_feature = maximum(DE.get_tree(ex)) do node
        if node.degree == 0 && !node.constant && !node.is_parameter
            node.feature
        else
            UInt16(0)
        end
    end
    variable_names3 = if variable_names2 === nothing
        vcat(["p$(i)" for i in 1:num_params], ["x$(i)" for i in 1:max_feature])
    else
        vcat(ex.metadata.parameter_names, variable_names2)
    end
    @assert length(variable_names3) >= num_params + max_feature
    return DE.string_tree(
        _to_node(ex),
        DE.get_operators(ex, operators);
        variable_names=variable_names3,
        kws...,
    )
end

# We also set up parsing for convenience:
function DE.parse_leaf(
    ex,
    variable_names,
    node_type::Type{<:ParametricNode},
    expression_type::Type{<:ParametricExpression},
    evaluate_on;
    parameter_names,
    kws...,
)
    @assert !(ex isa AbstractExpression)
    if ex isa Symbol
        @assert string(ex) ∈ parameter_names || string(ex) ∈ variable_names
        i = findfirst(==(string(ex)), variable_names)
        if i !== nothing
            return node_type(Float64; feature=i::Int)
        end
        # Special logic for parsing parameter:
        j = findfirst(==(string(ex)), parameter_names)
        n = node_type{Float64}()
        # HACK: Should implement conversion so we don't need this
        n.degree = 0
        n.constant = false
        n.is_parameter = true
        n.parameter = j::Int
        return n
    elseif ex isa AbstractExpressionNode
        return ex
    else
        return node_type(; val=ex)
    end
end

# And easy evaluation
function (ex::ParametricExpression)(X, classes, operators=nothing; kws...)
    out, complete = DE.eval_tree_array(ex, X, classes, operators; kws...)
    if !complete
        out .= NaN
    end
    return out
end
###############################################################################

let parameters = [1.0 2.0 3.0],
    ex = @parse_expression(
        sin(x) + p,
        operators = OperatorEnum(; binary_operators=(+, -), unary_operators=(sin,)),
        variable_names = ["x"],
        node_type = ParametricNode,
        expression_type = ParametricExpression,
        extra_metadata = (; parameters, parameter_names=["p"]),
    ),
    X = [0.0 π / 2 π 3π / 2 2π]

    @test ex isa ParametricExpression{Float64}
    @test ex.tree isa ParametricNode{Float64}
    @test string_tree(ex) == "sin(x) + p"

    # Evaluate on X with classes [1]
    @test ex(X, [1, 1, 1, 1, 1]) ≈ [1.0, 2.0, 1.0, 0.0, 1.0]

    # Then, with different classes
    @test ex(X, [1, 2, 2, 3, 1]) ≈ [1.0, 3.0, 2.0, 2.0, 1.0]
end

# With 2 parameters, 2 variables
let parameters = [
        1.0 1.0 0.8
        2.0 3.0 5.0
    ],
    ex = @parse_expression(
        sin(x) + y + p1 * p2,
        operators = OperatorEnum(; binary_operators=(+, -, *), unary_operators=(sin,)),
        variable_names = ["x", "y"],
        node_type = ParametricNode,
        expression_type = ParametricExpression,
        extra_metadata = (; parameters, parameter_names=["p1", "p2"]),
    ),
    X = [
        0.0 π/2 π 1.2
        0.0 0.0 1.5 0.1
    ],
    param_idx = [1, 1, 2, 3]

    @test string_tree(ex) == "(sin(x) + y) + (p1 * p2)"

    @test ex(X, param_idx) ≈ [
        sin(0.0) + 0.0 + 1.0 * 2.0
        sin(π / 2) + 0.0 + 1.0 * 2.0
        sin(π) + 1.5 + 1.0 * 3.0
        sin(1.2) + 0.1 + 0.8 * 5.0
    ]
end
