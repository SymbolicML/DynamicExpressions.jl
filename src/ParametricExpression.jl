module ParametricExpressionModule

using DispatchDoctor: @unstable

using ..NodeModule:
    AbstractExpressionNode, Node, with_type_parameters, constructorof, tree_mapreduce
using ..ExpressionModule: AbstractExpression, Metadata

import ..NodeModule: preserve_sharing, leaf_copy, leaf_hash, leaf_equal
import ..NodeUtilsModule:
    count_constants,
    index_constants,
    has_operators,
    has_constants,
    get_constants,
    set_constants!
import ..StringsModule: string_tree
import ..SimplifyModule: combine_operators, simplify_tree!
import ..EvaluateModule: eval_tree_array
import ..EvaluateDerivativeModule: eval_grad_tree_array
import ..EvaluationHelpersModule: _grad_evaluator
import ..ExpressionModule:
    get_tree, get_operators, get_variable_names, max_feature, default_node
import ..ParseModule: parse_leaf

"""A type of expression node that also stores a parameter index"""
mutable struct ParametricNode{T} <: AbstractExpressionNode{T}
    degree::UInt8
    constant::Bool  # if true => constant; if false, then check `is_parameter`
    val::T
    feature::UInt16

    is_parameter::Bool  # if true => parameter; if false, then is `feature`
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
@inline _data(x::Metadata) = getfield(x, :_data)
"""An expression to store parameters for a tree"""
struct ParametricExpression{T,N<:AbstractExpressionNode{T},D<:NamedTuple} <:
       AbstractExpression{T,N}
    tree::N
    metadata::Metadata{D}

    function ParametricExpression(tree::ParametricNode, metadata::Metadata)
        return new{eltype(tree),typeof(tree),typeof(_data(metadata))}(tree, metadata)
    end
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
@unstable default_node(::Type{<:ParametricExpression}) = ParametricNode
preserve_sharing(t::ParametricNode{T}) where {T} = false # TODO: Change this?
function leaf_copy(t::ParametricNode{T}) where {T}
    out = if t.constant
        constructorof(typeof(t))(; val=t.val)
    elseif !t.is_parameter
        constructorof(typeof(t))(T; feature=t.feature)
    else
        n = constructorof(typeof(t))(; val=zero(T))
        n.constant = false
        n.is_parameter = true
        n.parameter = t.parameter
        n
    end
    return out
end
function leaf_hash(h::UInt, t::ParametricNode)
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
function leaf_equal(a::ParametricNode, b::ParametricNode)
    if a.constant
        return b.constant && a.val == b.val
    else
        if a.is_parameter
            return b.is_parameter && a.parameter == b.parameter
        else
            return a.feature == b.feature
        end
    end
end
###############################################################################

###############################################################################
# Abstract expression interface ###############################################
###############################################################################
function get_tree(ex::ParametricExpression)
    return ex.tree
end
function get_operators(ex::ParametricExpression, operators)
    return operators === nothing ? ex.metadata.operators : operators
end
function get_variable_names(ex::ParametricExpression, variable_names)
    return variable_names === nothing ? ex.metadata.variable_names : variable_names
end
@inline _copy(x) = copy(x)
@inline _copy(::Nothing) = nothing
function Base.copy(ex::ParametricExpression; break_sharing::Val=Val(false))
    return ParametricExpression(
        copy(ex.tree; break_sharing=break_sharing);
        operators=_copy(ex.metadata.operators),
        variable_names=_copy(ex.metadata.variable_names),
        parameters=_copy(ex.metadata.parameters),
        parameter_names=_copy(ex.metadata.parameter_names),
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
count_constants(::ParametricExpression; kws...) = _interface_error()
index_constants(::ParametricExpression, ::Type{T}=UInt16) where {T} = _interface_error()
has_operators(::ParametricExpression) = _interface_error()
has_constants(::ParametricExpression) = _interface_error()
#! format: on

function get_constants(ex::ParametricExpression{T}) where {T}
    constants, constant_refs = get_constants(get_tree(ex))
    parameters = ex.metadata.parameters
    flat_parameters = parameters[:]
    num_constants = length(constants)
    num_parameters = length(flat_parameters)
    return vcat(constants, flat_parameters),
    (; constant_refs, parameter_refs=parameters, num_parameters, num_constants)
end
function set_constants!(ex::ParametricExpression{T}, x, refs) where {T}
    # First, set the usual constants
    set_constants!(get_tree(ex), @view(x[1:(refs.num_constants)]), refs.constant_refs)
    # Then, copy in the parameters
    ex.metadata.parameters[:] .= @view(x[(refs.num_constants + 1):end])
    return ex
end

function _to_node(ex::ParametricExpression{T}) where {T}
    num_params = UInt16(size(ex.metadata.parameters, 1))
    return tree_mapreduce(
        leaf -> if leaf.constant
            Node(; val=leaf.val)
        elseif leaf.is_parameter
            Node(T; feature=leaf.parameter)
        else
            Node(T; feature=leaf.feature + num_params)
        end,
        branch -> branch.op,
        (op, children...) -> Node(; op, children),
        get_tree(ex),
        Node{T},
    )
end
function eval_tree_array(
    ::ParametricExpression{T}, ::AbstractMatrix{T}, operators; kws...
) where {T}
    return error(
        "Incorrect call. You must pass the `classes::Vector` argument when calling `eval_tree_array`.",
    )
end
function eval_tree_array(
    ex::ParametricExpression{T},
    X::AbstractMatrix{T},
    classes::AbstractVector{<:Integer},
    operators=nothing;
    kws...,
) where {T}
    @assert length(classes) == size(X, 2)
    @assert maximum(classes) <= size(ex.metadata.parameters, 2)  # TODO: Remove when comfortable
    parameters = ex.metadata.parameters
    indexed_parameters = [
        parameters[i_parameter, classes[i_row]] for
        i_parameter in eachindex(axes(parameters, 1)), i_row in eachindex(classes)
    ]
    params_and_X = vcat(indexed_parameters, X)
    # Then, we create a normal `Node{T}` type from the `ParametricNode{T}`,
    # with `feature` set to the parameter index + num_features
    regular_tree = _to_node(ex)
    return eval_tree_array(regular_tree, params_and_X, get_operators(ex, operators); kws...)
end
function string_tree(
    ex::ParametricExpression,
    operators=nothing;
    variable_names=nothing,
    display_variable_names=nothing,
    X_sym_units=nothing,
    y_sym_units=nothing,
    raw=false,
    kws...,
)
    # TODO: HACK we ignore display_variable_names and others
    variable_names2 = get_variable_names(ex, variable_names)
    num_params = UInt16(size(ex.metadata.parameters, 1))
    max_feature = maximum(get_tree(ex)) do node
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
    return string_tree(
        _to_node(ex), get_operators(ex, operators); variable_names=variable_names3, kws...
    )
end

# We also set up parsing for convenience:
function parse_leaf(
    ex,
    variable_names,
    node_type::Type{<:ParametricNode},
    expression_type::Type{<:ParametricExpression};
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
    out, complete = eval_tree_array(ex, X, classes, operators; kws...)
    if !complete
        out .= NaN
    end
    return out
end
###############################################################################

function combine_operators(ex::ParametricExpression, operators=nothing; kws...)
    return ParametricExpression(
        combine_operators(get_tree(ex), get_operators(ex, operators); kws...), ex.metadata
    )
end
function simplify_tree!(ex::ParametricExpression, operators=nothing; kws...)
    return ParametricExpression(
        simplify_tree!(get_tree(ex), get_operators(ex, operators); kws...), ex.metadata
    )
end

using TestItems: @testitem

@testitem "Optimization with parameters" begin
    using DynamicExpressions
    using Random: MersenneTwister
    using Optim

    let
        variable_names = ["x", "y"]
        parameter_names = ["p1", "p2"]
        binary_operators = [+, -, *, /]
        unary_operators = [sin]
        expression_type = ParametricExpression

        rng = MersenneTwister(0)
        true_parameters = [
            -0.2 +0.2 +0.3
            +1.4 +0.5 -0.9
        ]
        init_parameters = zero(true_parameters)
        X = [
            11 12 13 14 15 16 17 18 19.0
            21 22 23 24 25 26 27 28 29
        ]
        classes = [1, 2, 3, 3, 2, 1, 1, 2, 3]

        (init_ex, true_ex) = map(
            p -> parse_expression(
                :((x * p2) + y + p1);
                variable_names,
                binary_operators,
                unary_operators,
                expression_type,
                parameters=p,
                parameter_names,
            ),
            (copy(init_parameters), copy(true_parameters)),
        )
        s = string_tree(init_ex)
        @test s == "((x * p2) + y) + p1"

        @test init_ex.metadata.parameters == init_parameters
        @test true_ex.metadata.parameters == true_parameters

        (init_out, true_out) = map(
            p -> [
                (X[1, i] * p[2, classes[i]] + X[2, i] + p[1, classes[i]]) for
                i in 1:size(X, 2)
            ],
            (init_parameters, true_parameters),
        )

        true_init_out = init_ex(X, classes)
        true_true_out = true_ex(X, classes)

        @test init_out == true_init_out
        @test true_out == true_true_out

        true_constants, true_refs = get_constants(true_ex)
        set_constants!(init_ex, true_constants, true_refs)
        @test init_ex.metadata.parameters == true_parameters

        init_loss = sum(abs2, init_out - true_out)
        # # Check if we can optimize this
        result = optimize(
            let classes = classes, X = X, true_out = true_out
                ex -> sum(abs2, ex(X, classes) - true_out)
            end,
            init_ex,
            Optim.BFGS(),
        )
        @test result.minimum < 1e-5 && init_loss > 0.1
        @test result.minimizer.metadata.parameters ≈ true_parameters
    end
end

end
