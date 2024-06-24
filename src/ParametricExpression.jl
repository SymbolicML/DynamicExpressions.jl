module ParametricExpressionModule

using DispatchDoctor: @stable, @unstable

using ..OperatorEnumModule: AbstractOperatorEnum
using ..NodeModule: AbstractExpressionNode, Node, tree_mapreduce
using ..ExpressionModule: AbstractExpression, Metadata

import ..NodeModule: constructorof, preserve_sharing, leaf_copy, leaf_hash, leaf_equal
import ..NodeUtilsModule:
    count_constants,
    index_constants,
    has_operators,
    has_constants,
    get_constants,
    set_constants!
import ..StringsModule: string_tree
import ..EvaluateModule: eval_tree_array
import ..EvaluateDerivativeModule: eval_grad_tree_array
import ..EvaluationHelpersModule: _grad_evaluator
import ..ExpressionModule:
    get_contents,
    get_metadata,
    get_tree,
    get_operators,
    get_variable_names,
    max_feature,
    default_node_type
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

"""
    ParametricExpression{T,N<:ParametricNode{T},D<:NamedTuple} <: AbstractExpression{T,N}

(Experimental) An expression to store parameters for a tree
"""
struct ParametricExpression{
    T,
    N<:ParametricNode{T},
    D<:NamedTuple{(:operators, :variable_names, :parameters, :parameter_names)},
} <: AbstractExpression{T,N}
    tree::N
    metadata::Metadata{D}

    function ParametricExpression(tree::ParametricNode, metadata::Metadata)
        return new{eltype(tree),typeof(tree),typeof(_data(metadata))}(tree, metadata)
    end
end
function ParametricExpression(
    tree::ParametricNode{T1};
    operators::Union{AbstractOperatorEnum,Nothing},
    variable_names,
    parameters::AbstractMatrix{T2},
    parameter_names,
) where {T1,T2}
    if !isnothing(parameter_names)
        @assert size(parameters, 1) == length(parameter_names)
    end
    T = promote_type(T1, T2)
    t = T === T1 ? tree : convert(ParametricNode{T}, tree)
    m = Metadata((;
        operators,
        variable_names,
        parameters=convert(AbstractArray{T}, parameters),
        parameter_names,
    ))
    return ParametricExpression(t, m)
end

###############################################################################
# Abstract expression node interface ##########################################
###############################################################################
@unstable constructorof(::Type{<:ParametricNode}) = ParametricNode
@unstable constructorof(::Type{<:ParametricExpression}) = ParametricExpression
@unstable default_node_type(::Type{<:ParametricExpression}) = ParametricNode
default_node_type(::Type{<:ParametricExpression{T}}) where {T} = ParametricNode{T}
preserve_sharing(::Union{Type{<:ParametricNode},ParametricNode}) = false # TODO: Change this?
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
get_contents(ex::ParametricExpression) = ex.tree
get_metadata(ex::ParametricExpression) = ex.metadata
get_tree(ex::ParametricExpression) = ex.tree
function get_operators(
    ex::ParametricExpression, operators::Union{AbstractOperatorEnum,Nothing}=nothing
)
    return operators === nothing ? ex.metadata.operators : operators
end
function get_variable_names(
    ex::ParametricExpression,
    variable_names::Union{Nothing,AbstractVector{<:AbstractString}}=nothing,
)
    return variable_names === nothing ? ex.metadata.variable_names : variable_names
end
@inline _copy_with_nothing(x) = copy(x)
@inline _copy_with_nothing(::Nothing) = nothing
function Base.copy(ex::ParametricExpression; break_sharing::Val=Val(false))
    return ParametricExpression(
        copy(ex.tree; break_sharing=break_sharing);
        operators=_copy_with_nothing(ex.metadata.operators),
        variable_names=_copy_with_nothing(ex.metadata.variable_names),
        parameters=_copy_with_nothing(ex.metadata.parameters),
        parameter_names=_copy_with_nothing(ex.metadata.parameter_names),
    )
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
end
_interface_error() = throw(InterfaceError())
Base.showerror(io::IO, e::InterfaceError) = print(io,
    "You should not use this function with `ParametricExpression`. " *
    "Instead, rewrite the calling function to work directly with parametric expressions, " *
    "which has two concepts of what constitutes a constant: a static, global constant, " *
    "as well as a per-instance constant."
)
count_constants(::ParametricExpression; kws...) = _interface_error()
index_constants(::ParametricExpression, ::Type{T}=UInt16) where {T} = _interface_error()
has_constants(ex::ParametricExpression) = _interface_error()
#! format: on

has_operators(ex::ParametricExpression) = has_operators(get_tree(ex))
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

function Base.convert(::Type{Node}, ex::ParametricExpression{T}) where {T}
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
#! format: off
function (ex::ParametricExpression)(X::AbstractMatrix, operators::Union{AbstractOperatorEnum,Nothing}=nothing; kws...)
    return eval_tree_array(ex, X, operators; kws...)  # Will error
end
function eval_tree_array(::ParametricExpression{T}, ::AbstractMatrix{T}, operators::Union{AbstractOperatorEnum,Nothing}=nothing; kws...) where {T}
    return error("Incorrect call. You must pass the `classes::Vector` argument when calling `eval_tree_array`.")
end
#! format: on
function (ex::ParametricExpression)(
    X::AbstractMatrix{T},
    classes::AbstractVector{<:Integer},
    operators::Union{AbstractOperatorEnum,Nothing}=nothing;
    kws...,
) where {T}
    (output, flag) = eval_tree_array(ex, X, classes, operators; kws...)  # Will error
    if !flag
        output .= NaN
    end
    return output
end
function eval_tree_array(
    ex::ParametricExpression{T},
    X::AbstractMatrix{T},
    classes::AbstractVector{<:Integer},
    operators::Union{AbstractOperatorEnum,Nothing}=nothing;
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
    regular_tree = convert(Node, ex)
    return eval_tree_array(regular_tree, params_and_X, get_operators(ex, operators); kws...)
end
function string_tree(
    ex::ParametricExpression,
    operators::Union{AbstractOperatorEnum,Nothing}=nothing;
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
    _parameter_names = ex.metadata.parameter_names
    parameter_names = if _parameter_names === nothing
        ["p$(i)" for i in 1:num_params]
    else
        _parameter_names
    end
    variable_names3 = if variable_names2 === nothing
        vcat(parameter_names, ["x$(i)" for i in 1:max_feature])
    else
        vcat(parameter_names, variable_names2)
    end
    @assert length(variable_names3) >= num_params + max_feature
    return string_tree(
        convert(Node, ex),
        get_operators(ex, operators);
        variable_names=variable_names3,
        kws...,
    )
end

# We also set up parsing for convenience:
@unstable function parse_leaf(
    ex,
    variable_names,
    node_type::Type{<:ParametricNode},
    expression_type::Type{<:ParametricExpression};
    parameter_names,
    kws...,
)
    @assert !(ex isa AbstractExpression)
    if ex isa Symbol
        @assert (!isnothing(parameter_names) && string(ex) ∈ parameter_names) ||
            (!isnothing(variable_names) && string(ex) ∈ variable_names)
        if !isnothing(variable_names)
            i = findfirst(==(string(ex)), variable_names)
            if i !== nothing
                return node_type(Float64; feature=i::Int)
            end
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
###############################################################################

end
