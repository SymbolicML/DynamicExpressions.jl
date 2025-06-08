module ParametricExpressionModule

using DispatchDoctor: @stable, @unstable
using ChainRulesCore: ChainRulesCore as CRC, NoTangent, @thunk

using ..OperatorEnumModule: AbstractOperatorEnum, OperatorEnum
using ..NodeModule: AbstractExpressionNode, Node, tree_mapreduce
using ..ExpressionModule:
    AbstractExpression, Metadata, with_contents, with_metadata, unpack_metadata
using ..ChainRulesModule: NodeTangent

import ..NodeModule:
    constructorof,
    with_type_parameters,
    with_max_degree,
    with_default_max_degree,
    max_degree,
    preserve_sharing,
    leaf_copy,
    leaf_convert,
    leaf_hash,
    leaf_equal,
    set_leaf!,
    @make_accessors
import ..NodePreallocationModule: copy_into!, allocate_container
import ..NodeUtilsModule:
    count_constant_nodes,
    index_constant_nodes,
    has_operators,
    has_constants,
    get_scalar_constants,
    set_scalar_constants!
import ..StringsModule: string_tree
import ..EvaluateModule: eval_tree_array
import ..EvaluateDerivativeModule: eval_grad_tree_array
import ..EvaluationHelpersModule: _grad_evaluator
import ..ChainRulesModule: extract_gradient
import ..ExpressionModule:
    get_contents,
    get_metadata,
    get_tree,
    get_operators,
    get_variable_names,
    max_feature,
    default_node_type
import ..ParseModule: parse_leaf
import ..ValueInterfaceModule:
    count_scalar_constants, pack_scalar_constants!, unpack_scalar_constants

"""A type of expression node that also stores a parameter index"""
mutable struct ParametricNode{T,D} <: AbstractExpressionNode{T,D}
    degree::UInt8
    constant::Bool  # if true => constant; if false, then check `is_parameter`
    val::T
    feature::UInt16

    is_parameter::Bool  # if true => parameter; if false, then is `feature`
    parameter::UInt16  # Stores index of per-class parameter

    op::UInt8
    children::NTuple{D,ParametricNode{T,D}}  # Children nodes

    function ParametricNode{_T,_D}() where {_T,_D}
        n = new{_T,_D}()
        n.is_parameter = false
        n.parameter = UInt16(0)
        return n
    end
    # TODO: Test with this disabled to spot any unintended uses
    function ParametricNode{_T}() where {_T}
        return ParametricNode{_T,2}()
    end
end

@make_accessors ParametricNode

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
        return new{eltype(tree),typeof(tree),typeof(unpack_metadata(metadata))}(
            tree, metadata
        )
    end
end
function ParametricExpression(
    tree::ParametricNode{T1};
    operators::Union{AbstractOperatorEnum,Nothing},
    variable_names=nothing,
    parameters::AbstractMatrix{T2},
    parameter_names=nothing,
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
@unstable constructorof(::Type{<:ParametricExpression}) = ParametricExpression
@unstable function default_node_type(::Type{<:ParametricExpression})
    return with_default_max_degree(ParametricNode)
end
function default_node_type(::Type{N}) where {T,N<:ParametricExpression{T}}
    return ParametricNode{T,max_degree(N)}
end
preserve_sharing(::Union{Type{<:ParametricNode},ParametricNode}) = false # TODO: Change this?
function leaf_copy(t::ParametricNode{T}) where {T}
    if t.constant
        return constructorof(typeof(t))(; val=t.val)
    elseif !t.is_parameter
        return constructorof(typeof(t))(T; feature=t.feature)
    else
        n = constructorof(typeof(t))(; val=zero(T))
        n.constant = false
        n.is_parameter = true
        n.parameter = t.parameter
        return n
    end
end
function set_leaf!(tree::ParametricNode, new_leaf::ParametricNode)
    if new_leaf.constant
        tree.constant = true
        tree.val = new_leaf.val
    elseif !new_leaf.is_parameter
        tree.constant = false
        tree.is_parameter = false
        tree.feature = new_leaf.feature
    else
        tree.constant = false
        tree.is_parameter = true
        tree.parameter = new_leaf.parameter
    end
    return nothing
end
function leaf_convert(::Type{N}, t::ParametricNode) where {T,N<:ParametricNode{T}}
    if t.constant
        return constructorof(N)(T; val=convert(T, t.val))
    elseif t.is_parameter
        n = constructorof(N)(T; val=zero(T))
        n.constant = false
        n.is_parameter = true
        n.parameter = t.parameter
        return n
    else
        return constructorof(N)(T; feature=t.feature)
    end
end
function leaf_hash(h::UInt, t::ParametricNode)
    if t.constant
        return hash((:constant, t.val), h)
    else
        if t.is_parameter
            return hash((:parameter, t.parameter), h)
        else
            return hash((:feature, t.feature), h)
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
    return if variable_names !== nothing
        variable_names
    elseif hasproperty(ex.metadata, :variable_names)
        ex.metadata.variable_names
    else
        nothing
    end
end
function Base.copy(ex::ParametricExpression; break_sharing::Val=Val(false))
    return ParametricExpression(copy(ex.tree; break_sharing), copy(ex.metadata))
end
###############################################################################

###############################################################################
# Extra utilities for parametric-specific behavior ############################
###############################################################################

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
count_constant_nodes(::ParametricExpression; kws...) = _interface_error()
index_constant_nodes(::ParametricExpression, ::Type{T}=UInt16) where {T} = _interface_error()
has_constants(::ParametricExpression) = _interface_error()
#! format: on

has_operators(ex::ParametricExpression) = has_operators(get_tree(ex))

function _get_constants_array(parameter_refs, ::Type{BT}) where {BT}
    size = sum(count_scalar_constants, parameter_refs)
    flat = Vector{BT}(undef, size)
    ix = 1
    for p in parameter_refs
        ix = pack_scalar_constants!(flat, ix, p)
    end
    return flat
end

function _set_constants_array!(parameter_refs, flat)
    ix, i = 1, 1
    while ix <= length(flat) && i <= length(parameter_refs)
        ix, parameter_refs[i] = unpack_scalar_constants(flat, ix, parameter_refs[i])
        i += 1
    end
end

function get_scalar_constants(ex::ParametricExpression{T}) where {T}
    constants, constant_refs = get_scalar_constants(get_tree(ex))
    parameters = get_metadata(ex).parameters
    flat_parameters = _get_constants_array(parameters, eltype(constants))
    num_constants = length(constants)
    num_parameters = length(flat_parameters)
    combined_scalars = vcat(constants, flat_parameters)
    refs = (; constant_refs, parameter_refs=parameters, num_parameters, num_constants)
    return combined_scalars, refs
end
function set_scalar_constants!(ex::ParametricExpression{T}, x, refs) where {T}
    # First, set the usual constants
    set_scalar_constants!(
        get_tree(ex), @view(x[1:(refs.num_constants)]), refs.constant_refs
    )
    # Then, copy in the parameters
    _set_constants_array!(
        @view(get_metadata(ex).parameters[:]), @view(x[(refs.num_constants + 1):end])
    )
    return ex
end
function extract_gradient(
    gradient::@NamedTuple{
        tree::NT,
        metadata::@NamedTuple{
            _data::@NamedTuple{
                operators::Nothing,
                variable_names::Nothing,
                parameters::PARAM,
                parameter_names::Nothing,
            }
        }
    },
    ex::ParametricExpression{T,N},
) where {T,N<:ParametricNode{T},NT<:NodeTangent{T,N},PARAM<:AbstractMatrix{T}}
    d_constants = extract_gradient(gradient.tree, get_tree(ex))
    d_params = gradient.metadata._data.parameters[:]
    return vcat(d_constants, d_params)  # Same shape as `get_scalar_constants`
end

struct BranchConverter{NT<:Node} <: Function end
struct LeafConverter{NT<:Node} <: Function
    num_params::UInt16
end
function (bc::BranchConverter{NT})(op::Integer, children::Vararg{Any,M}) where {NT,M}
    return NT(; op, children)
end
function (lc::LeafConverter{NT})(leaf::ParametricNode) where {NT}
    if leaf.constant
        return NT(; val=leaf.val)
    elseif leaf.is_parameter
        return NT(; feature=leaf.parameter)
    else
        return NT(; feature=leaf.feature + lc.num_params)
    end
end
function Base.convert(::Type{Node}, ex::ParametricExpression{T}) where {T}
    num_params = UInt16(size(ex.metadata.parameters, 1))
    tree = get_tree(ex)
    _NT = typeof(tree)
    D = max_degree(_NT)
    NT = with_max_degree(with_type_parameters(Node, T), Val(D))

    return tree_mapreduce(
        LeafConverter{NT}(num_params), branch -> branch.op, BranchConverter{NT}(), tree, NT
    )
end
function CRC.rrule(::typeof(convert), ::Type{Node}, ex::ParametricExpression{T}) where {T}
    tree = get_contents(ex)
    primal = convert(Node, ex)
    pullback = let tree = tree
        d_primal -> let
            # ^The exact same tangent with respect to constants, so we can just take it.
            d_ex = @thunk(
                let
                    parametric_node_tangent = NodeTangent(tree, d_primal.gradient)
                    (;
                        tree=parametric_node_tangent,
                        metadata=(;
                            _data=(;
                                operators=NoTangent(),
                                variable_names=NoTangent(),
                                parameters=NoTangent(),
                                parameter_names=NoTangent(),
                            )
                        ),
                    )
                end
            )
            (NoTangent(), NoTangent(), d_ex)
        end
    end
    return primal, pullback
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
    (output, flag) = eval_tree_array(ex, X, classes, operators; kws...)
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
function allocate_container(
    prototype::ParametricExpression, n::Union{Nothing,Integer}=nothing
)
    return (;
        tree=allocate_container(get_contents(prototype), n),
        parameters=similar(get_metadata(prototype).parameters),
    )
end
function copy_into!(dest::NamedTuple, src::ParametricExpression)
    new_tree = copy_into!(dest.tree, get_contents(src))
    metadata = get_metadata(src)
    new_parameters = dest.parameters
    new_parameters .= metadata.parameters
    new_metadata = Metadata((;
        operators=metadata.operators,
        variable_names=metadata.variable_names,
        parameters=new_parameters,
        parameter_names=metadata.parameter_names,
    ))
    # TODO: Better interface for this^
    return with_metadata(with_contents(src, new_tree), new_metadata)
end
###############################################################################

end
