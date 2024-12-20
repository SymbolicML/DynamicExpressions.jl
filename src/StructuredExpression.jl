module StructuredExpressionModule

using ..OperatorEnumModule: AbstractOperatorEnum, OperatorEnum
using ..NodeModule: AbstractExpressionNode, Node, tree_mapreduce
using ..ExpressionModule: AbstractExpression, Metadata, node_type
using ..ChainRulesModule: NodeTangent

import ..NodeModule: constructorof
import ..NodePreallocationModule: copy_into!, allocate_container
import ..ExpressionModule:
    get_contents,
    get_metadata,
    get_tree,
    get_operators,
    get_variable_names,
    with_contents,
    Metadata,
    _copy,
    unpack_metadata,
    default_node_type,
    node_type,
    get_scalar_constants,
    set_scalar_constants!
import ..ReadOnlyNodeModule: ReadOnlyNode

abstract type AbstractStructuredExpression{
    T,F<:Function,N<:AbstractExpressionNode{T},E<:AbstractExpression{T,N},D<:NamedTuple
} <: AbstractExpression{T,N} end

"""
    StructuredExpression{T,F,N,E,TS,D} <: AbstractStructuredExpression{T,F,N,E,D} <: AbstractExpression{T,N}

This expression type allows you to combine multiple expressions
together in a predefined way.

# Parameters

- `T`: The numeric value type of the expressions.
- `F`: The type of the structure function, which combines each expression into a single expression.
- `N`: The type of the nodes inside expressions.
- `E`: The type of the expressions.
- `TS`: The type of the named tuple containing those inner expressions.
- `D`: The type of the metadata, another named tuple.

# Usage

For example, we can create two expressions, `f`, and `g`,
and then combine them together in a new expression, `f_plus_g`,
using a constructor function that simply adds them together:

```julia
kws = (;
    binary_operators=[+, -, *, /],
    unary_operators=[-, cos, exp],
    variable_names=["x", "y"],
)
f = parse_expression(:(x * x - cos(2.5f0 * y + -0.5f0)); kws...)
g = parse_expression(:(exp(-(y * y))); kws...)

f_plus_g = StructuredExpression((; f, g); structure=nt -> nt.f + nt.g)
```

Now, when evaluating `f_plus_g`, this expression type will
return the result of adding together the results of `f` and `g`.

You can dispatch on a particular structured expression with the second
type parameter, `F`, which is the function defined above:

```julia
my_factory(nt) = nt.f + nt.g

Base.show(io::IO, e::StructuredExpression{T,typeof(my_factory)}) where {T} = ...
```

which will create a new method particular to this expression type defined on that function.
"""
struct StructuredExpression{
    T,
    F<:Function,
    N<:AbstractExpressionNode{T},
    E<:AbstractExpression{T,N},
    TS<:NamedTuple{<:Any,<:NTuple{<:Any,E}},
    D<:@NamedTuple{structure::F, operators::O, variable_names::V} where {O,V},
} <: AbstractStructuredExpression{T,F,N,E,D}
    trees::TS
    metadata::Metadata{D}

    function StructuredExpression(
        trees::TS, metadata::Metadata{D}
    ) where {
        TS,
        F<:Function,
        D<:@NamedTuple{structure::F, operators::O, variable_names::V} where {O,V},
    }
        E = typeof(first(values(trees)))
        N = node_type(E)
        return new{eltype(N),F,N,E,TS,D}(trees, metadata)
    end
end

function StructuredExpression(
    trees::NamedTuple;
    structure::F,
    operators::Union{AbstractOperatorEnum,Nothing}=nothing,
    variable_names::Union{AbstractVector{<:AbstractString},Nothing}=nothing,
) where {F<:Function}
    example_tree = first(values(trees))
    operators = get_operators(example_tree, operators)
    variable_names = get_variable_names(example_tree, variable_names)
    metadata = (; structure, operators, variable_names)
    return StructuredExpression(trees, Metadata(metadata))
end
constructorof(::Type{<:StructuredExpression}) = StructuredExpression
function Base.copy(e::AbstractStructuredExpression)
    ts = get_contents(e)
    meta = get_metadata(e)
    meta_inner = unpack_metadata(meta)
    copy_ts = NamedTuple{keys(ts)}(map(copy, values(ts)))
    keys_except_structure = filter(!=(:structure), keys(meta_inner))
    copy_metadata = (;
        meta_inner.structure,
        NamedTuple{keys_except_structure}(
            map(_copy, values(meta_inner[keys_except_structure]))
        )...,
    )
    return constructorof(typeof(e))(copy_ts, Metadata(copy_metadata))
end
function get_contents(e::AbstractStructuredExpression)
    return e.trees
end
function get_metadata(e::AbstractStructuredExpression)
    return e.metadata
end
function get_tree(e::AbstractStructuredExpression)
    return ReadOnlyNode(get_tree(get_metadata(e).structure(get_contents(e))))
end
function get_operators(
    e::AbstractStructuredExpression, operators::Union{AbstractOperatorEnum,Nothing}=nothing
)
    return operators === nothing ? get_metadata(e).operators : operators
end
function get_variable_names(
    e::AbstractStructuredExpression,
    variable_names::Union{AbstractVector{<:AbstractString},Nothing}=nothing,
)
    return if variable_names !== nothing
        variable_names
    elseif hasproperty(get_metadata(e), :variable_names)
        get_metadata(e).variable_names
    else
        nothing
    end
end
function get_scalar_constants(e::AbstractStructuredExpression)
    # Get constants for each inner expression
    consts_and_refs = map(get_scalar_constants, values(get_contents(e)))
    flat_constants = vcat(map(first, consts_and_refs)...)
    # Collect info so we can put them back in the right place,
    # like the indexes of the constants in the flattened array
    refs = map(c_ref -> (; n=length(first(c_ref)), ref=last(c_ref)), consts_and_refs)
    return flat_constants, refs
end
function set_scalar_constants!(e::AbstractStructuredExpression, constants, refs)
    cursor = Ref(1)
    foreach(values(get_contents(e)), refs) do tree, r
        n = r.n
        i = cursor[]
        c = constants[i:(i + n - 1)]
        set_scalar_constants!(tree, c, r.ref)
        cursor[] += n
    end
    return e
end

function allocate_container(
    e::AbstractStructuredExpression, n::Union{Nothing,Integer}=nothing
)
    ts = get_contents(e)
    return (; trees=NamedTuple{keys(ts)}(map(t -> allocate_container(t, n), values(ts))))
end
function copy_into!(dest::NamedTuple, src::AbstractStructuredExpression)
    ts = get_contents(src)
    new_contents = NamedTuple{keys(ts)}(map(copy_into!, values(dest.trees), values(ts)))
    return with_contents(src, new_contents)
end

end
