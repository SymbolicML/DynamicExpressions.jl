module StructuredExpressionModule

using ..OperatorEnumModule: AbstractOperatorEnum, OperatorEnum
using ..NodeModule: AbstractExpressionNode, Node, tree_mapreduce
using ..ExpressionModule: AbstractExpression, Metadata, node_type
using ..ChainRulesModule: NodeTangent

import ..ExpressionModule:
    get_contents,
    get_metadata,
    get_tree,
    get_operators,
    get_variable_names,
    Metadata,
    _copy,
    default_node_type,
    node_type,
    get_constants,
    set_constants!

"""
    StructuredExpression

This expression type allows you to combine multiple expressions
together in a predefined way.

For example, we can create two expressions, `f`, and `g`,
and then combine them together in a new expression, `f_plus_g`,
using a constructor function that simply adds them together:

```julia
f = parse_expression(:(x * x - cos(2.5f0 * y + -0.5f0)); kws...)
g = parse_expression(:(exp(-(y * y))); kws...)

f_plus_g = StructuredExpression((; f, g), nt -> nt.f + nt.g)
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
    F,
    EX<:NamedTuple,
    N<:AbstractExpressionNode{T},
    E<:AbstractExpression{T,N},
    TS<:NamedTuple{<:Any,<:NTuple{<:Any,E}},
    D<:@NamedTuple{structure::F, operators::O, variable_names::V, extra::EX} where {O,V},
} <: AbstractExpression{T,N}
    trees::TS
    metadata::Metadata{D}

    function StructuredExpression(
        trees::TS, metadata::Metadata{D}
    ) where {
        TS,
        F,
        EX,
        D<:@NamedTuple{
            structure::F, operators::O, variable_names::V, extra::EX
        } where {O,V},
    }
        E = typeof(first(values(trees)))
        N = node_type(E)
        return new{eltype(N),F,EX,N,E,TS,D}(trees, metadata)
    end
end

function StructuredExpression(
    trees::NamedTuple,
    structure::F;
    operators::Union{AbstractOperatorEnum,Nothing}=nothing,
    variable_names::Union{AbstractVector{<:AbstractString},Nothing}=nothing,
    extra...,
) where {F<:Function}
    example_tree = first(values(trees))
    operators = get_operators(example_tree, operators)
    variable_names = get_variable_names(example_tree, variable_names)
    metadata = (; structure, operators, variable_names, extra=(; extra...))
    return StructuredExpression(trees, Metadata(metadata))
end

function Base.copy(e::StructuredExpression)
    ts = get_contents(e)
    meta = get_metadata(e)
    copy_ts = NamedTuple{keys(ts)}(map(copy, values(ts)))
    return StructuredExpression(
        copy_ts,
        Metadata((;
            meta.structure,
            operators=_copy(meta.operators),
            variable_names=_copy(meta.variable_names),
            extra=_copy(meta.extra),
        )),
    )
end
#! format: off
function get_contents(e::StructuredExpression)
    return e.trees
end
function get_metadata(e::StructuredExpression)
    return e.metadata
end
function get_tree(e::StructuredExpression)
    return get_tree(e.metadata.structure(e.trees))
end
function get_operators(e::StructuredExpression, operators::Union{AbstractOperatorEnum,Nothing}=nothing)
    return operators === nothing ? e.metadata.operators : operators
end
function get_variable_names(e::StructuredExpression, variable_names::Union{AbstractVector{<:AbstractString},Nothing}=nothing)
    return variable_names === nothing ? e.metadata.variable_names : variable_names
end
function get_constants(e::StructuredExpression)
    # Get constants for each inner expression
    consts_and_refs = map(get_constants, values(e.trees))
    flat_constants = vcat(map(first, consts_and_refs)...)
    # Collect info so we can put them back in the right place,
    # like the indexes of the constants in the flattened array
    refs = map(c_ref -> (; n=length(first(c_ref)), ref=last(c_ref)), consts_and_refs)
    return flat_constants, refs
end
function set_constants!(e::StructuredExpression, constants, refs)
    cursor = Ref(1)
    foreach(values(e.trees), refs) do tree, r
        n = r.n
        i = cursor[]
        c = constants[i:(i+n-1)]
        set_constants!(tree, c, r.ref)
        cursor[] += n
    end
    return e
end
#! format: on

end
