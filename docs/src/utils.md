# Node utilities

## `Base`

Various functions in `Base` are overloaded to treat an `AbstractNode` as a
collection of its nodes.

```@docs
copy(tree::AbstractExpressionNode; break_sharing::Val=Val(false))
filter(f::Function, tree::AbstractNode; break_sharing::Val=Val(false))
count(f::Function, tree::AbstractNode; init=0, break_sharing::Val=Val(false))
foreach(f::Function, tree::AbstractNode; break_sharing::Val=Val(false))
sum(f::F, tree::AbstractNode; init=0, return_type=Undefined, f_on_shared=_default_shared_aggregation, break_sharing::Val=Val(false)) where {F<:Function}
mapreduce(f::F, op::G, tree::AbstractNode; return_type, f_on_shared, break_sharing) where {F<:Function,G<:Function}
any(f::F, tree::AbstractNode) where {F<:Function}
all(f::F, tree::AbstractNode) where {F<:Function}
map(f::F, tree::AbstractNode, result_type::Type{RT}=Nothing; break_sharing::Val=Val(false)) where {F<:Function,RT}
convert(::Type{<:AbstractExpressionNode{T1}}, n::AbstractExpressionNode{T2}) where {T1,T2}
hash(tree::AbstractExpressionNode{T}, h::UInt; break_sharing::Val=Val(false)) where {T}
```

## Printing

Trees are printed using the `string_tree` function, which is very
configurable:

```@docs
string_tree(tree::Node, operators::AbstractOperatorEnum)
```

The standard `show` and `print` methods will use the most recently-created `OperatorEnum`
in a `string_tree`.

## Sampling

There are also methods for random sampling of nodes:

```@docs
NodeSampler
rand(rng::AbstractRNG, tree::AbstractNode)
rand(rng::AbstractRNG, sampler::NodeSampler{N,F,Nothing}) where {N,F}
```

## Internal utilities

Almost all node utilities are crafted using the `tree_mapreduce` function,
which evaluates a mapreduce over a tree-like (or graph-like) structure:

```@docs
tree_mapreduce
```

Various other utility functions include the following:

```@docs
filter_map
filter_map!
```
