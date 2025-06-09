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
hash(tree::AbstractExpressionNode{T}, h::UInt; break_sharing::Val=Val(false)) where {T}
```

## Printing

Trees are printed using the `