module RandomModule

using Compat: Returns, @inline
using Random: AbstractRNG
using ..NodeModule: AbstractNode, tree_mapreduce, filter_map
using ..ExpressionModule: AbstractExpression, get_tree

import Base: rand

"""
    NodeSampler(; tree, filter::Function=Returns(true), weighting::Union{Nothing,Function}=nothing, break_sharing::Val=Val(false))

Defines a sampler of nodes in a tree.

# Arguments

- `tree`: The tree to sample nodes from. For a regular `Node`,
  nodes are sampled uniformly. For a `GraphNode`, nodes are also
  sampled uniformly (e.g., in `sin(x) + {x}`, the `x` has equal
  probability of being sampled from the `sin` or the `+` node, because
  it is shared), unless `break_sharing` is set to `Val(true)`.
- `filter::Function`: A function that takes a node and returns a boolean
  indicating whether the node should be sampled. Defaults to `Returns(true)`.
- `weighting::Union{Nothing,Function}`: A function that takes a node and
  returns a weight for the node, if it passes the filter, proportional
  to the probability of sampling the node. If `nothing`, all nodes are
  sampled uniformly.
- `break_sharing::Val`: If `Val(true)`, the
  sampler will break sharing in the tree, and sample nodes uniformly
  from the tree.
"""
Base.@kwdef struct NodeSampler{
    N<:Union{AbstractNode,AbstractExpression},F<:Function,W<:Union{Nothing,Function},B<:Val
}
    tree::N
    weighting::W = nothing
    filter::F = Returns(true)
    break_sharing::B = Val(false)
end

"""
    rand(rng::AbstractRNG, tree::AbstractNode)

Sample a node from a tree according to the default sampler `NodeSampler(; tree)`.
"""
rand(rng::AbstractRNG, tree::Union{AbstractNode,AbstractExpression}) =
    rand(rng, NodeSampler(; tree))

"""
    rand(rng::AbstractRNG, sampler::NodeSampler)

Sample a node from a tree according to the sampler `sampler`.
"""
function rand(rng::AbstractRNG, sampler::NodeSampler{N,F,Nothing}) where {N,F}
    n = count(sampler.filter, get_tree(sampler.tree); sampler.break_sharing)
    if n == 0
        error("No nodes matching $(sampler.filter) were found in $(sampler.tree).")
    end
    idx = rand(rng, 1:n)
    return _get_node(get_tree(sampler.tree), sampler.filter, idx, sampler.break_sharing)
end
function rand(rng::AbstractRNG, sampler::NodeSampler{N,F,W}) where {N,F,W<:Function}
    t = get_tree(sampler.tree)
    weights = filter_map(
        sampler.filter, sampler.weighting, t, Float64; sampler.break_sharing
    )
    if length(weights) == 0
        error("No nodes matching $(sampler.filter) were found in $(sampler.tree).")
    end
    idx = _sample_idx(rng, weights)
    return _get_node(t, sampler.filter, idx, sampler.break_sharing)
end

function _get_node(
    tree, filter_f::F, idx::Int, ::Val{break_sharing}
) where {F,break_sharing}
    i = Ref(0)
    out = Ref(tree)
    foreach(tree; break_sharing=Val(break_sharing)) do node
        if @inline(filter_f(node)) && (i[] += 1) == idx
            out[] = node
        end
        nothing
    end
    return out[]
end

function _sample_idx(rng::AbstractRNG, weights)
    csum = cumsum(weights)
    if iszero(csum[end])
        error("Cumulative weighting of nodes in tree is zero.")
    end
    r = rand(rng, eltype(weights)) * csum[end]
    return findfirst(ci -> ci > r, csum)::Int
end

end
