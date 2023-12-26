module RandomModule

import Compat: Returns
import Random: AbstractRNG
import Base: rand
import ..EquationModule: AbstractNode, tree_mapreduce, filter_map

"""
    NodeSampler(; tree, filter::Function=Returns(true), weighting::Union{Nothing,Function}=nothing, break_sharing::Val=Val(false))

Defines a sampler of nodes in a tree. `filter` can be used to pre-filter
nodes on which to sample.

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
    N<:AbstractNode,F<:Function,W<:Union{Nothing,Function},B<:Val
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
rand(rng::AbstractRNG, tree::AbstractNode) = rand(rng, NodeSampler(; tree))

"""
    rand(rng::AbstractRNG, sampler::NodeSampler)

Sample a node from a tree according to the sampler `sampler`.
"""
function rand(rng::AbstractRNG, sampler::NodeSampler{N,F,Nothing}) where {N,F}
    n = count(sampler.filter, sampler.tree; sampler.break_sharing)
    idx = rand(rng, 1:n)
    i = Ref(0)
    out = Ref(sampler.tree)
    foreach(sampler.tree; sampler.break_sharing) do node
        if @inline(sampler.filter(node)) && (i[] += 1) == idx
            out[] = node
        end
        nothing
    end
    return out[]
end
function rand(rng::AbstractRNG, sampler::NodeSampler{N,F,W}) where {N,F,W<:Function}
    weights = filter_map(
        sampler.filter, sampler.weighting, sampler.tree, Float64; sampler.break_sharing
    )
    idx = sample_idx(rng, weights)
    i = Ref(0)
    out = Ref(sampler.tree)
    foreach(sampler.tree; sampler.break_sharing) do node
        if @inline(sampler.filter(node)) && (i[] += 1) == idx
            out[] = node
        end
        nothing
    end
    return out[]
end
function sample_idx(rng::AbstractRNG, weights)
    normalization = sum(weights)
    r = rand(rng, typeof(normalization)) * normalization
    return findfirst(cumsum(weights) .> r)::Int
end

end
