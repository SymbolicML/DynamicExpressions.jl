using DynamicExpressions:
    AbstractExpressionNode,
    AbstractNode,
    Node,
    NodeSampler,
    constructorof,
    set_node!,
    count_nodes
using Random: AbstractRNG, default_rng

"""
    random_node(tree::AbstractNode; filter::F=Returns(true))

Return a random node from the tree. You may optionally
filter the nodes matching some condition before sampling.
"""
function random_node(
    tree::AbstractNode; rng::AbstractRNG=default_rng(), filter::F=Returns(true)
) where {F<:Function}
    Base.depwarn(
        "Instead of `random_node(tree, filter)`, use `rand(NodeSampler(; tree, filter))`",
        :random_node,
    )
    return rand(rng, NodeSampler(; tree, filter))
end

function make_random_leaf(
    nfeatures::Int, ::Type{T}, ::Type{N}; rng::AbstractRNG=default_rng()
) where {T,N<:AbstractExpressionNode}
    if rand(rng, Bool)
        return constructorof(N)(; val=randn(rng, T))
    else
        return constructorof(N)(T; feature=rand(rng, 1:nfeatures))
    end
end

"""Add a random unary/binary operation to the end of a tree"""
function append_random_op(
    tree::AbstractExpressionNode{T},
    operators,
    nfeatures::Int;
    rng::AbstractRNG=default_rng(),
    makeNewBinOp::Union{Bool,Nothing}=nothing,
) where {T}
    node = rand(rng, NodeSampler(; tree, filter=t -> t.degree == 0))
    nuna = length(operators.unaops)
    nbin = length(operators.binops)

    if makeNewBinOp === nothing
        choice = rand(rng)
        makeNewBinOp = choice < nbin / (nuna + nbin)
    end

    if makeNewBinOp
        newnode = constructorof(typeof(tree))(
            rand(rng, 1:nbin),
            make_random_leaf(nfeatures, T, typeof(tree), rng),
            make_random_leaf(nfeatures, T, typeof(tree), rng),
        )
    else
        newnode = constructorof(typeof(tree))(
            rand(rng, 1:nuna), make_random_leaf(nfeatures, T, typeof(tree), rng)
        )
    end

    set_node!(node, newnode)

    return tree
end

function gen_random_tree_fixed_size(
    node_count::Int,
    operators,
    nfeatures::Int,
    ::Type{T};
    node_type=Node,
    rng::AbstractRNG=default_rng(),
) where {T}
    tree = make_random_leaf(nfeatures, T, node_type, rng)
    cur_size = count_nodes(tree)
    while cur_size < node_count
        if cur_size == node_count - 1  # only unary operator allowed.
            length(operators.unaops) == 0 && break # We will go over the requested amount, so we must break.
            tree = append_random_op(tree, operators, nfeatures, rng; makeNewBinOp=false)
        else
            tree = append_random_op(tree, operators, nfeatures, rng)
        end
        cur_size = count_nodes(tree)
    end
    return tree
end
