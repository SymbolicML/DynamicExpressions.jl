import DynamicExpressions: Node, NodeSampler, copy_node, count_nodes, has_constants, has_operators

# This code is copied from SymbolicRegression.jl and modified

function make_random_leaf(nfeatures::Integer, ::Type{T})::Node{T} where {T}
    if rand() > 0.5
        return Node(; val=randn(T))
    else
        return Node(T; feature=rand(1:nfeatures))
    end
end

# Add a random unary/binary operation to the end of a tree
function append_random_op(
    tree::Node{T}, operators, nfeatures::Integer; make_new_bin_op::Union{Bool,Nothing}=nothing
)::Node{T} where {T}
    nuna = length(operators.unaops)
    nbin = length(operators.binops)

    choice = rand()
    make_new_bin_op = make_new_bin_op === nothing ? choice < nbin / (nuna + nbin) : make_new_bin_op

    new_node = if make_new_bin_op
        Node(
            rand(1:nbin), make_random_leaf(nfeatures, T), make_random_leaf(nfeatures, T)
        )
    else
        Node(rand(1:nuna), make_random_leaf(nfeatures, T))
    end

    tree.degree == 0 && return new_node

    has_child_leaf(t) = (t.degree > 0 && t.l.degree == 0) || (t.degree > 1 && t.r.degree == 0)
    node = rand(NodeSampler(; tree, filter=has_child_leaf))
    if node.degree == 1 || node.r.degree != 0 || (node.l.degree == 0 && rand(Bool))
        node.l = new_node
    else
        node.r = new_node
    end

    return tree
end

function gen_random_tree_fixed_size(
    node_count::Integer, operators, nfeatures::Integer, ::Type{T}
)::Node{T} where {T}
    tree = make_random_leaf(nfeatures, T)
    cur_size = count_nodes(tree)
    while cur_size < node_count
        if cur_size == node_count - 1  # only unary operator allowed.
            length(operators.unaops) == 0 && break # We will go over the requested amount, so we must break.
            tree = append_random_op(tree, operators, nfeatures; make_new_bin_op=false)
        else
            tree = append_random_op(tree, operators, nfeatures)
        end
        cur_size = count_nodes(tree)
    end
    return tree
end
