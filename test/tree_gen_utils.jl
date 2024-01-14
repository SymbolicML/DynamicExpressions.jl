import DynamicExpressions:
    Node, copy_node, set_node!, count_nodes, has_constants, has_operators

# This code is copied from SymbolicRegression.jl and modified

# Return a random node from the tree
function random_node(tree::Node{T})::Node{T} where {T}
    if tree.degree == 0
        return tree
    end
    b = count_nodes(tree.l)
    c = if tree.degree == 2
        count_nodes(tree.r)
    else
        0
    end

    i = rand(1:(1 + b + c))
    if i <= b
        return random_node(tree.l)
    elseif i == b + 1
        return tree
    end

    return random_node(tree.r)
end

function make_random_leaf(nfeatures::Integer, ::Type{T})::Node{T} where {T}
    if rand() > 0.5
        return Node(; val=randn(T))
    else
        return Node(T; feature=rand(1:nfeatures))
    end
end

# Add a random unary/binary operation to the end of a tree
function append_random_op(
    tree::Node{T}, operators, nfeatures::Integer; makeNewBinOp::Union{Bool,Nothing}=nothing
)::Node{T} where {T}
    nuna = length(operators.unaops)
    nbin = length(operators.binops)

    node = random_node(tree)
    while node.degree != 0
        node = random_node(tree)
    end

    if makeNewBinOp === nothing
        choice = rand()
        makeNewBinOp = choice < nbin / (nuna + nbin)
    end

    if makeNewBinOp
        newnode = Node(
            rand(1:nbin), make_random_leaf(nfeatures, T), make_random_leaf(nfeatures, T)
        )
    else
        newnode = Node(rand(1:nuna), make_random_leaf(nfeatures, T))
    end

    set_node!(node, newnode)

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
            tree = append_random_op(tree, operators, nfeatures; makeNewBinOp=false)
        else
            tree = append_random_op(tree, operators, nfeatures)
        end
        cur_size = count_nodes(tree)
    end
    return tree
end
