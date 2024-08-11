module DynamicExpressionsVisualizeExt

using Plots, GraphRecipes, DynamicExpressions
using DynamicExpressions: GraphNode, Node, topological_sort, AbstractOperatorEnum, get_op_name

function DynamicExpressions.visualize(
    graph::Union{GraphNode,Node},  # types accepted by topological_sort
    operators::AbstractOperatorEnum,
    show = true
)
    @info "Generating graph visualization"
    
    order = reverse(topological_sort(graph))

    # multigraph adjacency list
    g = map(
        node -> convert(Vector{Int64}, map(
            cindex -> findfirst(x -> x === node.children[cindex][], order), 
            1:node.degree
        )),
        order
    )

    # node labels
    n = map(x ->
        if x.degree == 0
            x.constant ? x.val : 'x' * string(x.feature)
        elseif x.degree == 1
            join(get_op_name(operators.unaops[x.op]))
        elseif x.degree == 2
            join(get_op_name(operators.binops[x.op]))
        else
            @warn "Can't label operator node with degree > 2"
        end, 
        order
    )

    # edge labels (specifies parameter no.)
    e = Dict{Tuple{Int64, Int64, Int64}, String}()
    for (index, node) in enumerate(order)
        edge_count = Dict{Int64, Int64}() # count number of edges to each child node
        for cindex in 1:node.degree
            order_cindex = findfirst(x -> x === node.children[cindex][], order)
            get!(
                e, 
                (
                    index, # source
                    order_cindex, # dest
                    get!(edge_count, order_cindex, pop!(edge_count, order_cindex, 0)+1) # edge no.
                ), 
                string(cindex)
            )
        end
    end

    # node colours
    c = map(x -> x == 1 ? 2 : 1, eachindex(order))

    return graphplot(
        g,
        names = n,
        edgelabel = e,
        nodecolor = c,
        show = show,
        nodeshape=:circle,
        edge_label_box = false,
        edgelabel_offset = 0.015,
        nodesize=0.15
    )
end

end