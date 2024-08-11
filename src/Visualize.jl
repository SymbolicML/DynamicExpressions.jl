module VisualizeModule

using ..NodeModule: GraphNode, Node
using ..OperatorEnumModule: AbstractOperatorEnum

function visualize(
    graph::Union{GraphNode,Node},  # types accepted by topological_sort
    operators::AbstractOperatorEnum,
    show = true
)
    error("Please load the Plots.jl and GraphRecipes.jl packages to use this feature.")
end

end