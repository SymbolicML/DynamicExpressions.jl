module ExtensionInterfaceModule

function node_to_symbolic(args...; kws...)
    return error(
        "Please load the `SymbolicUtils` package to use `node_to_symbolic(::Node, ::AbstractOperatorEnum; kws...)`.",
    )
end
function symbolic_to_node(args...; kws...)
    return error(
        "Please load the `SymbolicUtils` package to use `symbolic_to_node(::Symbolic, ::AbstractOperatorEnum; kws...)`.",
    )
end

end
