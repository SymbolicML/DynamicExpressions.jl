module ExtensionInterfaceModule

function node_to_symbolic(args...; kws...)
    return error("Please load the `SymbolicUtils` package to use `node_to_symbolic`.")
end
function symbolic_to_node(args...; kws...)
    return error("Please load the `SymbolicUtils` package to use `symbolic_to_node`.")
end

function _zygote_gradient(args...)
    return error("Please load the Zygote.jl package.")
end

function bumper_eval_tree_array(args...)
    return error("Please load the Bumper.jl package to use this feature.")
end

_is_loopvectorization_loaded(_) = false

end
