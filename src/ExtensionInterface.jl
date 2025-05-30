module ExtensionInterfaceModule

function node_to_symbolic(args...; kws...)
    return error("Please load the `SymbolicUtils` package to use `node_to_symbolic`.")
end
function symbolic_to_node(args...; kws...)
    return error("Please load the `SymbolicUtils` package to use `symbolic_to_node`.")
end

struct ZygoteGradient{F,degree} <: Function
    op::F
end

function Base.show(io::IO, g::ZygoteGradient{F,degree}) where {F,degree}
    print(io, "∂")
    print(io, g.op)
    return nothing
end
Base.show(io::IO, ::MIME"text/plain", g::ZygoteGradient) = show(io, g)

function _zygote_gradient(args...)
    return error("Please load the Zygote.jl package.")
end

function bumper_eval_tree_array(args...)
    return error("Please load the Bumper.jl package to use this feature.")
end
function bumper_kern! end

_is_loopvectorization_loaded(_) = false  # COV_EXCL_LINE

end
