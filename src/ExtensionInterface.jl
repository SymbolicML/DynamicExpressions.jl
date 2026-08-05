module ExtensionInterfaceModule

_is_extension_loaded(::Val) = false

function node_to_symbolic(args...; kws...)
    _is_extension_loaded(Val(:SymbolicUtils)) ||
        error("Please load the `SymbolicUtils` package to use `node_to_symbolic`.")
    return _node_to_symbolic(args...; kws...)
end
function _node_to_symbolic end

function symbolic_to_node(args...; kws...)
    _is_extension_loaded(Val(:SymbolicUtils)) ||
        error("Please load the `SymbolicUtils` package to use `symbolic_to_node`.")
    return _symbolic_to_node(args...; kws...)
end
function _symbolic_to_node end

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
    _is_extension_loaded(Val(:Zygote)) || error("Please load the Zygote.jl package.")
    return _zygote_gradient_impl(args...)
end
function _zygote_gradient_impl end

function bumper_eval_tree_array(args...)
    _is_extension_loaded(Val(:Bumper)) ||
        error("Please load the Bumper.jl package to use this feature.")
    return _bumper_eval_tree_array(args...)
end
function _bumper_eval_tree_array end
function bumper_kern! end

_is_loopvectorization_loaded(_) = false  # COV_EXCL_LINE

end
