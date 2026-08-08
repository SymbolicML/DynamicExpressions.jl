module ExtensionInterfaceModule

using DispatchDoctor: @unstable

is_extension_loaded(::Val) = false

"""
    node_to_symbolic(tree::AbstractExpressionNode, operators::AbstractOperatorEnum;
                variable_names::Union{AbstractVector{<:AbstractString}, Nothing}=nothing,
                index_functions::Bool=false)

The interface to SymbolicUtils.jl. Passing a tree to this function
will generate a symbolic equation in SymbolicUtils.jl format.

## Arguments

- `tree::AbstractExpressionNode`: The equation to convert.
- `operators::AbstractOperatorEnum`: OperatorEnum, which contains the operators used in the equation.
- `variable_names::Union{AbstractVector{<:AbstractString}, Nothing}=nothing`: What variable names to use for
    each feature. Default is [x1, x2, x3, ...].
- `index_functions::Bool=false`: Whether to generate special names for the
    operators, which then allows one to convert back to a `AbstractExpressionNode` format
    using `symbolic_to_node`.
"""
@unstable function node_to_symbolic(args...; kws...)
    is_extension_loaded(Val(:SymbolicUtils)) ||
        error("Please load the `SymbolicUtils` package to use `node_to_symbolic`.")
    return _node_to_symbolic(args...; kws...)
end
function _node_to_symbolic end

function symbolic_to_node(args...; kws...)
    is_extension_loaded(Val(:SymbolicUtils)) ||
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
    is_extension_loaded(Val(:Zygote)) || error("Please load the Zygote.jl package.")
    return _zygote_gradient_impl(args...)
end
function _zygote_gradient_impl end

function bumper_eval_tree_array(args...)
    is_extension_loaded(Val(:Bumper)) ||
        error("Please load the Bumper.jl package to use this feature.")
    return _bumper_eval_tree_array(args...)
end
function _bumper_eval_tree_array end
function bumper_kern! end

_is_loopvectorization_loaded(_) = false  # COV_EXCL_LINE

end
