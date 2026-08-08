module DynamicExpressionsZygoteExt

using Zygote: gradient
import DynamicExpressions.ExtensionInterfaceModule:
    is_extension_loaded, _zygote_gradient_impl, ZygoteGradient

function _zygote_gradient_impl(op::F, ::Val{degree}) where {F,degree}
    return ZygoteGradient{F,degree}(op)
end

# All this does is remove `nothing`, so that we get type stability
function (g::ZygoteGradient{F,degree})(args::Vararg{Any,degree}) where {F,degree}
    partials = gradient(g.op, args...)
    return ntuple(i -> @something(partials[i], zero(args[i])), Val(degree))
end

is_extension_loaded(::Val{:Zygote}) = true

end
