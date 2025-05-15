module DynamicExpressionsZygoteExt

using Zygote: gradient
import DynamicExpressions.ExtensionInterfaceModule: _zygote_gradient, ZygoteGradient

function _zygote_gradient(op::F, ::Val{degree}) where {F,degree}
    return ZygoteGradient{F,degree}(op)
end

# All this does is remove `nothing`, so that we get type stability
function (g::ZygoteGradient{F,degree})(args::Vararg{Any,degree}) where {F,degree}
    partials = gradient(g.op, args...)
    return ntuple(i -> @something(partials[i], zero(args[i])), Val(degree))
end

end
