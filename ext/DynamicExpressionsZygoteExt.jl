module DynamicExpressionsZygoteExt

using Zygote: gradient
import DynamicExpressions.ExtensionInterfaceModule: _zygote_gradient, ZygoteGradient

function _zygote_gradient(op::F, ::Val{1}) where {F}
    return ZygoteGradient{F,1,1}(op)
end
function _zygote_gradient(op::F, ::Val{2}, ::Val{side}=Val(nothing)) where {F,side}
    # side should be either nothing (for both), 1, or 2
    @assert side === nothing || side in (1, 2)
    return ZygoteGradient{F,2,side}(op)
end

end
