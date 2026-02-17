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

function (g::ZygoteGradient{F,1,1})(x) where {F}
    out = only(gradient(g.op, x))
    return out === nothing ? zero(x) : out
end
function (g::ZygoteGradient{F,2,nothing})(x, y) where {F}
    (∂x, ∂y) = gradient(g.op, x, y)
    return (∂x === nothing ? zero(x) : ∂x, ∂y === nothing ? zero(y) : ∂y)
end
function (g::ZygoteGradient{F,2,1})(x, y) where {F}
    ∂x = only(gradient(Base.Fix2(g.op, y), x))
    return ∂x === nothing ? zero(x) : ∂x
end
function (g::ZygoteGradient{F,2,2})(x, y) where {F}
    ∂y = only(gradient(Base.Fix1(g.op, x), y))
    return ∂y === nothing ? zero(y) : ∂y
end

end
