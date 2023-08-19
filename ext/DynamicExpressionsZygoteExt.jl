module DynamicExpressionsZygoteExt

import Zygote: gradient
import DynamicExpressions.EvaluateEquationDerivativeModule: _zygote_gradient

function _zygote_gradient(op::F, ::Val{1}, ::Val{turbo}) where {F,turbo}
    if turbo
        return x -> gradient(op, x)[1]
    else
        function (x)
            out = gradient(op, x)[1]
            return out === nothing ? zero(x) : out
        end
    end
end
function _zygote_gradient(op::F, ::Val{2}, ::Val{turbo}) where {F,turbo}
    if turbo
        return (x, y) -> gradient(op, x, y)
    else
        return function (x, y)
            (∂x, ∂y) = gradient(op, x, y)
            return (∂x === nothing ? zero(x) : ∂x, ∂y === nothing ? zero(y) : ∂y)
        end
    end
end

end
