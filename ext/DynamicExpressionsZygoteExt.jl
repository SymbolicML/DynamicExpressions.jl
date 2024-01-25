module DynamicExpressionsZygoteExt

import Zygote: gradient
import DynamicExpressions.ExtensionInterfaceModule: _zygote_gradient

function _zygote_gradient(op::F, ::Val{1}) where {F}
    function (x)
        out = gradient(op, x)[1]
        return out === nothing ? zero(x) : out
    end
end
function _zygote_gradient(op::F, ::Val{2}) where {F}
    function (x, y)
        (∂x, ∂y) = gradient(op, x, y)
        return (∂x === nothing ? zero(x) : ∂x, ∂y === nothing ? zero(y) : ∂y)
    end
end

end
