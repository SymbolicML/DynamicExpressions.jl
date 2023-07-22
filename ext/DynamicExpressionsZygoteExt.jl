module DynamicExpressionsZygoteExt

import Zygote: gradient
import DynamicExpressions.EvaluateEquationDerivativeModule: _zygote_gradient

_zygote_gradient(op::F, ::Val{1}) where {F} = x -> gradient(op, x)[1]
_zygote_gradient(op::F, ::Val{2}) where {F} = (x, y) -> gradient(op, x, y)

end
