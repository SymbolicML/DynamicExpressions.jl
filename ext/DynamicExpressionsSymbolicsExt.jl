module DynamicExpressionsSymbolicsExt

using Symbolics: Num, value

import DynamicExpressions.ExtensionInterfaceModule: _symbolic_to_node

_symbolic_to_node(expr::Num, args...; kws...) = _symbolic_to_node(value(expr), args...; kws...)

end
