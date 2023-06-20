module SelfContainedEquation

import ..OperatorEnumModule: AbstractOperatorEnum
import ..EquationModule: Node

struct SelfContainedNode{T,OP<:AbstractOperatorEnum}
    tree::Node{T,OP}
    operators::OP

    function SelfContainedNode(
        tree::N, operators::O
    ) where {T,N<:Node{T},O<:AbstractOperatorEnum}
        return new{T,O}(tree, operators)
    end
end

end