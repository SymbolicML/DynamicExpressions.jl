module OperatorEnumModule

abstract type AbstractOperatorEnum end

"""
    OperatorEnum{T}

Defines an enum over operators, along with their derivatives.
`.ops` is a tuple of operators, where `.ops[1]` is the unary
operators and `.ops[2]` is the binary operators, and so on.
"""
struct OperatorEnum{T<:Tuple} <: AbstractOperatorEnum
    ops::T
end

"""
    GenericOperatorEnum

Defines an enum over operators, along with their derivatives.
This is equivalent to [`OperatorEnum`](@ref), but dispatches
to generic evaluation for non-numeric types.
"""
struct GenericOperatorEnum{T<:Tuple} <: AbstractOperatorEnum
    ops::T
end

function Base.getproperty(op::AbstractOperatorEnum, name::Symbol)
    if name == :unaops
        return getfield(op, :ops)[1]
    elseif name == :binops
        return getfield(op, :ops)[2]
    else
        return getfield(op, name)
    end
end

Base.copy(op::AbstractOperatorEnum) = op
# TODO: Is this safe? What if a vector is passed here?

end
