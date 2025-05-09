module OperatorEnumModule

using DispatchDoctor: @unstable

abstract type AbstractOperatorEnum end

"""
    OperatorEnum

Defines an enum over operators, along with their derivatives.

# Fields
- `ops`: A tuple of operators, with index `i` corresponding to the operator tuple for a node of degree `i`.
"""
struct OperatorEnum{OPS<:Tuple{Vararg{Tuple}}} <: AbstractOperatorEnum
    ops::OPS
end

function OperatorEnum(binary_operators::Tuple, unary_operators::Tuple)
    return OperatorEnum((unary_operators, binary_operators))
end

"""
    GenericOperatorEnum

Defines an enum over operators, along with their derivatives.

# Fields
- `ops`: A tuple of operators, with index `i` corresponding to the operator tuple for a node of degree `i`.
"""
struct GenericOperatorEnum{OPS<:Tuple{Vararg{Tuple}}} <: AbstractOperatorEnum
    ops::OPS
end

function GenericOperatorEnum(binops::Tuple, unaops::Tuple)
    return GenericOperatorEnum((unaops, binops))
end

Base.copy(op::AbstractOperatorEnum) = op
# TODO: Is this safe? What if a vector is passed here?

@unstable @inline function Base.getindex(op::AbstractOperatorEnum, i::Int)
    return getfield(op, :ops)[i]
end
@inline function Base.getproperty(op::AbstractOperatorEnum, k::Symbol)
    if k == :unaops
        return getfield(op, :ops)[1]
    elseif k == :binops
        ops = getfield(op, :ops)
        return length(ops) > 1 ? ops[2] : ()
    else
        return getfield(op, k)
    end
end

end
