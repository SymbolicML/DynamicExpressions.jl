module OperatorEnumModule

abstract type AbstractOperatorEnum end

"""
    OperatorEnum

Defines an enum over operators, along with their derivatives.
# Fields
- `binops`: A tuple of binary operators. Scalar input type.
- `unaops`: A tuple of unary operators. Scalar input type.
"""
struct OperatorEnum{B,U} <: AbstractOperatorEnum
    binops::B
    unaops::U
end

"""
    GenericOperatorEnum

Defines an enum over operators, along with their derivatives.
# Fields
- `binops`: A tuple of binary operators.
- `unaops`: A tuple of unary operators.
"""
struct GenericOperatorEnum{B,U} <: AbstractOperatorEnum
    binops::B
    unaops::U
end

Base.copy(op::AbstractOperatorEnum) = op
# TODO: Is this safe? What if a vector is passed here?

end
