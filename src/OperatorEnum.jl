module OperatorEnumModule

abstract type AbstractOperatorEnum end

"""
    OperatorEnum

Defines an enum over operators, along with their derivatives.
# Fields
- `binops`: A tuple of binary operators. Scalar input type.
- `unaops`: A tuple of unary operators. Scalar input type.
- `diff_binops`: A tuple of Zygote-computed derivatives of the binary operators.
- `diff_unaops`: A tuple of Zygote-computed derivatives of the unary operators.
"""
struct OperatorEnum{B,U,dB,dU} <: AbstractOperatorEnum
    binops::B
    unaops::U
    diff_binops::dB
    diff_unaops::dU
end

"""
    OperatorEnum

Defines an enum over operators, along with their derivatives.
# Fields
- `binops`: A tuple of binary operators.
- `unaops`: A tuple of unary operators.
- `diff_binops`: A tuple of Zygote-computed derivatives of the binary operators.
- `diff_unaops`: A tuple of Zygote-computed derivatives of the unary operators.
"""
struct GenericOperatorEnum{B,U} <: AbstractOperatorEnum
    binops::B
    unaops::U
end

end
