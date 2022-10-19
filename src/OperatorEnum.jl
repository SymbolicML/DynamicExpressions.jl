module OperatorEnumModule

abstract type AbstractOperatorEnum end

"""
    OperatorEnum

Defines an enum over operators, along with their derivatives.
# Fields
- `binops`: A tuple of binary operators. Real scalar input type.
- `unaops`: A tuple of unary operators. Real scalar input type.
- `diff_binops`: A tuple of Zygote-computed derivatives of the binary operators.
- `diff_unaops`: A tuple of Zygote-computed derivatives of the unary operators.
"""
struct OperatorEnum{A<:Tuple,B<:Tuple,dA<:Union{Tuple,Nothing},dB<:Union{Tuple,Nothing}} <:
       AbstractOperatorEnum
    binops::A
    unaops::B
    diff_binops::dA
    diff_unaops::dB
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
struct GenericOperatorEnum{A<:Tuple,B<:Tuple} <: AbstractOperatorEnum
    binops::A
    unaops::B
end

end
