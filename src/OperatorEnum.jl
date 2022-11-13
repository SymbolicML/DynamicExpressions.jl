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
struct OperatorEnum <: AbstractOperatorEnum
    binops::Vector{Function}
    unaops::Vector{Function}
    diff_binops::Vector{Function}
    diff_unaops::Vector{Function}
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
struct GenericOperatorEnum <: AbstractOperatorEnum
    binops::Vector{Function}
    unaops::Vector{Function}
end

end
