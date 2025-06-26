module OperatorEnumModule

using DispatchDoctor: @unstable

abstract type AbstractOperatorEnum end

"""
    OperatorEnum(ops::Tuple{Vararg{Tuple}})

# Fields

- `ops`: A tuple of tuples of operators. Index `i` corresponds to operators with `i` arguments.
"""
struct OperatorEnum{OPS<:Tuple{Vararg{Tuple}}} <: AbstractOperatorEnum
    ops::OPS
end

"""
    GenericOperatorEnum(ops::Tuple{Vararg{Tuple}})

Defines an enum over operators for non-scalar data types (vectors, matrices, strings, custom types).

# Fields

- `ops`: A tuple of operators, with index `i` corresponding to the operator tuple for a node of degree `i`.
"""
struct GenericOperatorEnum{OPS<:Tuple{Vararg{Tuple}}} <: AbstractOperatorEnum
    ops::OPS
end

Base.copy(op::AbstractOperatorEnum) = op
# TODO: Is this safe? What if a vector is passed here?

@unstable @inline function Base.getindex(op::AbstractOperatorEnum, i::Int)
    return getfield(op, :ops)[i]
end
@inline function Base.length(op::AbstractOperatorEnum)
    return length(getfield(op, :ops))
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
