module ValueInterfaceModule

using Interfaces: Interfaces, @interface, @implements, Arguments

is_valid(x::T) where {T} = true
is_valid(x::T) where {T<:Number} = isfinite(x) && !isnan(x)

is_valid_array(x::AbstractArray{T}) where {T} = all(is_valid, x)
is_valid_array(x::AbstractArray{T}) where {T<:Number} = is_valid(sum(x))

get_number_type(t::Type) = error("Base number type of type $(t) is not defined")
get_number_type(::Type{T}) where {T<:Number} = T

"""
    pack_scalar_constants!(nvals, idx, value)

Fill all constants stored in your `value` to `nvals`, starting from `idx`.
Return the index for the next `pack_scalar_constants!` to use
"""
function pack_scalar_constants!(nvals::AbstractVector{<:Number}, idx::Int64, value::Number)
    nvals[idx] = value
    return idx + 1
end

"""
    unpack_scalar_constants(nvals, idx, value)

The inverse of `pack_scalar_constants!`. Given an array `nvals` containing data,
fill `value` with `nvals` starting from `idx`.

Returns a tuple of the next index to read from, and the filled-in value.

!!! note
    In the case of a `Number` `value`, this will simply increment the index by 1 and return the current value.
"""
function unpack_scalar_constants(
    nvals::AbstractVector{<:Number}, idx::Int64, value::T
) where {T}
    return (idx + 1, convert(T, nvals[idx]))
end

"""
    count_scalar_constants(value)

Count how many scalar constants `value` has, for use in `pack_scalar_constants!` and `unpack_scalar_constants`.

Note that this will return 1 for scalars.
"""
@inline count_scalar_constants(::T) where {T<:Number} = 1

################################################################################
# Interface.jl integration #####################################################
################################################################################

function _check_is_valid(x)
    return is_valid(x) isa Bool
end
function _check_is_valid_array(x)
    return is_valid_array([x]) isa Bool && is_valid_array([x]) == is_valid(x)
end
function _check_get_number_type(x)
    try
        get_number_type(typeof(x)) <: Number
    catch e
        @error e
        return false
    end
end
function _check_pack_scalar_constants!(x)
    packed_x = Vector{get_number_type(typeof(x))}(undef, count_scalar_constants(x))
    new_idx = pack_scalar_constants!(packed_x, 1, x)
    return new_idx == 1 + count_scalar_constants(x)
end
function _check_unpack_scalar_constants(x)
    packed_x = Vector{get_number_type(typeof(x))}(undef, count_scalar_constants(x))
    pack_scalar_constants!(packed_x, 1, x)
    new_idx, x2 = unpack_scalar_constants(packed_x, 1, x)
    return new_idx == 1 + count_scalar_constants(x) && x2 == x
end
function _check_count_scalar_constants(x)
    return count_scalar_constants(x) isa Int &&
           (!(x isa Number) || count_scalar_constants(x) == 1)
end

#! format: off
vi_components = (
    mandatory = (
        is_valid = "checks if the value is valid (no NaN/Inf)" => _check_is_valid,
        is_valid_array = "checks if each value of the array is valid" => _check_is_valid_array,
        get_number_type = "gets the underlying scalar type, which must be `<:Number`" => _check_get_number_type,
        pack_scalar_constants! = "packs scalar constants into an array" => _check_pack_scalar_constants!,
        unpack_scalar_constants = "unpacks scalar constants from an array" => _check_unpack_scalar_constants,
        count_scalar_constants = "counts how many scalar constants the value has" => _check_count_scalar_constants,
    ),
    optional = (;)
)
vi_description = (
    "Defines the interface for types operated on by `DynamicExpressions`. " *
    "Any `Number` has this interface, but for other types, you will need to " *
    "define it."
)
#! format: on

@interface(ValueInterface, Any, vi_components, vi_description)
@implements(ValueInterface, Number, [Arguments()])

end
