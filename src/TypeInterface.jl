module TypeInterfaceModule

# Must implement:

# Is a value of your type valid?
is_valid(x::T) where {T} = true #=::Bool=#
is_valid_array(x::AbstractArray{T}) where {T} = all(is_valid, x) #=::Bool=#

# Obtain the underlying number type
function get_number_type(::Type{T}) where {T} #=::DataType=#
    error("Base number type of your custom type is not defined")
    return T
end

"""
    append_number_constants!(nvals, idx, value)

Fill all constants stored in your `value` to `nvals`, starting from `idx`. Return the index for the next `append_number_constants!` to use
"""
function append_number_constants!(
    nvals::AbstractVector{BT}, idx::Int64, value::T
) where {BT<:Number,T}
    return idx
end

"""
    pop_number_constants(nvals, idx, value)

The inverse of `append_number_constants!`. Given an array `nvals` containing data,
fill `value` with `nvals` starting from `idx`.

Returns a tuple of the next index to read from, and the filled-in value.

!!! note
    In the case of a scalar `value`, this will simply return `(idx, value)` without affecting `nvals`.
"""
function pop_number_constants(nvals::AbstractVector{BT}, idx::Int64, value::T) where {T,BT<:Number}
    return (idx, value)
end

"""
Count how many scalar constants `value` has, for use in `append_number_constants!` and `pop_number_constants`.

Note that this will return 0 for a scalar input.
"""
count_number_constants(value::T) where {T} = 0

# Implementations for numbers:

get_number_type(::Type{W}) where {W<:Number} = W
function append_number_constants!(
    nvals::AbstractVector{BT}, idx::Int64, value::T
) where {BT<:Number,T<:Number}
    nvals[idx] = value
    return idx + 1
end
function pop_number_constants(
    nvals::AbstractVector{BT}, idx::Int64, value::T
) where {T<:Number,BT<:Number}
    return idx + 1, nvals[idx]
end
@inline count_number_constants(::T) where {T<:Number} = 1
@inline is_valid(x::T) where {T<:Number} = isfinite(x) && !isnan(x)
@inline is_valid_array(x::AbstractArray{T}) where {T<:Number} = is_valid(sum(x)) # Fastest way to check for NaN in an array.

end
