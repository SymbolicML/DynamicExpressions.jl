module TypeInterfaceModule

# Must implement:

# Is a value of your type valid?
is_valid(x::T) where {T} = true #=::Bool=#
@inline is_valid(x::T) where {T<:Number} = isfinite(x) && !isnan(x)

# Is an array of values of your type valid?
is_valid_array(x::AbstractArray{T}) where {T} = all(is_valid, x) #=::Bool=#
@inline is_valid_array(x::AbstractArray{T}) where {T<:Number} = is_valid(sum(x)) # Fastest way to check for NaN in an array.

# Obtain the underlying number type
function get_number_type(::Type{T}) where {T}
    return error("Base number type of your custom type is not defined")
end  #=::DataType=#
get_number_type(::Type{W}) where {W<:Number} = W

"""
    pack_scalar_constants!(nvals, idx, value)

Fill all constants stored in your `value` to `nvals`, starting from `idx`. Return the index for the next `pack_scalar_constants!` to use
"""
function pack_scalar_constants!(
    nvals::AbstractVector{BT}, idx::Int64, value::T
) where {BT<:Number,T<:Number} #= ::Int64 =#
    nvals[idx] = value
    return idx + 1
end

"""
    unpack_scalar_constants(nvals, idx, value)

The inverse of `pack_scalar_constants!`. Given an array `nvals` containing data,
fill `value` with `nvals` starting from `idx`.

Returns a tuple of the next index to read from, and the filled-in value.

!!! note
    In the case of a `Number` `value`, this will increment the index by 1 and return the current value.
"""
function unpack_scalar_constants(
    nvals::AbstractVector{BT}, idx::Int64, value::T
) where {T,BT<:Number} #= ::Tuple{Int64, T}  =#
    return (idx + 1, convert(T, nvals[idx]))
end

"""
Count how many scalar constants `value` has, for use in `pack_scalar_constants!` and `unpack_scalar_constants`.

Note that this will return 1 for scalars.
"""
@inline count_scalar_constants(::T) where {T<:Number} = 1

end
