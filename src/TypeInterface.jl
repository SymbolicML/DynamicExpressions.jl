module TypeInterfaceModule

# Must implement:

# Is a value of your type valid? (for OperatorEnum)

function is_valid(x::T)::Bool where {T}
    return true
end
function is_valid_array(x::AbstractArray{T})::Bool where {T}
    return all(is_valid, x)
end

# For Optimisation, obtaining the number constants:

function get_number_type(::Type{T}) where {T}
    return error("Base number type of your custom type is not defined")
end
get_number_type(::Type{W}) where {W<:Number} = W

# Given an array of numbers, append all number constants stored in your type there
function append_number_constants!(nvals::AbstractVector{BT}, value::T) where {BT<:Number,T} end
# Given an array of numbers, get all the number constants starting at a given index and store them in your type.
function pop_number_constants(
    nvals::AbstractVector{BT}, value::T, idx::Int64
)::Tuple{T,idx} where {T,BT<:Number} end
function count_number_constants(value::T)::Int64 where {T}
    return 0
end

# Implementations

function append_number_constants!(
    nvals::AbstractVector{BT}, value::T
) where {BT<:Number,T<:Number}
    push!(nvals, value)
    return nothing
end
function pop_number_constants(
    nvals::AbstractVector{BT}, value::T, idx::Int64
)::Tuple{T,Int64} where {T<:Number,BT<:Number}
    return nvals[idx], idx + 1
end
@inline count_number_constants(::T) where {T<:Number} = 1

@inline is_valid(x::T) where {T<:Number} = isfinite(x) && !isnan(x)

# Fastest way to check for NaN in an array.
# (due to optimizations in sum())
@inline is_valid_array(x::AbstractArray{T}) where {T<:Number} = is_valid(sum(x))

end
