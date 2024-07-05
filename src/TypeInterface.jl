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

# Given an array of numbers, append all number constants stored in your type at a given index
function append_number_constants!(
    nvals::AbstractVector{BT}, idx::Int64, value::T
) where {BT<:Number,T}
    return idx
end #=::Int64=#

# Given an array of numbers, get all the number constants starting at a given index and store them in your type.
function #=::Tuple{Int64,T}=#
pop_number_constants(nvals::AbstractVector{BT}, idx::Int64, value::T) where {T,BT<:Number}
    return (idx, value)
end #=::Tuple{Int64,T}=#

# Count how many number constants your value has
count_number_constants(value::T) where {T} = 0 #=::Int64=#

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
