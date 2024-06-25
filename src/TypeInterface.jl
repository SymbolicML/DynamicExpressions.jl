module TypeInterfaceModule

function is_valid(x::T)::Bool where {T} end
function is_valid_array(x::AbstractArray{T})::Bool where {T} end

is_valid(x::T) where {T <: Number} = isfinite(x) && !isnan(x)
is_valid(x::T) where {T} = true

# Fastest way to check for NaN in an array.
# (due to optimizations in sum())
is_valid_array(x::AbstractArray{T}) where {T <: Number} = is_valid(sum(x))
is_valid_array(x::AbstractArray{T}) where {T} = all(is_valid, x)

end