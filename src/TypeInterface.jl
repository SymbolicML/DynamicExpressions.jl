module TypeInterfaceModule

function is_valid(x::T)::Bool where {T}
    return true
end
function is_valid_array(x::AbstractArray{T})::Bool where {T}
    return all(is_valid, x)
end

is_valid(x::T) where {T<:Number} = isfinite(x) && !isnan(x)

# Fastest way to check for NaN in an array.
# (due to optimizations in sum())
is_valid_array(x::AbstractArray{T}) where {T<:Number} = is_valid(sum(x))

end
