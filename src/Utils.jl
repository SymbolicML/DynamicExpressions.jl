"""Useful functions to be used throughout the library."""
module UtilsModule

macro return_on_false(flag, retval)
    :(
        if !$(esc(flag))
            return ($(esc(retval)), false)
        end
    )
end

# Returns two arrays
macro return_on_false2(flag, retval, retval2)
    :(
        if !$(esc(flag))
            return ($(esc(retval)), $(esc(retval2)), false)
        end
    )
end

# Fastest way to check for NaN in an array.
# (due to optimizations in sum())
is_bad_array(array) = !isfinite(sum(array))
isgood(x::T) where {T<:Number} = !(isnan(x) || !isfinite(x))
isgood(x) = true
isbad(x) = !isgood(x)

end
