"""Useful functions to be used throughout the library."""
module UtilsModule

using LoopVectorization: @turbo

"""Remove all type assertions in an expression."""
function _remove_type_assertions(ex::Expr)
    if ex.head == :(::)
        @assert length(ex.args) == 2
        return ex.args[1]
    else
        return Expr(ex.head, map(_remove_type_assertions, ex.args)...)
    end
end

function _remove_type_assertions(ex)
    return ex
end

"""
    @maybe_turbo use_turbo expression

Use @turbo if flag is true; otherwise @inbounds @simd.
This will also remove all type assertions from the expression.
"""
macro maybe_turbo(turboflag, ex)
    # Thanks @jlapeyre https://discourse.julialang.org/t/optional-macro-invocation/18588
    clean_ex = _remove_type_assertions(ex)
    turbo_ex = Expr(:macrocall, Symbol("@turbo"), LineNumberNode(@__LINE__), clean_ex)
    simple_ex = Expr(
        :macrocall,
        Symbol("@inbounds"),
        LineNumberNode(@__LINE__),
        Expr(:macrocall, Symbol("@simd"), LineNumberNode(@__LINE__), ex),
    )
    quote
        if $(esc(turboflag))
            $(esc(turbo_ex))
        else
            $(esc(simple_ex))
        end
    end
end

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

const max_ops = 1024
const vals = ntuple(i -> Val(i), max_ops)

end
