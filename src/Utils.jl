"""Useful functions to be used throughout the library."""
module UtilsModule

using MacroTools: postwalk, @capture, splitdef, combinedef
using DispatchDoctor: @unstable

# Returns two arrays
macro return_on_false2(flag, retval, retval2)
    :(
        if !$(esc(flag))
            return ($(esc(retval)), $(esc(retval2)), false)
        end
    )
end

@inline function fill_similar(value::T, array, args...) where {T}
    out_array = similar(array, args...)
    fill!(out_array, value)
    return out_array
end

function deprecate_varmap(variable_names, varMap, func_name)
    if varMap !== nothing
        Base.depwarn("`varMap` is deprecated; use `variable_names` instead", func_name)
        @assert variable_names === nothing "Cannot pass both `varMap` and `variable_names`"
        variable_names = varMap
    end
    return variable_names
end

@unstable counttuple(::Type{<:NTuple{N,Any}}) where {N} = N

"""
    Undefined

Just a type like `Nothing` to differentiate from a literal `Nothing`.
"""
struct Undefined end

"""
    ResultOk{A}

Stores the result of an evaluation and whether
any errors occured during the evaluation. This
is used to quit early, so that we do not pass
`Inf` to `cos` (for example).
"""
struct ResultOk{A}
    x::A
    ok::Bool
end

struct ResultOk2{A<:AbstractArray,B<:AbstractArray}
    x::A
    dx::B
    ok::Bool
end

end
