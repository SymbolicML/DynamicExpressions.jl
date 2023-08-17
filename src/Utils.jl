"""Useful functions to be used throughout the library."""
module UtilsModule

using LoopVectorization: @turbo
using MacroTools: postwalk, @capture, splitdef, combinedef

"""Remove all type assertions in an expression."""
function _remove_type_assertions(ex::Expr)
    if ex.head == :(::)
        @assert length(ex.args) == 2
        return _remove_type_assertions(ex.args[1])
    else
        return Expr(ex.head, map(_remove_type_assertions, ex.args)...)
    end
end
_remove_type_assertions(ex) = ex

"""Replace instances of (isfinite(x) ? op(x) : T(Inf)) with op(x)"""
function _remove_isfinite(ex::Expr)
    if (
        ex.head == :if &&
        length(ex.args) == 3 &&
        ex.args[1].head == :call &&
        ex.args[1].args[1] == :isfinite
    )
        return _remove_isfinite(ex.args[2])
    else
        return Expr(ex.head, map(_remove_isfinite, ex.args)...)
    end
end
_remove_isfinite(ex) = ex

"""
    @maybe_turbo use_turbo expression

Use @turbo if flag is true; otherwise @inbounds @simd.
This will also remove all type assertions from the expression.
"""
macro maybe_turbo(turboflag, ex)
    # Thanks @jlapeyre https://discourse.julialang.org/t/optional-macro-invocation/18588
    clean_ex = _remove_type_assertions(ex)
    clean_ex = _remove_isfinite(clean_ex)
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
is_bad_array(array) = !(isempty(array) || isfinite(sum(array)))
isgood(x::T) where {T<:Number} = !(isnan(x) || !isfinite(x))
isgood(x) = true
isbad(x) = !isgood(x)

"""
    @memoize_on tree function my_function_on_tree(tree::Node)
        ...
    end

This macro takes a function definition and creates a second version of the
function with an additional `id_map` argument. When passed this argument (an
IdDict()), it will use use the `id_map` to avoid recomputing the same value
for the same node in a tree. Use this to automatically create functions that
work with trees that have shared child nodes.
"""
macro memoize_on(tree, def)
    idmap_def = _memoize_on(tree, def)
    return quote
        $(esc(def)) # The normal function
        $(esc(idmap_def)) # The function with an id_map argument
    end
end
function _memoize_on(tree::Symbol, def::Expr)
    sdef = splitdef(def)

    # Add an id_map argument
    push!(sdef[:args], :(id_map::IdDict))

    f_name = sdef[:name]

    # Add id_map argument to all calls within the function:
    sdef[:body] = postwalk(sdef[:body]) do ex
        if @capture(ex, f_(args__))
            if f == f_name
                return Expr(:call, f, args..., :id_map)
            end
        end
        return ex
    end

    # Wrap the function body in a get!(id_map, tree) do ... end block:
    sdef[:body] = quote
        get!(id_map, $(tree)) do
            $(sdef[:body])
        end
    end

    return combinedef(sdef)
end

"""
    @with_memoize(call, id_map)

This simple macro simply puts the `id_map`
into the call, to be consistent with the `@memoize_on` macro.

```
@with_memoize(_copy_node(tree), IdDict{Any,Any}())
````

is converted to 

```
_copy_node(tree, IdDict{Any,Any}())
```

"""
macro with_memoize(def, id_map)
    idmap_def = _add_idmap_to_call(def, id_map)
    return quote
        $(esc(idmap_def))
    end
end

function _add_idmap_to_call(def::Expr, id_map::Expr)
    @assert def.head == :call
    return Expr(:call, def.args[1], def.args[2:end]..., id_map)
end

@inline function fill_similar(value, array, args...)
    out_array = similar(array, args...)
    out_array .= value
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

end
