###############################
#  DynamicExpressionsMooncakeExt.jl
#
#  Custom tangent implementation for
#  DynamicExpressions.AbstractExpressionNode and its two
#  concrete tree types `Node` and `GraphNode` (as defined in
#  DynamicExpressions.jl ≥ v0.6).
#
#  The design follows the recursive‑type recipe from the
#  Mooncake manual.  Every expression node gets a mirror
#  `TangentExprNode` tree containing the tangents of the
#  numeric payloads (`val`) while preserving left/right
#  topology.  Mooncake’s forward data is the tangent object
#  itself; reverse data is `NoRData()` because all fields are
#  mutated in‑place.
###############################

module DynamicExpressionsMooncakeExt

using DynamicExpressions
using Mooncake
using Random: AbstractRNG

const _AEN = DynamicExpressions.AbstractExpressionNode

################################################################################
# 1. Recursive tangent type
################################################################################

mutable struct TangentExprNode{Tv}
    val::Tv
    l::Union{TangentExprNode{Tv},Mooncake.NoTangent}
    r::Union{TangentExprNode{Tv},Mooncake.NoTangent}

    # leaf‑only constructor
    function TangentExprNode{Tv}(val_tan::Tv) where {Tv}
        return new{Tv}(val_tan, Mooncake.NoTangent(), Mooncake.NoTangent())
    end

    # full constructor (used by recursion)
    function TangentExprNode{Tv}(
        val_tan::Tv,
        l_tan::Union{TangentExprNode{Tv},Mooncake.NoTangent},
        r_tan::Union{TangentExprNode{Tv},Mooncake.NoTangent},
    ) where {Tv}
        return new{Tv}(val_tan, l_tan, r_tan)
    end

    # required tuple‑style constructor for Mooncake’s fallback logic
    function TangentExprNode{Tv}(
        nt::@NamedTuple{
            val::Tv,
            l::Union{Mooncake.NoTangent,TangentExprNode{Tv}},
            r::Union{Mooncake.NoTangent,TangentExprNode{Tv}},
        }
    ) where {Tv}
        return new{Tv}(nt.val, nt.l, nt.r)
    end

    # fallback constructor: accept any NamedTuple with at least a :val key.
    function TangentExprNode{Tv}(nt::NamedTuple) where {Tv}
        @assert haskey(nt, :val) "TangentExprNode requires a :val field in NamedTuple"
        val_tan = nt.val
        l_tan = haskey(nt, :l) ? nt.l : Mooncake.NoTangent()
        r_tan = haskey(nt, :r) ? nt.r : Mooncake.NoTangent()
        return TangentExprNode{Tv}(val_tan, l_tan, r_tan)
    end
end

################################################################################
# 2. Associate tangent type with every expression node subtype
################################################################################

function Mooncake.tangent_type(::Type{N}) where {T,N<:_AEN{T}}
    Tv = Mooncake.tangent_type(T)
    return Tv === Mooncake.NoTangent ? Mooncake.NoTangent : TangentExprNode{Tv}
end

################################################################################
# 3. Forward / reverse data split
################################################################################

Mooncake.rdata(::TangentExprNode) = Mooncake.NoRData()
Mooncake.tangent(t::TangentExprNode, ::Mooncake.NoRData) = t  # identity

# 3.1  Tell Mooncake how to combine (fdata, rdata) types where the
#       fdata *is itself* a TangentExprNode and rdata is NoRData.
#       Without this, Mooncake’s generic reconstruction logic errors.
function Mooncake.tangent_type(::Type{TangentExprNode{Tv}}) where {Tv}
    return TangentExprNode{Tv}
end
function Mooncake.tangent_type(
    ::Type{TangentExprNode{Tv}}, ::Type{Mooncake.NoRData}
) where {Tv}
    return TangentExprNode{Tv}
end

################################################################################
# 4. Helper predicates and utilities
################################################################################

@inline _has_child(p, field::Symbol) = isdefined(p, field) && (getfield(p, field) isa _AEN)

################################################################################
# 5. zero_tangent / randn_tangent
################################################################################

function _gen_tangent(fn, args...)
    # helper to share the recursive implementation
    return fn(args...)
end

function Mooncake.zero_tangent_internal(
    p::N, dict::Mooncake.MaybeCache
) where {T,N<:_AEN{T}}
    Tv = Mooncake.tangent_type(T)
    Tv === Mooncake.NoTangent && return Mooncake.NoTangent()

    if haskey(dict, p)
        return dict[p]::TangentExprNode{Tv}
    end

    val_tan = Mooncake.zero_tangent_internal(p.val, dict)::Tv
    t = TangentExprNode{Tv}(val_tan)
    dict[p] = t
    if _has_child(p, :l)
        t.l = Mooncake.zero_tangent_internal(getfield(p, :l), dict)
    end
    if _has_child(p, :r)
        t.r = Mooncake.zero_tangent_internal(getfield(p, :r), dict)
    end
    return t
end

function Mooncake.randn_tangent_internal(
    rng::AbstractRNG, p::N, dict::Mooncake.MaybeCache
) where {T,N<:_AEN{T}}
    Tv = Mooncake.tangent_type(T)
    Tv === Mooncake.NoTangent && return Mooncake.NoTangent()

    if haskey(dict, p)
        return dict[p]::TangentExprNode{Tv}
    end

    val_tan = Mooncake.randn_tangent_internal(rng, p.val, dict)::Tv
    t = TangentExprNode{Tv}(val_tan)
    dict[p] = t
    if _has_child(p, :l)
        t.l = Mooncake.randn_tangent_internal(rng, getfield(p, :l), dict)
    end
    if _has_child(p, :r)
        t.r = Mooncake.randn_tangent_internal(rng, getfield(p, :r), dict)
    end
    return t
end

################################################################################
# 6. In‑place mutation helpers (increment!! / set_to_zero!!)
################################################################################

function Mooncake.increment_internal!!(
    c::Mooncake.IncCache, t::TangentExprNode{Tv}, s::TangentExprNode{Tv}
) where {Tv}
    (haskey(c, t) || t === s) && return t
    c[t] = true
    t.val = Mooncake.increment_internal!!(c, t.val, s.val)
    if !(t.l isa Mooncake.NoTangent)
        t.l = Mooncake.increment_internal!!(c, t.l, s.l)
    end
    if !(t.r isa Mooncake.NoTangent)
        t.r = Mooncake.increment_internal!!(c, t.r, s.r)
    end
    return t
end

function Mooncake.set_to_zero_internal!!(
    c::Mooncake.IncCache, t::TangentExprNode{Tv}
) where {Tv}
    haskey(c, t) && return t
    c[t] = false
    t.val = Mooncake.set_to_zero_internal!!(c, t.val)
    if !(t.l isa Mooncake.NoTangent)
        t.l = Mooncake.set_to_zero_internal!!(c, t.l)
    end
    if !(t.r isa Mooncake.NoTangent)
        t.r = Mooncake.set_to_zero_internal!!(c, t.r)
    end
    return t
end

################################################################################
# 7. Algebraic helpers (_dot / _scale)
################################################################################

function Mooncake._dot_internal(
    c::Mooncake.MaybeCache, t::TangentExprNode{Tv}, s::TangentExprNode{Tv}
) where {Tv}
    key = (t, s)
    haskey(c, key) && return c[key]::Float64
    c[key] = 0.0
    res = Mooncake._dot_internal(c, t.val, s.val)
    if !(t.l isa Mooncake.NoTangent)
        res += Mooncake._dot_internal(c, t.l, s.l)
    end
    if !(t.r isa Mooncake.NoTangent)
        res += Mooncake._dot_internal(c, t.r, s.r)
    end
    c[key] = res
    return res
end

function Mooncake._scale_internal(
    c::Mooncake.MaybeCache, a::Number, t::TangentExprNode{Tv}
) where {Tv}
    haskey(c, t) && return c[t]::TangentExprNode{Tv}
    val_new = Mooncake._scale_internal(c, a, t.val)
    l_new = if t.l isa Mooncake.NoTangent
        Mooncake.NoTangent()
    else
        Mooncake._scale_internal(c, a, t.l)
    end
    r_new = if t.r isa Mooncake.NoTangent
        Mooncake.NoTangent()
    else
        Mooncake._scale_internal(c, a, t.r)
    end
    t_new = TangentExprNode{Tv}(val_new, l_new, r_new)
    c[t] = t_new
    return t_new
end

################################################################################
# 8. add_to_primal_internal (produces a new primal node tree)
################################################################################

# Generic constructor helper that re‑uses the *existing* node type but with
# updated fields.  It allocates a fresh node and copies every field using
# reflection; this is slower than a hand‑coded version but guarantees we work
# for both `Node` and `GraphNode` without knowing their internals.
function _clone_with(p::P; val, l, r) where {P<:_AEN}
    newp = Base.deepcopy_internal(p)  # keep degree, constant, op, feature, etc.
    setfield!(newp, :val, val)
    if isdefined(p, :l)
        setfield!(newp, :l, l)
    end
    if isdefined(p, :r)
        setfield!(newp, :r, r)
    end
    return newp
end

function Mooncake._add_to_primal_internal(
    c::Mooncake.MaybeCache, p::N, t::TangentExprNode{Tv}, unsafe::Bool
) where {T,N<:_AEN{T},Tv}
    key = (p, t, unsafe)
    haskey(c, key) && return c[key]::N

    val_new = Mooncake._add_to_primal_internal(c, p.val, t.val, unsafe)
    l_new = if _has_child(p, :l)
        Mooncake._add_to_primal_internal(c, getfield(p, :l), t.l, unsafe)
    else
        nothing
    end
    r_new = if _has_child(p, :r)
        Mooncake._add_to_primal_internal(c, getfield(p, :r), t.r, unsafe)
    else
        nothing
    end

    new_p = _clone_with(
        p;
        val=val_new,
        l=(_has_child(p, :l) ? l_new : getfield(p, :l)),
        r=(_has_child(p, :r) ? r_new : getfield(p, :r)),
    )
    c[key] = new_p
    return new_p
end

################################################################################
# 9. _diff_internal  (primal difference → tangent)
################################################################################

function Mooncake._diff_internal(c::Mooncake.MaybeCache, p::N, q::N) where {T,N<:_AEN{T}}
    key = (p, q)
    haskey(c, key) && return c[key]

    Tv = Mooncake.tangent_type(T)
    Tv === Mooncake.NoTangent && return (c[key] = Mooncake.NoTangent())

    val_tan = Mooncake._diff_internal(c, p.val, q.val)
    l_tan = if _has_child(p, :l)
        Mooncake._diff_internal(c, getfield(p, :l), getfield(q, :l))
    else
        Mooncake.NoTangent()
    end
    r_tan = if _has_child(p, :r)
        Mooncake._diff_internal(c, getfield(p, :r), getfield(q, :r))
    else
        Mooncake.NoTangent()
    end
    t = TangentExprNode{Tv}(val_tan, l_tan, r_tan)
    c[key] = t
    return t
end

################################################################################
# 10. Test‑utility helpers for pointer‑equality tracking
################################################################################

function Mooncake.TestUtils.populate_address_map_internal(
    m::Mooncake.TestUtils.AddressMap, p::N, t::TangentExprNode
) where {T,N<:_AEN{T}}
    kp = Base.pointer_from_objref(p)
    kt = Base.pointer_from_objref(t)
    (haskey(m, kp) || (m[kp] = kt))
    Mooncake.TestUtils.populate_address_map_internal(m, p.val, t.val)
    if !(t.l isa Mooncake.NoTangent)
        Mooncake.TestUtils.populate_address_map_internal(m, getfield(p, :l), t.l)
    end
    if !(t.r isa Mooncake.NoTangent)
        Mooncake.TestUtils.populate_address_map_internal(m, getfield(p, :r), t.r)
    end
    return m
end

function Mooncake.TestUtils.has_equal_data_internal(
    x::N, y::N, equndef::Bool, d::Dict{Tuple{UInt,UInt},Bool}
) where {T,N<:_AEN{T}}
    idp = (objectid(x), objectid(y))
    haskey(d, idp) && return d[idp]
    d[idp] = true
    eq = Mooncake.TestUtils.has_equal_data_internal(x.val, y.val, equndef, d)
    if _has_child(x, :l) || _has_child(y, :l)
        eq &= Mooncake.TestUtils.has_equal_data_internal(
            getfield(x, :l), getfield(y, :l), equndef, d
        )
    end
    if _has_child(x, :r) || _has_child(y, :r)
        eq &= Mooncake.TestUtils.has_equal_data_internal(
            getfield(x, :r), getfield(y, :r), equndef, d
        )
    end
    return eq
end

function Mooncake.TestUtils.has_equal_data_internal(
    t::TangentExprNode{Tv},
    s::TangentExprNode{Tv},
    equndef::Bool,
    d::Dict{Tuple{UInt,UInt},Bool},
) where {Tv}
    idp = (objectid(t), objectid(s))
    haskey(d, idp) && return d[idp]
    d[idp] = true
    eq = Mooncake.TestUtils.has_equal_data_internal(t.val, s.val, equndef, d)
    if !(t.l isa Mooncake.NoTangent) || !(s.l isa Mooncake.NoTangent)
        eq &= Mooncake.TestUtils.has_equal_data_internal(t.l, s.l, equndef, d)
    end
    if !(t.r isa Mooncake.NoTangent) || !(s.r isa Mooncake.NoTangent)
        eq &= Mooncake.TestUtils.has_equal_data_internal(t.r, s.r, equndef, d)
    end
    return eq
end

################################################################################
# 11. getfield / lgetfield rrules (minimal set)
################################################################################

_field_sym(x::Symbol) = x
_field_sym(i::Int) = if i == 1
    :val
elseif i == 2
    :l
elseif i == 3
    :r
else
    :non_differentiable
end
_field_sym(::Type{Val{F}}) where {F} = _field_sym(F)
_field_sym(::Val{F}) where {F} = _field_sym(F)

function _rrule_getfield_common(
    obj_cd::Mooncake.CoDual{N,TangentExprNode{Tv}}, field_sym::Symbol, n_args::Int
) where {T,N<:_AEN{T},Tv}
    p = Mooncake.primal(obj_cd)
    pt = Mooncake.tangent(obj_cd)

    value_primal = getfield(p, field_sym)

    tangent_for_field = if field_sym === :val
        pt.val
    elseif field_sym === :l
        pt.l
    elseif field_sym === :r
        pt.r
    else
        Mooncake.NoTangent()
    end

    y_cd = Mooncake.CoDual(value_primal, Mooncake.fdata(tangent_for_field))

    function pb(Δy_rdata)
        if field_sym === :val && !(Δy_rdata isa Mooncake.NoRData)
            pt.val = Mooncake.increment_rdata!!(pt.val, Δy_rdata)
        end
        return ntuple(_ -> Mooncake.NoRData(), n_args)
    end
    return y_cd, pb
end

# lgetfield(AEN, Val{field}) --------------------------------------------------
Mooncake.@is_primitive Mooncake.MinimalCtx Tuple{typeof(Mooncake.lgetfield),_AEN,Val}
function Mooncake.rrule!!(
    ::Mooncake.CoDual{typeof(Mooncake.lgetfield)},
    obj_cd::Mooncake.CoDual{N,TangentExprNode{Tv}},
    vfield_cd::Mooncake.CoDual{Val{F},Mooncake.NoFData},
) where {T,N<:_AEN{T},Tv,F}
    return _rrule_getfield_common(obj_cd, _field_sym(F), 3)
end

# getfield by Symbol ----------------------------------------------------------
Mooncake.@is_primitive Mooncake.MinimalCtx Tuple{typeof(getfield),_AEN,Symbol}
function Mooncake.rrule!!(
    ::Mooncake.CoDual{typeof(getfield)},
    obj_cd::Mooncake.CoDual{N,TangentExprNode{Tv}},
    sym_cd::Mooncake.CoDual{Symbol,Mooncake.NoFData},
) where {T,N<:_AEN{T},Tv}
    return _rrule_getfield_common(obj_cd, Mooncake.primal(sym_cd), 3)
end

# getfield by Int -------------------------------------------------------------
Mooncake.@is_primitive Mooncake.MinimalCtx Tuple{typeof(getfield),_AEN,Int}
function Mooncake.rrule!!(
    ::Mooncake.CoDual{typeof(getfield)},
    obj_cd::Mooncake.CoDual{N,TangentExprNode{Tv}},
    idx_cd::Mooncake.CoDual{Int,Mooncake.NoFData},
) where {T,N<:_AEN{T},Tv}
    return _rrule_getfield_common(obj_cd, _field_sym(Mooncake.primal(idx_cd)), 3)
end

################################################################################
# 12. Module footers
################################################################################

end # module DynamicExpressionsMooncakeExt
