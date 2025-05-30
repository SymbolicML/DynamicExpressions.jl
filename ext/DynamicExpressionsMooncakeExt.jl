module DynamicExpressionsMooncakeExt

using DynamicExpressions: AbstractExpressionNode, constructorof, branch_copy, leaf_copy
using Mooncake
using Mooncake: NoTangent
using Random: AbstractRNG

################################################################################
# Main tangent type
################################################################################

mutable struct TangentExprNode{Tv}
    const degree::UInt8
    val::Union{Tv,NoTangent}
    l::Union{TangentExprNode{Tv},NoTangent}
    r::Union{TangentExprNode{Tv},NoTangent}

    # full constructor (used by recursion)
    function TangentExprNode{Tv}(
        val_tan::Union{Tv,NoTangent},
    ) where {Tv}
        return new{Tv}(UInt8(0), val_tan, NoTangent(), NoTangent())
    end
    function TangentExprNode{Tv}(
        val_tan::Union{Tv,NoTangent},
        l_tan::Union{TangentExprNode{Tv},NoTangent},
    ) where {Tv}
        return new{Tv}(UInt8(1), val_tan, l_tan, NoTangent())
    end
    function TangentExprNode{Tv}(
        val_tan::Union{Tv,NoTangent},
        l_tan::Union{TangentExprNode{Tv},NoTangent},
        r_tan::Union{TangentExprNode{Tv},NoTangent},
    ) where {Tv}
        return new{Tv}(UInt8(2), val_tan, l_tan, r_tan)
    end
end

Mooncake.tangent_type(::Type{<:AbstractExpressionNode{T}}) where {T} = let Tv = Mooncake.tangent_type(T); Tv === NoTangent ? NoTangent : TangentExprNode{Tv}; end
Mooncake.tangent_type(::Type{TangentExprNode{Tv}}) where {Tv} = TangentExprNode{Tv}
Mooncake.tangent_type(::Type{TangentExprNode{Tv}}, ::Type{Mooncake.NoRData}) where {Tv} = TangentExprNode{Tv}
Mooncake.tangent(t::TangentExprNode, ::Mooncake.NoRData) = t
Mooncake.rdata(::TangentExprNode) = Mooncake.NoRData()

################################################################################
# zero_tangent / randn_tangent
################################################################################

struct InitHelper{F,ARGS<:Tuple,M<:Mooncake.MaybeCache}
    f::F
    args::ARGS
    dict::M
end
function (helper::InitHelper)(p::N) where {T,N<:AbstractExpressionNode{T}}
    Tv = Mooncake.tangent_type(T)
    Tv === NoTangent && return NoTangent()
    get!(helper.dict, p) do
        if p.degree == 0
            if p.constant
                return TangentExprNode{Tv}(helper(p.val))
            else
                return TangentExprNode{Tv}(NoTangent())
            end
        elseif p.degree == 1
            return TangentExprNode{Tv}(NoTangent(), helper(p.l))
        else
            return TangentExprNode{Tv}(NoTangent(), helper(p.l), helper(p.r))
        end
    end::TangentExprNode{Tv}
end
function (helper::InitHelper)(val)
    return helper.f(helper.args..., val, helper.dict)
end

function Mooncake.zero_tangent_internal(p::N, dict::Mooncake.MaybeCache) where {T,N<:AbstractExpressionNode{T}}
    return InitHelper(Mooncake.zero_tangent_internal, (), dict)(p)
end
function Mooncake.randn_tangent_internal(rng::AbstractRNG, p::N, dict::Mooncake.MaybeCache) where {T,N<:AbstractExpressionNode{T}}
    return InitHelper(Mooncake.randn_tangent_internal, (rng,), dict)(p)
end

################################################################################
# In‑place mutation helpers
################################################################################

struct IncrementHelper{F,C<:Mooncake.IncCache} 
    f::F
    cache::C
end
function (helper::IncrementHelper)(t::TangentExprNode, s...)
    if haskey(helper.cache, t) || (!isempty(s) && t === first(s))
        return t
    end
    helper.cache[t] = true
    ts = (t, s...)
    if t.degree == 0
        t.val = helper(map(ti -> ti.val, ts)...)
    elseif t.degree == 1
        t.l = helper(map(ti -> ti.l, ts)...)
    else
        t.l = helper(map(ti -> ti.l, ts)...)
        t.r = helper(map(ti -> ti.r, ts)...)
    end
    return t
end
function (helper::IncrementHelper)(t, s...)
    return helper.f(helper.cache, t, s...)
end

function Mooncake.increment_internal!!(c::Mooncake.IncCache, t::TangentExprNode, s::TangentExprNode)
    return IncrementHelper(Mooncake.increment_internal!!, c)(t, s)
end
function Mooncake.set_to_zero_internal!!(c::Mooncake.IncCache, t::TangentExprNode)
    return IncrementHelper(Mooncake.set_to_zero_internal!!, c)(t)
end

################################################################################
# Algebraic helpers (_dot / _scale / _add_to_primal / _diff)
################################################################################

function Mooncake._dot_internal(
    c::Mooncake.MaybeCache, t::TangentExprNode{Tv}, s::TangentExprNode{Tv}
) where {Tv}
    key = (t, s)
    haskey(c, key) && return c[key]::Float64
    c[key] = 0.0
    c[key] = if t.degree == 0
        if t.constant
            Mooncake._dot_internal(c, t.val, s.val)
        else
            0.0
        end
    elseif t.degree == 1
        Mooncake._dot_internal(c, t.l, s.l)
    else
        Mooncake._dot_internal(c, t.l, s.l) + Mooncake._dot_internal(c, t.r, s.r)
    end
    return res
end

function Mooncake._scale_internal(
    c::Mooncake.MaybeCache, a::Number, t::TangentExprNode{Tv}
) where {Tv}
    get!(c, t) do
        if t.degree == 0
            TangentExprNode{Tv}(Mooncake._scale_internal(c, a, t.val))
        elseif t.degree == 1
            TangentExprNode{Tv}(NoTangent(), Mooncake._scale_internal(c, a, t.l))
        else
            TangentExprNode{Tv}(NoTangent(), Mooncake._scale_internal(c, a, t.l), Mooncake._scale_internal(c, a, t.r))
        end
    end::TangentExprNode{Tv}
end

function Mooncake._add_to_primal_internal(
    c::Mooncake.MaybeCache, p::N, t::TangentExprNode{Tv}, unsafe::Bool
) where {T,N<:AbstractExpressionNode{T},Tv}
    key = (p, t, unsafe)
    get!(c, key) do
        if p.degree == 0
            new_leaf = leaf_copy(p)
            if p.constant
                new_leaf.val = Mooncake._add_to_primal_internal(c, p.val, t.val, unsafe)
            end
            return new_leaf
        elseif p.degree == 1
            l_new = Mooncake._add_to_primal_internal(c, p.l, t.l, unsafe)
            return branch_copy(p, l_new)
        else
            l_new = Mooncake._add_to_primal_internal(c, p.l, t.l, unsafe)
            r_new = Mooncake._add_to_primal_internal(c, p.r, t.r, unsafe)
            return branch_copy(p, l_new, r_new)
        end
    end::N
end

function Mooncake._diff_internal(c::Mooncake.MaybeCache, p::N, q::N) where {T,N<:AbstractExpressionNode{T}}
    Tv = Mooncake.tangent_type(T)
    Tv === NoTangent() && return NoTangent()
    key = (p, q)
    get!(c, key) do
        if p.degree == 0
            if p.constant
                TangentExprNode{Tv}(Mooncake._diff_internal(c, p.val, q.val))
            else
                NoTangent()
            end
        elseif p.degree == 1
            TangentExprNode{Tv}(
                NoTangent(),
                Mooncake._diff_internal(c, p.l, q.l)
            )
        else
            TangentExprNode{Tv}(
                NoTangent(),
                Mooncake._diff_internal(c, p.l, q.l),
                Mooncake._diff_internal(c, p.r, q.r)
            )
        end
    end::Union{TangentExprNode{Tv},NoTangent}
end

################################################################################
# Test‑utility helpers
################################################################################

function Mooncake.TestUtils.populate_address_map_internal(
    m::Mooncake.TestUtils.AddressMap, p::N, t::TangentExprNode
) where {T,N<:AbstractExpressionNode{T}}
    kp = Base.pointer_from_objref(p)
    kt = Base.pointer_from_objref(t)
    !haskey(m, kp) && (m[kp] = kt)
    if p.degree == 0
        if p.constant
            Mooncake.TestUtils.populate_address_map_internal(m, p.val, t.val)
        end
    elseif p.degree == 1
        Mooncake.TestUtils.populate_address_map_internal(m, p.l, t.l)
    else
        Mooncake.TestUtils.populate_address_map_internal(m, p.l, t.l)
        Mooncake.TestUtils.populate_address_map_internal(m, p.r, t.r)
    end
    return m
end

function Mooncake.TestUtils.has_equal_data_internal(
    x::N, y::N, equndef::Bool, d::Dict{Tuple{UInt,UInt},Bool}
) where {T,N<:AbstractExpressionNode{T}}
    idp = (objectid(x), objectid(y))
    # Just use regular `AbstractExpressionNode` Base.:(==)
    return get!(() -> x == y, d, idp)
end

function Mooncake.TestUtils.has_equal_data_internal(
    t::TangentExprNode{Tv},
    s::TangentExprNode{Tv},
    equndef::Bool,
    d::Dict{Tuple{UInt,UInt},Bool},
) where {Tv}
    idp = (objectid(t), objectid(s))
    get!(d, idp) do
        deg = t.degree
        deg == s.degree && if t.degree == 0
            Mooncake.TestUtils.has_equal_data_internal(t.val, s.val, equndef, d)
        elseif t.degree == 1
            Mooncake.TestUtils.has_equal_data_internal(t.l, s.l, equndef, d)
        else
            Mooncake.TestUtils.has_equal_data_internal(t.l, s.l, equndef, d) &&
            Mooncake.TestUtils.has_equal_data_internal(t.r, s.r, equndef, d)
        end
    end
end

################################################################################
# getfield / lgetfield rrules
################################################################################

_field_sym(x::Symbol) = x
_field_sym(i::Int) =
    if i == 1
        :degree
    elseif i == 2
        :val
    elseif i == 3
        :l
    elseif i == 4
        :r
    else
        :non_differentiable
    end
_field_sym(::Type{Val{F}}) where {F} = _field_sym(F)
_field_sym(::Val{F}) where {F} = _field_sym(F)

function _rrule_getfield_common(
    obj_cd::Mooncake.CoDual{N,TangentExprNode{Tv}}, field_sym::Symbol, n_args::Int
) where {T,N<:AbstractExpressionNode{T},Tv}
    p = Mooncake.primal(obj_cd)
    pt = Mooncake.tangent(obj_cd)

    value_primal = getfield(p, field_sym)

    tangent_for_field = if field_sym === :degree
        NoTangent()
    elseif field_sym === :val
        pt.val
    elseif field_sym === :l
        pt.l
    elseif field_sym === :r
        pt.r
    else
        NoTangent()
    end
    fdata_for_output = if tangent_for_field isa NoTangent
        Mooncake.NoFData()
    else
        Mooncake.fdata(tangent_for_field)
    end
    y_cd = Mooncake.CoDual(value_primal, fdata_for_output)

    function pb(Δy_rdata)
        if field_sym === :val && !(Δy_rdata isa Mooncake.NoRData)
            pt.val = Mooncake.increment_rdata!!(pt.val, Δy_rdata)
        end
        return ntuple(_ -> Mooncake.NoRData(), n_args)
    end
    return y_cd, pb
end

# lgetfield(AEN, Val{field})
Mooncake.@is_primitive Mooncake.MinimalCtx Tuple{typeof(Mooncake.lgetfield),AbstractExpressionNode,Val}
function Mooncake.rrule!!(
    ::Mooncake.CoDual{typeof(Mooncake.lgetfield)},
    obj_cd::Mooncake.CoDual{N,TangentExprNode{Tv}},
    vfield_cd::Mooncake.CoDual{Val{F},Mooncake.NoFData},
) where {T,N<:AbstractExpressionNode{T},Tv,F}
    return _rrule_getfield_common(obj_cd, _field_sym(F), 3)
end

# getfield by Symbol
Mooncake.@is_primitive Mooncake.MinimalCtx Tuple{typeof(getfield),AbstractExpressionNode,Symbol}
function Mooncake.rrule!!(
    ::Mooncake.CoDual{typeof(getfield)},
    obj_cd::Mooncake.CoDual{N,TangentExprNode{Tv}},
    sym_cd::Mooncake.CoDual{Symbol,Mooncake.NoFData},
) where {T,N<:AbstractExpressionNode{T},Tv}
    return _rrule_getfield_common(obj_cd, Mooncake.primal(sym_cd), 3)
end

# getfield by Int
Mooncake.@is_primitive Mooncake.MinimalCtx Tuple{typeof(getfield),AbstractExpressionNode,Int}
function Mooncake.rrule!!(
    ::Mooncake.CoDual{typeof(getfield)},
    obj_cd::Mooncake.CoDual{N,TangentExprNode{Tv}},
    idx_cd::Mooncake.CoDual{Int,Mooncake.NoFData},
) where {T,N<:AbstractExpressionNode{T},Tv}
    return _rrule_getfield_common(obj_cd, _field_sym(Mooncake.primal(idx_cd)), 3)
end

end # module DynamicExpressionsMooncakeExt
