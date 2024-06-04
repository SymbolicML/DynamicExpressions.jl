"""Test what happens if we create a custom AbstractExpressionNode that has extra fields."""

using Test
using DynamicExpressions
using DynamicExpressions: constructorof

mutable struct FrozenNode{T} <: AbstractExpressionNode{T}
    degree::UInt8
    constant::Bool
    val::T
    frozen::Bool  # Extra field!
    feature::UInt16
    op::UInt8
    l::FrozenNode{T}
    r::FrozenNode{T}

    function FrozenNode{_T}() where {_T}
        n = new{_T}()
        n.frozen = false
        return n
    end
end
function DynamicExpressions.leaf_copy(t::FrozenNode{T}) where {T}
    out = if t.constant
        constructorof(typeof(t))(; val=t.val)
    else
        constructorof(typeof(t))(T; feature=t.feature)
    end
    out.frozen = t.frozen
    return out
end
function DynamicExpressions.branch_copy(t::FrozenNode, children::Vararg{Any,M}) where {M}
    out = constructorof(typeof(t))(; op=t.op, children)
    out.frozen = t.frozen
    return out
end
function DynamicExpressions.leaf_hash(h::UInt, t::FrozenNode)
    return t.constant ? hash((0, t.val, t.frozen), h) : hash((1, t.feature, t.frozen), h)
end
function DynamicExpressions.branch_hash(
    h::UInt, t::FrozenNode, children::Vararg{Any,M}
) where {M}
    return hash((t.degree + 1, t.op, t.frozen, children), h)
end
function DynamicExpressions.branch_equal(a::FrozenNode, b::FrozenNode)
    return a.op == b.op && a.frozen == b.frozen
end
function DynamicExpressions.leaf_equal(a::FrozenNode, b::FrozenNode)
    if a.constant != b.constant || a.frozen != b.frozen
        return false
    end
    if a.constant
        return a.val == b.val
    else
        return a.feature == b.feature
    end
end

n = let n = FrozenNode{Float64}()
    n.degree = 0
    n.constant = true
    n.val = 0.0
    n
end

n = FrozenNode{Float64}(; val=0.0)
n.frozen = true
m = copy(n)
@test n.frozen == m.frozen
@test n == m
@test hash(n) == hash(m)

# Assert that changing it changes all the fields
m.frozen = !m.frozen

@test n.frozen != m.frozen
@test hash(n) != hash(m)
@test n != m

# Try out an interface for freezing parts of an expression
freeze!(n) = (n.frozen = true; n)
thaw!(n) = (n.frozen = false; n)

ex = parse_expression(
    :(x + $freeze!(sin($thaw!(y + 2.1))));
    operators=OperatorEnum(; binary_operators=[+, -, *, /], unary_operators=[sin]),
    variable_names=[:x, :y],
    evaluate_on=[freeze!, thaw!],
    node_type=FrozenNode,
    calling_module=@__MODULE__
)

@test string_tree(ex) == "x + sin(y + 2.1)"
@test ex.tree.frozen == false
@test ex.tree.r.frozen == true
@test ex.tree.r.l.frozen == false
