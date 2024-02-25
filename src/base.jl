"""This file is imported by Equation.jl"""

import Base:
    all,
    any,
    collect,
    count,
    convert,
    copy,
    filter,
    foldl,
    foldr,
    foreach,
    hash,
    in,
    isempty,
    iterate,
    length,
    map,
    mapfoldl,
    mapfoldr,
    mapreduce,
    reduce,
    sum
import Compat: @inline, Returns
import ..UtilsModule: @memoize_on, @with_memoize, Undefined

"""
    tree_mapreduce(
        f::Function,
        [f_branch::Function,]
        op::Function,
        tree::AbstractNode,
        f_on_shared::Function=(result, is_shared) -> result,
        break_sharing::Val=Val(false),
    )

Map a function over a tree and aggregate the result using an operator `op`.
`op` should be defined with inputs `(parent, child...) ->` so that it can aggregate
both unary and binary operators. `op` will not be called for leafs of the tree.
This differs from a normal `mapreduce` in that it allows different treatment
for parent nodes than children nodes. If this is not necessary, you may
use the regular `mapreduce` instead.
The argument `break_sharing` can be used to break connections in a `GraphNode`.

You can also provide separate functions for leaf (variable/constant) nodes
and branch (operator) nodes.

# Examples
```jldoctest
julia> operators = OperatorEnum(; binary_operators=[+, *]);

julia> tree = Node(; feature=1) + Node(; feature=2) * 3.2;

julia> tree_mapreduce(t -> 1, +, tree)  # count nodes. (regular mapreduce also works)
5

julia> tree_mapreduce(t -> 1, (p, c...) -> p + max(c...), tree)  # compute depth. regular mapreduce would fail!
5

julia> tree_mapreduce(vcat, tree) do t
    t.degree == 2 ? [t.op] : UInt8[]
end  # Get list of binary operators used. (regular mapreduce also works)
2-element Vector{UInt8}:
 1
 2

julia> tree_mapreduce(vcat, tree) do t
    (t.degree == 0 && t.constant) ? [t.val] : Float64[]
end  # Get list of constants. (regular mapreduce also works)
1-element Vector{Float64}:
 3.2
```
"""
function tree_mapreduce(
    f::F,
    op::G,
    tree::AbstractNode,
    result_type::Type=Undefined;
    f_on_shared::H=(result, is_shared) -> result,
    break_sharing=Val(false),
) where {F<:Function,G<:Function,H<:Function}
    return tree_mapreduce(f, f, op, tree, result_type; f_on_shared, break_sharing)
end
function tree_mapreduce(
    f_leaf::F1,
    f_branch::F2,
    op::G,
    tree::AbstractNode,
    result_type::Type{RT}=Undefined;
    f_on_shared::H=(result, is_shared) -> result,
    break_sharing::Val=Val(false),
) where {F1<:Function,F2<:Function,G<:Function,H<:Function,RT}

    # Trick taken from here:
    # https://discourse.julialang.org/t/recursive-inner-functions-a-thousand-times-slower/85604/5
    # to speed up recursive closure
    @memoize_on t f_on_shared function inner(inner, t)
        if t.degree == 0
            return @inline(f_leaf(t))
        elseif t.degree == 1
            return @inline(op(@inline(f_branch(t)), inner(inner, t.l)))
        else
            return @inline(op(@inline(f_branch(t)), inner(inner, t.l), inner(inner, t.r)))
        end
    end

    sharing = preserve_sharing(typeof(tree)) && break_sharing === Val(false)

    RT == Undefined &&
        sharing &&
        throw(ArgumentError("Need to specify `result_type` if nodes are shared.."))

    if sharing && RT != Undefined
        d = allocate_id_map(tree, RT)
        return @with_memoize inner(inner, tree) d
    else
        return inner(inner, tree)
    end
end
function allocate_id_map(tree::AbstractNode, ::Type{RT}) where {RT}
    d = Dict{UInt,RT}()
    # Preallocate maximum storage (counting with duplicates is fast)
    N = length(tree; break_sharing=Val(true))
    sizehint!(d, N)
    return d
end

# TODO: Raise Julia issue for this.
# Surprisingly Dict{UInt,RT} is faster than IdDict{Node{T},RT} here!
# I think it's because `setindex!` is declared with `@nospecialize` in IdDict.

"""
    any(f::Function, tree::AbstractNode)

Reduce a flag function over a tree, returning `true` if the function returns `true` for any node.
By using this instead of tree_mapreduce, we can take advantage of early exits.
"""
function any(f::F, tree::AbstractNode) where {F<:Function}
    if tree.degree == 0
        return @inline(f(tree))::Bool
    elseif tree.degree == 1
        return @inline(f(tree))::Bool || any(f, tree.l)
    else
        return @inline(f(tree))::Bool || any(f, tree.l) || any(f, tree.r)
    end
end

function Base.:(==)(a::AbstractExpressionNode, b::AbstractExpressionNode)
    return Base.:(==)(promote(a, b)...)
end
function Base.:(==)(a::N, b::N)::Bool where {N<:AbstractExpressionNode}
    if preserve_sharing(N)
        return inner_is_equal_shared(a, b, Dict{UInt,Nothing}(), Dict{UInt,Nothing}())
    else
        return inner_is_equal(a, b)
    end
end
function inner_is_equal(a, b)
    (degree = a.degree) != b.degree && return false
    if degree == 0
        return isequal_deg0(a, b)
    elseif degree == 1
        return isequal_deg1(a, b) && inner_is_equal(a.l, b.l)
    else
        return isequal_deg2(a, b) && inner_is_equal(a.l, b.l) && inner_is_equal(a.r, b.r)
    end
end
function inner_is_equal_shared(a, b, id_map_a, id_map_b)
    id_a = objectid(a)
    id_b = objectid(b)
    has_a = haskey(id_map_a, id_a)
    has_b = haskey(id_map_b, id_b)

    if has_a && has_b
        return true
    elseif has_a ⊻ has_b
        return false
    end

    (degree = a.degree) != b.degree && return false

    result = if degree == 0
        isequal_deg0(a, b)
    elseif degree == 1
        isequal_deg1(a, b) && inner_is_equal_shared(a.l, b.l, id_map_a, id_map_b)
    else
        isequal_deg2(a, b) &&
            inner_is_equal_shared(a.l, b.l, id_map_a, id_map_b) &&
            inner_is_equal_shared(a.r, b.r, id_map_a, id_map_b)
    end

    id_map_a[id_a] = nothing
    id_map_b[id_b] = nothing

    return result
end

@inline isequal_deg1(a::AbstractExpressionNode, b::AbstractExpressionNode) = a.op == b.op
@inline isequal_deg2(a::AbstractExpressionNode, b::AbstractExpressionNode) = a.op == b.op
@inline function isequal_deg0(
    a::AbstractExpressionNode{T1}, b::AbstractExpressionNode{T2}
) where {T1,T2}
    (constant = a.constant) != b.constant && return false
    if constant
        return a.val::T1 == b.val::T2
    else
        return a.feature == b.feature
    end
end

###############################################################################
# Derived functions: ##########################################################
###############################################################################

"""
    count_nodes(tree::AbstractNode)::Int

Count the number of nodes in the tree.
"""
function count_nodes(tree::AbstractNode; break_sharing=Val(false))
    return tree_mapreduce(
        _ -> 1,
        +,
        tree,
        Int64;
        f_on_shared=(c, is_shared) -> is_shared ? 0 : c,
        break_sharing,
    )
end

"""
    foreach(f::Function, tree::AbstractNode; break_sharing::Val=Val(false))

Apply a function to each node in a tree without returning the results.
"""
function foreach(
    f::F, tree::AbstractNode; break_sharing::Val=Val(false)
) where {F<:Function}
    tree_mapreduce(
        t -> (@inline(f(t)); nothing), Returns(nothing), tree, Nothing; break_sharing
    )
    return nothing
end

"""
    filter_map(filter_fnc::Function, map_fnc::Function, tree::AbstractNode, result_type::Type, break_sharing::Val=Val(false))

A faster equivalent to `map(map_fnc, filter(filter_fnc, tree))`
that avoids the intermediate allocation. However, using this requires
specifying the `result_type` of `map_fnc` so the resultant array can
be preallocated.
"""
function filter_map(
    filter_fnc::F,
    map_fnc::G,
    tree::AbstractNode,
    result_type::Type{GT};
    break_sharing::Val=Val(false),
) where {F<:Function,G<:Function,GT}
    stack = Array{GT}(undef, count(filter_fnc, tree; init=0, break_sharing))
    filter_map!(filter_fnc, map_fnc, stack, tree; break_sharing)
    return stack::Vector{GT}
end

"""
    filter_map!(filter_fnc::Function, map_fnc::Function, stack::Vector{GT}, tree::AbstractNode)

Equivalent to `filter_map`, but stores the results in a preallocated array.
"""
function filter_map!(
    filter_fnc::F,
    map_fnc::G,
    destination::Vector{GT},
    tree::AbstractNode;
    break_sharing::Val=Val(false),
) where {GT,F<:Function,G<:Function}
    pointer = Ref(0)
    foreach(tree; break_sharing) do t
        if @inline(filter_fnc(t))
            map_result = @inline(map_fnc(t))::GT
            @inbounds destination[pointer.x += 1] = map_result
        end
    end
    return nothing
end

"""
    filter(f::Function, tree::AbstractNode; break_sharing::Val=Val(false))

Filter nodes of a tree, returning a flat array of the nodes for which the function returns `true`.
"""
function filter(f::F, tree::AbstractNode; break_sharing::Val=Val(false)) where {F<:Function}
    return filter_map(f, identity, tree, typeof(tree); break_sharing)
end

"""
    collect(tree::AbstractNode; break_sharing::Val=Val(false))

Collect all nodes in a tree into a flat array in depth-first order.
"""
function collect(tree::AbstractNode; break_sharing::Val=Val(false))
    return filter(Returns(true), tree; break_sharing)
end

"""
    map(f::F, tree::AbstractNode, result_type::Type{RT}=Nothing; break_sharing::Val=Val(false)) where {F<:Function,RT}

Map a function over a tree and return a flat array of the results in depth-first order.
Pre-specifying the `result_type` of the function can be used to avoid extra allocations.
"""
function map(
    f::F, tree::AbstractNode, result_type::Type{RT}=Nothing; break_sharing::Val=Val(false)
) where {F<:Function,RT}
    if RT == Nothing
        return f.(collect(tree; break_sharing))
    else
        return filter_map(Returns(true), f, tree, result_type; break_sharing)
    end
end

"""
    count(f::F, tree::AbstractNode; init=0, break_sharing::Val=Val(false)) where {F<:Function}

Count the number of nodes in a tree for which the function returns `true`.
"""
function count(
    f::F, tree::AbstractNode; init=0, break_sharing::Val=Val(false)
) where {F<:Function}
    return tree_mapreduce(
        t -> @inline(f(t)) ? 1 : 0,
        +,
        tree,
        Int64;
        f_on_shared=(c, is_shared) -> is_shared ? 0 : c,
        break_sharing,
    ) + init
end

"""
    sum(f::Function, tree::AbstractNode; init=0, return_type=Undefined, f_on_shared=_default_shared_aggregation, break_sharing::Val=Val(false)) where {F<:Function}

Sum the results of a function over a tree. For graphs with shared nodes
such as `GraphNode`, the function `f_on_shared` is called on the result
of each shared node. This is used to avoid double-counting shared nodes (default
behavior).
"""
function sum(
    f::F,
    tree::AbstractNode;
    init=0,
    return_type=Undefined,
    f_on_shared=_default_shared_aggregation,
    break_sharing::Val=Val(false),
) where {F<:Function}
    if preserve_sharing(typeof(tree))
        @assert typeof(return_type) !== Undefined "Must specify `return_type` as a keyword argument to `sum` if `preserve_sharing` is true."
    end
    return tree_mapreduce(f, +, tree, return_type; f_on_shared, break_sharing) + init
end
function _default_shared_aggregation(c, is_shared)
    return is_shared ? (false * c) : c
end

"""
    all(f::Function, tree::AbstractNode)

Reduce a flag function over a tree, returning `true` if the
function returns `true` for all nodes, `false` otherwise.
"""
all(f::F, tree::AbstractNode) where {F<:Function} = !any(t -> !@inline(f(t)), tree)

"""
    mapreduce(f::Function, op::Function, tree::AbstractNode; return_type, f_on_shared, break_sharing)

Map a function over a tree and aggregate the result using an operator `op`.
"""
function mapreduce(
    f::F,
    op::G,
    tree::AbstractNode;
    return_type=Undefined,
    f_on_shared=(c, is_shared) -> is_shared ? (false * c) : c,
    break_sharing::Val=Val(false),
) where {F<:Function,G<:Function}
    if preserve_sharing(typeof(tree))
        @assert typeof(return_type) !== Undefined "Must specify `return_type` as a keyword argument to `mapreduce` if `preserve_sharing` is true."
    end
    return tree_mapreduce(
        f, (n...) -> reduce(op, n), tree, return_type; f_on_shared, break_sharing
    )
end

isempty(::AbstractNode) = false
function iterate(root::AbstractNode)
    return (root, collect(root; break_sharing=Val(true))[(begin + 1):end])
end
iterate(::AbstractNode, stack) = isempty(stack) ? nothing : (popfirst!(stack), stack)
in(item, tree::AbstractNode) = any(t -> t == item, tree)
function length(tree::AbstractNode; break_sharing::Val=Val(false))
    return count_nodes(tree; break_sharing)
end

"""
    hash(tree::AbstractExpressionNode{T}[, h::UInt]; break_sharing::Val=Val(false)) where {T}

Compute a hash of a tree. This will compute a hash differently
if nodes are shared in a tree. This is ignored if `break_sharing` is set to `Val(true)`.
"""
function hash(
    tree::AbstractExpressionNode{T}, h::UInt=zero(UInt); break_sharing::Val=Val(false)
) where {T}
    return tree_mapreduce(
        t -> t.constant ? hash((0, t.val), h) : hash((1, t.feature), h),
        t -> hash((t.degree + 1, t.op), h),
        (n...) -> hash(n, h),
        tree,
        UInt;
        f_on_shared=(cur_hash, is_shared) ->
            is_shared ? hash((:shared, cur_hash), h) : cur_hash,
        break_sharing,
    )
end

"""
    copy_node(tree::AbstractExpressionNode; break_sharing::Val=Val(false))

Copy a node, recursively copying all children nodes.
This is more efficient than the built-in copy.

If `break_sharing` is set to `Val(true)`, sharing in a tree will be ignored.
"""
function copy_node(
    tree::N; break_sharing::Val=Val(false)
) where {T,N<:AbstractExpressionNode{T}}
    return tree_mapreduce(
        t -> if t.constant
            constructorof(N)(; val=t.val)
        else
            constructorof(N)(T; feature=t.feature)
        end,
        identity,
        (p, children...) -> constructorof(N)(; op=p.op, children),
        tree,
        N;
        break_sharing,
    )
end

"""
    copy(tree::AbstractExpressionNode; break_sharing::Val=Val(false))

Copy a node, recursively copying all children nodes.
This is more efficient than the built-in copy.

If `break_sharing` is set to `Val(true)`, sharing in a tree will be ignored.
"""
function copy(tree::AbstractExpressionNode; break_sharing::Val=Val(false))
    return copy_node(tree; break_sharing)
end

"""
    convert(::Type{<:AbstractExpressionNode{T1}}, n::AbstractExpressionNode{T2}) where {T1,T2}

Convert a `AbstractExpressionNode{T2}` to a `AbstractExpressionNode{T1}`.
This will recursively convert all children nodes to `AbstractExpressionNode{T1}`,
using `convert(T1, tree.val)` at constant nodes.

# Arguments
- `::Type{AbstractExpressionNode{T1}}`: Type to convert to.
- `tree::AbstractExpressionNode{T2}`: AbstractExpressionNode to convert.
"""
function convert(
    ::Type{N1}, tree::N2
) where {T1,T2,N1<:AbstractExpressionNode{T1},N2<:AbstractExpressionNode{T2}}
    if N1 === N2
        return tree
    end
    return tree_mapreduce(
        t -> if t.constant
            constructorof(N1)(; val=convert(T1, t.val::T2))
        else
            constructorof(N1)(T1; feature=t.feature)
        end,
        identity,
        (p, children...) -> constructorof(N1)(; op=p.op, children),
        tree,
        N1,
    )
end
function convert(
    ::Type{N1}, tree::N2
) where {T2,N1<:AbstractExpressionNode,N2<:AbstractExpressionNode{T2}}
    return convert(with_type_parameters(N1, T2), tree)
end
function (::Type{N})(tree::AbstractExpressionNode) where {N<:AbstractExpressionNode}
    return convert(N, tree)
end

for func in (:reduce, :foldl, :foldr, :mapfoldl, :mapfoldr)
    @eval begin
        function $func(f, tree::AbstractNode; kws...)
            throw(
                error(
                    string($func) *
                    " not implemented for AbstractNode. Use `tree_mapreduce` instead.",
                ),
            )
        end
    end
end
