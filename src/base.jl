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
import ..UtilsModule: @memoize_on, @with_memoize

"""
    tree_mapreduce(f::Function, op::Function, tree::Node, result_type::Type=Nothing)
    tree_mapreduce(f_leaf::Function, f_branch::Function, op::Function, tree::Node, result_type::Type=Nothing)

Map a function over a tree and aggregate the result using an operator `op`.
`op` should be defined with inputs `(parent, child...) ->` so that it can aggregate
both unary and binary operators. `op` will not be called for leafs of the tree.
This differs from a normal `mapreduce` in that it allows different treatment
for parent nodes than children nodes. If this is not necessary, you may
use the regular `mapreduce` instead.

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
    t.degree == 2 ? [t.op] : Int[]
end  # Get list of binary operators used. (regular mapreduce also works)
2-element Vector{Int64}:
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
    f::F, op::G, tree::N, result_type::Type{RT}=Nothing; preserve_sharing::Bool=false
) where {T,N<:Node{T},F<:Function,G<:Function,RT}
    return tree_mapreduce(f, op, (tree,), result_type; preserve_sharing)
end
function tree_mapreduce(
    f::F, op::G, trees::Ns, result_type::Type{RT}=Nothing; preserve_sharing::Bool=false
) where {m,T,N<:Node{T},Ns<:NTuple{m,N},F<:Function,G<:Function,RT}
    return tree_mapreduce(f, f, op, trees, result_type; preserve_sharing)
end
function tree_mapreduce(
    f_leaf::F1,
    f_branch::F2,
    op::G,
    tree::N,
    result_type::Type{RT}=Nothing;
    preserve_sharing::Bool=false,
) where {T,N<:Node{T},F1<:Function,F2<:Function,G<:Function,RT}
    return tree_mapreduce(f_leaf, f_branch, op, (tree,), result_type; preserve_sharing)
end
function tree_mapreduce(
    f_leaf::F1,
    f_branch::F2,
    op::G,
    trees::Ns,
    result_type::Type{RT}=Nothing;
    preserve_sharing::Bool=false,
) where {m,T,N<:Node{T},Ns<:NTuple{m,N},F1<:Function,F2<:Function,G<:Function,RT}

    # Trick taken from here:
    # https://discourse.julialang.org/t/recursive-inner-functions-a-thousand-times-slower/85604/5
    # to speed up recursive closure
    @memoize_on ts function inner(inner, ts)
        if first(ts).degree == 0
            return @inline(f_leaf(ts...))
        elseif first(ts).degree == 1
            return @inline(op(@inline(f_branch(ts...)), inner(inner, map(t -> t.l, ts))))
        else
            return @inline(
                op(
                    @inline(f_branch(ts...)),
                    inner(inner, map(t -> t.l, ts)),
                    inner(inner, map(t -> t.r, ts)),
                )
            )
        end
    end

    RT == Nothing &&
        preserve_sharing &&
        throw(ArgumentError("Need to specify `result_type` if you use `preserve_sharing`."))

    if preserve_sharing && RT != Nothing
        return @with_memoize inner(inner, trees) IdDict{Ns,RT}()
    else
        return inner(inner, trees)
    end
end

"""
    any(f::Function, tree::Node)

Reduce a flag function over a tree, returning `true` if the function returns `true` for any node.
By using this instead of tree_mapreduce, we can take advantage of early exits.
"""
function any(f::F, tree::Node) where {F<:Function}
    if tree.degree == 0
        return @inline(f(tree))::Bool
    elseif tree.degree == 1
        return @inline(f(tree))::Bool || any(f, tree.l)
    else
        return @inline(f(tree))::Bool || any(f, tree.l) || any(f, tree.r)
    end
end

function Base.:(==)(a::Node{T1}, b::Node{T2})::Bool where {T1,T2}
    (degree = a.degree) != b.degree && return false
    if degree == 0
        (constant = a.constant) != b.constant && return false
        if constant
            return a.val::T1 == b.val::T2
        else
            return a.feature == b.feature
        end
    elseif degree == 1
        return a.op == b.op && a.l == b.l
    else
        return a.op == b.op && a.l == b.l && a.r == b.r
    end
end

###############################################################################
# Derived functions: ##########################################################
###############################################################################

"""
    foreach(f::Function, tree::Node)

Apply a function to each node in a tree.
"""
function foreach(f::Function, tree::Node)
    return tree_mapreduce(t -> (@inline(f(t)); nothing), Returns(nothing), tree)
end

"""
    filter_map(filter_fnc::Function, map_fnc::Function, tree::Node, result_type::Type)

A faster equivalent to `map(map_fnc, filter(filter_fnc, tree))`
that avoids the intermediate allocation. However, using this requires
specifying the `result_type` of `map_fnc` so the resultant array can
be preallocated.
"""
function filter_map(
    filter_fnc::F, map_fnc::G, tree::Node, result_type::Type{GT}
) where {F<:Function,G<:Function,GT}
    stack = Array{GT}(undef, count(filter_fnc, tree))
    filter_map!(filter_fnc, map_fnc, stack, tree)
    return stack::Vector{GT}
end

"""
    filter_map!(filter_fnc::Function, map_fnc::Function, stack::Vector{GT}, tree::Node)

Equivalent to `filter_map`, but stores the results in a preallocated array.
"""
function filter_map!(
    filter_fnc::Function, map_fnc::Function, destination::Vector{GT}, tree::Node
) where {GT}
    pointer = Ref(0)
    foreach(tree) do t
        if @inline(filter_fnc(t))
            map_result = @inline(map_fnc(t))::GT
            @inbounds destination[pointer.x += 1] = map_result
        end
    end
    return nothing
end

"""
    filter(f::Function, tree::Node)

Filter nodes of a tree, returning a flat array of the nodes for which the function returns `true`.
"""
function filter(f::F, tree::Node{T}) where {F<:Function,T}
    return filter_map(f, identity, tree, Node{T})
end

collect(tree::Node) = filter(Returns(true), tree)

"""
    map(f::Function, tree::Node, result_type::Type{RT}=Nothing)

Map a function over a tree and return a flat array of the results in depth-first order.
Pre-specifying the `result_type` of the function can be used to avoid extra allocations,
"""
function map(f::F, tree::Node, result_type::Type{RT}=Nothing) where {F<:Function,RT}
    if RT == Nothing
        return f.(collect(tree))
    else
        return filter_map(Returns(true), f, tree, result_type)
    end
end

function count(f::F, tree::Node; init=0) where {F<:Function}
    return tree_mapreduce(t -> @inline(f(t)) ? 1 : 0, +, tree) + init
end

function sum(f::F, tree::Node; init=0) where {F<:Function}
    return tree_mapreduce(f, +, tree) + init
end

all(f::F, tree::Node) where {F<:Function} = !any(t -> !@inline(f(t)), tree)

function mapreduce(f::F, op::G, tree::Node) where {F<:Function,G<:Function}
    return tree_mapreduce(f, (n...) -> reduce(op, n), tree)
end

isempty(::Node) = false
iterate(root::Node) = (root, collect(root)[(begin + 1):end])
iterate(::Node, stack) = isempty(stack) ? nothing : (popfirst!(stack), stack)
in(item, tree::Node) = any(t -> t == item, tree)
length(tree::Node) = sum(Returns(1), tree)
function hash(tree::Node{T}) where {T}
    return tree_mapreduce(
        t -> hash(t.constant ? (0, t.val::T) : (1, t.feature)),
        t -> hash((t.degree + 1, t.op)),
        (n...) -> hash(n),
        tree,
    )
end

"""
    copy_node(tree::Node; preserve_sharing::Bool=false)

Copy a node, recursively copying all children nodes.
This is more efficient than the built-in copy.
With `preserve_sharing=true`, this will also
preserve linkage between a node and
multiple parents, whereas without, this would create
duplicate child node copies.

id_map is a map from `objectid(tree)` to `copy(tree)`.
We check against the map before making a new copy; otherwise
we can simply reference the existing copy.
[Thanks to Ted Hopp.](https://stackoverflow.com/questions/49285475/how-to-copy-a-full-non-binary-tree-including-loops)

Note that this will *not* preserve loops in graphs.
"""
function copy_node(tree::N; preserve_sharing::Bool=false) where {T,N<:Node{T}}
    return tree_mapreduce(
        t -> t.constant ? Node(; val=t.val::T) : Node(T; feature=t.feature),
        identity,
        (p, c...) -> Node(p.op, c...),
        tree,
        N;
        preserve_sharing,
    )
end

copy(tree::Node; kws...) = copy_node(tree; kws...)

"""
    convert(::Type{Node{T1}}, n::Node{T2}) where {T1,T2}

Convert a `Node{T2}` to a `Node{T1}`.
This will recursively convert all children nodes to `Node{T1}`,
using `convert(T1, tree.val)` at constant nodes.

# Arguments
- `::Type{Node{T1}}`: Type to convert to.
- `tree::Node{T2}`: Node to convert.
"""
function convert(
    ::Type{Node{T1}}, tree::Node{T2}; preserve_sharing::Bool=false
) where {T1,T2}
    if T1 == T2
        return tree
    end
    return tree_mapreduce(
        t -> if t.constant
            Node(T1, 0, true, convert(T1, t.val::T2))
        else
            Node(T1, 0, false, nothing, t.feature)
        end,
        identity,
        (p, c...) -> Node(p.degree, false, nothing, 0, p.op, c...),
        tree,
        Node{T1};
        preserve_sharing,
    )
end
(::Type{Node{T}})(tree::Node; kws...) where {T} = convert(Node{T}, tree; kws...)

for func in (:reduce, :foldl, :foldr, :mapfoldl, :mapfoldr)
    @eval begin
        function $func(f, tree::Node; kws...)
            throw(
                error(
                    string($func) *
                    " not implemented for Node. Use `tree_mapreduce` instead.",
                ),
            )
        end
    end
end
