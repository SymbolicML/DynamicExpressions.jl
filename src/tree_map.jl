import Base:
    all,
    any,
    collect,
    count,
    convert,
    copy,
    filter,
    firstindex,
    foldl,
    foldr,
    foreach,
    getindex,
    hash,
    in,
    isempty,
    iterate,
    keys,
    lastindex,
    length,
    map,
    mapfoldl,
    mapfoldr,
    mapreduce,
    reduce,
    setindex!,
    sum
import Compat: @inline, Returns
import ..UtilsModule: @generate_idmap, @use_idmap

function reduce(f, tree::Node; init=nothing)
    throw(ArgumentError("reduce is not supported for trees. Use tree_mapreduce instead."))
end
function foldl(f, tree::Node; init=nothing)
    throw(ArgumentError("foldl is not supported for trees. Use tree_mapreduce instead."))
end
function foldr(f, tree::Node; init=nothing)
    throw(ArgumentError("foldr is not supported for trees. Use tree_mapreduce instead."))
end
function mapfoldl(f, tree::Node; init=nothing)
    throw(ArgumentError("mapfoldl is not supported for trees. Use tree_mapreduce instead."))
end
function mapfoldr(f, tree::Node; init=nothing)
    throw(ArgumentError("mapfoldr is not supported for trees. Use tree_mapreduce instead."))
end

"""
    tree_mapreduce(f::Function, op::Function, tree::Node)
    tree_mapreduce(f_leaf::Function, f_branch::Function, op::Function, tree::Node)

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
    f::F, op::G, tree::N; preserve_sharing::Bool=false, result_type::Type{RT}=Nothing
) where {T,N<:Node{T},F<:Function,G<:Function,RT}
    return tree_mapreduce(f, f, op, tree; preserve_sharing, result_type)
end
function tree_mapreduce(
    f_leaf::F1,
    f_branch::F2,
    op::G,
    tree::N;
    preserve_sharing::Bool=false,
    result_type::Type{RT}=Nothing,
) where {T,N<:Node{T},F1<:Function,F2<:Function,G<:Function,RT}
    if preserve_sharing
        @use_idmap(_tree_mapreduce(f_leaf, f_branch, op, tree), IdDict{N,RT}())
    end
    return _tree_mapreduce(f_leaf, f_branch, op, tree)
end
@generate_idmap tree function _tree_mapreduce(
    f_leaf::F1, f_branch::F2, op::G, tree::Node
) where {F1<:Function,F2<:Function,G<:Function}
    if tree.degree == 0
        return @inline(f_leaf(tree))
    elseif tree.degree == 1
        return op(@inline(f_branch(tree)), _tree_mapreduce(f_leaf, f_branch, op, tree.l))
    else
        return op(
            @inline(f_branch(tree)),
            _tree_mapreduce(f_leaf, f_branch, op, tree.l),
            _tree_mapreduce(f_leaf, f_branch, op, tree.r),
        )
    end
end

#! format: off
function mapreduce(f::F, op::G, tree::Node; init=nothing) where {F<:Function,G<:Function}
    if tree.degree == 0
        return @inline(f(tree))
    elseif tree.degree == 1
        return op(@inline(f(tree)), mapreduce(f, op, tree.l; init))
    else
        return op(op(@inline(f(tree)), mapreduce(f, op, tree.l; init)), mapreduce(f, op, tree.r; init))
    end
end
#! format: on

"""
    filter_and_map(filter_fnc::Function, map_fnc::Function, tree::Node; result_type)

A faster equivalent to `map(map_fnc, filter(filter_fnc, tree))`
that avoids the intermediate allocation. However, using this requires
specifying the `result_type` of `map_fnc` so the resultant array can
be preallocated.
"""
function filter_and_map(
    filter_fnc::F, map_fnc::G, tree::Node; result_type::Type{GT}
) where {F<:Function,G<:Function,GT}
    stack_size = count(filter_fnc, tree)
    # Preallocate stack:
    stack = Array{GT}(undef, stack_size)
    pointer = Ref(0)
    _filter_and_map(filter_fnc, map_fnc, tree, stack, pointer)
    return stack::Vector{result_type}
end
function _filter_and_map(
    filter_fnc::F, map_fnc::G, tree::Node, stack::Vector{GT}, pointer::Ref
) where {F<:Function,G<:Function,GT}
    if @inline(filter_fnc(tree))
        map_result = @inline(map_fnc(tree))::GT
        @inbounds stack[pointer.x += 1] = map_result
    end
    if tree.degree == 1
        _filter_and_map(filter_fnc, map_fnc, tree.l, stack, pointer)
    elseif tree.degree == 2
        _filter_and_map(filter_fnc, map_fnc, tree.l, stack, pointer)
        _filter_and_map(filter_fnc, map_fnc, tree.r, stack, pointer)
    end
    return nothing
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

"""
    getindex(root::Node, i::Int)

Get the `i`th node of `root` in depth-first order. This does not require
extra allocations, but does require a traversal of the tree for every call.
Once the matching node is found, the traversal stops.
"""
function getindex(root::N, i::Int) where {N<:Node}
    return_tree = Ref(root)
    _extract!(return_tree, root, i, 0)
    return return_tree.x
end
function _extract!(return_tree::Ref{N}, tree::N, i::Int, iter::Int)::Int where {N<:Node}
    iter += 1
    if i == iter
        return_tree.x = tree
        return iter
    end
    if tree.degree == 1
        iter = _extract!(return_tree, tree.l, i, iter)
    elseif tree.degree == 2
        iter = _extract!(return_tree, tree.l, i, iter)
        iter = _extract!(return_tree, tree.r, i, iter)
    end
    return iter
end

function Base.:(==)(a::Node{T}, b::Node{T})::Bool where {T}
    (degree = a.degree) != b.degree && return false
    if degree == 0
        (constant = a.constant) != b.constant && return false
        if constant
            return a.val::T == b.val::T
        else
            return a.feature == b.feature
        end
    end
    a.op != b.op && return false
    a.l != b.l && return false
    degree == 2 && a.r != b.r && return false
    return true
end
function Base.:(==)(a::Node{T1}, b::Node{T2})::Bool where {T1,T2}
    # TODO: Should also have preserve_sharing check... But how?
    T = promote_type(T1, T2)
    return Node{T}(a) == Node{T}(b)
end


###############################################################################
# Derived functions: ##########################################################
###############################################################################

"""
    filter(f::Function, tree::Node)

Filter nodes of a tree, returning a flat array of the nodes for which the function returns `true`.
"""
function filter(f::F, tree::Node{T}) where {F<:Function,T}
    return filter_and_map(f, identity, tree; result_type=Node{T})
end

collect(tree::Node) = filter(Returns(true), tree)

"""
    map(f::Function, tree::Node; result_type::Type{RT}=Nothing)

Map a function over a tree and return a flat array of the results in depth-first order.
Pre-specifying the `result_type` of the function can be used to avoid extra allocations,
"""
function map(f::F, tree::Node; result_type::Type{RT}=Nothing) where {F<:Function,RT}
    if RT == Nothing
        return f.(collect(tree))
    else
        return filter_and_map(Returns(true), f, tree; result_type=result_type)
    end
end

function count(f::F, tree::Node; init=0) where {F}
    return tree_mapreduce(t -> @inline(f(t)) ? 1 : 0, +, tree) + init
end

function sum(f::F, tree::Node; init=0) where {F}
    return tree_mapreduce(f, +, tree) + init
end

all(f::F, tree::Node) where {F<:Function} = !any(t -> !@inline(f(t)), tree)

function setindex!(root::Node{T}, insert::Node{T}, i::Int) where {T}
    set_node!(getindex(root, i), insert)
    return nothing
end
function setindex!(root::Node{T1}, insert::Node{T2}, i::Int) where {T1,T2}
    return setindex!(root, convert(Node{T1}, insert), i)
end

isempty(::Node) = false
iterate(root::Node) = (root, collect(root)[(begin + 1):end])
iterate(::Node, stack) = isempty(stack) ? nothing : (popfirst!(stack), stack)
in(item, tree::Node) = any(t -> t == item, tree)
length(tree::Node) = sum(Returns(1), tree)
firstindex(::Node) = 1
lastindex(tree::Node) = length(tree)
keys(tree::Node) = Base.OneTo(length(tree))
function foreach(f::Function, tree::Node)
    return tree_mapreduce(t -> (@inline(f(t)); nothing), Returns(nothing), tree)
end
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
        t -> t.op,
        (op, c...) -> Node(op, c...),
        tree;
        preserve_sharing,
        result_type=N,
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
            Node(T1, 0, true, maybe_convert(t.val::T2, T1, T2))
        else
            Node(T1, 0, false, nothing, t.feature)
        end,
        identity,
        (p, c...) -> Node(p.degree, false, nothing, 0, p.op, c...),
        tree;
        preserve_sharing,
        result_type=Node{T1},
    )
end
@inline function maybe_convert(val, ::Type{T1}, ::Type{T2}) where {T1,T2}
    # e.g., we don't want to convert Float32 to Union{Float32,Vector{Float32}}!
    !(T2 <: T1) && return convert(T1, val)
    return val
end
(::Type{Node{T}})(tree::Node; kws...) where {T} = convert(Node{T}, tree; kws...)
