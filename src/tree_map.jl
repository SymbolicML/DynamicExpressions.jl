import Base:
    reduce,
    foldl,
    foldr,
    mapfoldl,
    mapfoldr,
    in,
    count,
    map,
    mapreduce,
    all,
    any,
    collect,
    iterate,
    length,
    filter,
    getindex,
    keys,
    setindex!,
    firstindex,
    lastindex

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

"""Internal macro to fix @inline on Julia versions before 1.8"""
macro _inline(ex)
    ex = _fix_inline(ex)
    return :($(esc(ex)))
end

function _fix_inline(ex)
    if VERSION >= v"1.8"
        return Expr(:macrocall, Symbol("@inline"), LineNumberNode(@__LINE__), ex)
    else
        return ex
    end
end

"""
    tree_mapreduce(f::Function, op::Function, tree::Node)

Map a function over a tree and aggregate the result using an operator `op`.
The operator will take the result of `f` on the current node, as well
as on the left node. For binary nodes, `op` will receive the result of
`f` on the current node, and both the left and right nodes (three arguments).
In other words, you may define the function signature of `op` to be `(parent, child...)`
for between 1 and 2 children.

# Examples
```jldoctest
julia> operators = OperatorEnum(; binary_operators=[+, *]);

julia> tree = Node(; feature=1) + Node(; feature=2) * 3.2;

julia> tree_mapreduce(t -> 1, +, tree)  # count nodes
5

julia> tree_mapreduce(vcat, tree) do t
    t.degree == 2 ? [t.op] : Int[]
end  # Get list of binary operators used
2-element Vector{Int64}:
 1
 2

julia> tree_mapreduce(vcat, tree) do t
    (t.degree == 0 && t.constant) ? [t.val] : Float64[]
end  # Get list of constants
1-element Vector{Float64}:
 3.2
```
"""
function tree_mapreduce(f::F, op::G, tree::Node) where {F<:Function,G<:Function}
    if tree.degree == 0
        return @_inline(f(tree))
    elseif tree.degree == 1
        return op(@_inline(f(tree)), tree_mapreduce(f, op, tree.l))
    else
        return op(
            @_inline(f(tree)), tree_mapreduce(f, op, tree.l), tree_mapreduce(f, op, tree.r)
        )
    end
end

function filter_and_map(
    filter_fnc::F, map_fnc::G, tree::Node{T}; result_type::Type{GT}=Any
) where {F<:Function,G<:Function,GT,T}
    stack = result_type[]
    _filter_and_map(filter_fnc, map_fnc, tree, stack)
    return stack::Vector{result_type}
end
function _filter_and_map(
    filter_fnc::F, map_fnc::G, tree::Node{T}, stack::Vector{GT}
) where {F<:Function,G<:Function,GT,T}
    @_inline(filter_fnc(tree)) && push!(stack, @_inline(map_fnc(tree)))
    if tree.degree == 1
        _filter_and_map(filter_fnc, map_fnc, tree.l, stack)
    elseif tree.degree == 2
        _filter_and_map(filter_fnc, map_fnc, tree.l, stack)
        _filter_and_map(filter_fnc, map_fnc, tree.r, stack)
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
        return @_inline(f(tree))::Bool
    elseif tree.degree == 1
        return @_inline(f(tree))::Bool || any(f, tree.l)
    else
        return @_inline(f(tree))::Bool || any(f, tree.l) || any(f, tree.r)
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

collect(tree::Node) = filter(_ -> true, tree)

"""
    map(f::Function, tree::Node{T}; result_type::Type{RT}=Nothing)

Map a function over a tree and return a flat array of the results in depth-first order.
Pre-specifying the `result_type` of the function can be used to avoid extra allocations,
"""
function map(f::F, tree::Node; result_type::Type{RT}=Nothing) where {F<:Function,RT}
    if RT == Nothing
        return f.(collect(tree))
    else
        return filter_and_map(_ -> true, f, tree; result_type=result_type)
    end
end
all(f::F, tree::Node) where {F<:Function} = !any(t -> !@_inline(f(t)), tree)

function setindex!(root::Node{T}, insert::Node{T}, i::Int) where {T}
    set_node!(getindex(root, i), insert)
    return nothing
end
function setindex!(root::Node{T1}, insert::Node{T2}, i::Int) where {T1,T2}
    return setindex!(root, convert(Node{T1}, insert), i)
end

#! format: off
iterate(root::Node) = (root, collect(root)[(begin + 1):end])
iterate(::Node, stack) = isempty(stack) ? nothing : (popfirst!(stack), stack)
length(tree::Node) = tree_mapreduce(_ -> 1, +, tree)
firstindex(::Node) = 1
lastindex(tree::Node) = length(tree)
keys(tree::Node) = Base.OneTo(length(tree))
in(item, tree::Node) = any(t -> t == item, tree)
count(f::F, tree::Node; init=0) where {F} = tree_mapreduce(t -> f(t) ? 1 : 0, +, tree) + init
#! format: on
