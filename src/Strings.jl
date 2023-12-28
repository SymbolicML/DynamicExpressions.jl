module StringsModule

import Compat: Returns

import ..UtilsModule: deprecate_varmap
import ..OperatorEnumModule: AbstractOperatorEnum
import ..EquationModule: AbstractExpressionNode, tree_mapreduce

const OP_NAMES = Base.ImmutableDict(
    "safe_log" => "log",
    "safe_log2" => "log2",
    "safe_log10" => "log10",
    "safe_log1p" => "log1p",
    "safe_acosh" => "acosh",
    "safe_sqrt" => "sqrt",
    "safe_pow" => "^",
)

function dispatch_op_name(::Val{2}, ::Nothing, idx)::Vector{Char}
    return vcat(collect("binary_operator["), collect(string(idx)), [']'])
end
function dispatch_op_name(::Val{1}, ::Nothing, idx)::Vector{Char}
    return vcat(collect("unary_operator["), collect(string(idx)), [']'])
end
function dispatch_op_name(::Val{2}, operators::AbstractOperatorEnum, idx)::Vector{Char}
    return get_op_name(operators.binops[idx])
end
function dispatch_op_name(::Val{1}, operators::AbstractOperatorEnum, idx)::Vector{Char}
    return get_op_name(operators.unaops[idx])
end

@generated function get_op_name(op::F)::Vector{Char} where {F}
    try
        # Bit faster to just cache the name of the operator:
        op_s = string(F.instance)
        out = collect(get(OP_NAMES, op_s, op_s))
        return :($out)
    catch
    end
    return quote
        op_s = string(op)
        out = collect(get(OP_NAMES, op_s, op_s))
        return out
    end
end

@inline function strip_brackets(s::Vector{Char})::Vector{Char}
    if first(s) == '(' && last(s) == ')'
        return s[(begin + 1):(end - 1)]
    else
        return s
    end
end

# Can overload these for custom behavior:
needs_brackets(val::Real) = false
needs_brackets(val::AbstractArray) = false
needs_brackets(val::Complex) = true
needs_brackets(val) = true

function string_constant(val)
    if needs_brackets(val)
        '(' * string(val) * ')'
    else
        string(val)
    end
end

function string_variable(feature, variable_names)
    if variable_names === nothing || feature > lastindex(variable_names)
        return 'x' * string(feature)
    else
        return variable_names[feature]
    end
end

# Vector of chars is faster than strings, so we use that.
function combine_op_with_inputs(op, l, r)::Vector{Char}
    if first(op) in ('+', '-', '*', '/', '^')
        # "(l op r)"
        out = ['(']
        append!(out, l)
        push!(out, ' ')
        append!(out, op)
        push!(out, ' ')
        append!(out, r)
        push!(out, ')')
    else
        # "op(l, r)"
        out = copy(op)
        push!(out, '(')
        append!(out, strip_brackets(l))
        push!(out, ',')
        push!(out, ' ')
        append!(out, strip_brackets(r))
        push!(out, ')')
        return out
    end
end
function combine_op_with_inputs(op, l)
    # "op(l)"
    out = copy(op)
    push!(out, '(')
    append!(out, strip_brackets(l))
    push!(out, ')')
    return out
end

"""
    string_tree(
        tree::AbstractExpressionNode{T},
        operators::Union{AbstractOperatorEnum,Nothing}=nothing;
        f_variable::F1=string_variable,
        f_constant::F2=string_constant,
        variable_names::Union{Array{String,1},Nothing}=nothing,
        # Deprecated
        varMap=nothing,
    )::String where {T,F1<:Function,F2<:Function}

Convert an equation to a string.

# Arguments
- `tree`: the tree to convert to a string
- `operators`: the operators used to define the tree

# Keyword Arguments
- `f_variable`: (optional) function to convert a variable to a string, with arguments `(feature::UInt8, variable_names)`.
- `f_constant`: (optional) function to convert a constant to a string, with arguments `(val,)`
- `variable_names::Union{Array{String, 1}, Nothing}=nothing`: (optional) what variables to print for each feature.
"""
function string_tree(
    tree::AbstractExpressionNode{T},
    operators::Union{AbstractOperatorEnum,Nothing}=nothing;
    f_variable::F1=string_variable,
    f_constant::F2=string_constant,
    variable_names::Union{Array{String,1},Nothing}=nothing,
    # Deprecated
    varMap=nothing,
)::String where {T,F1<:Function,F2<:Function}
    variable_names = deprecate_varmap(variable_names, varMap, :string_tree)
    raw_output = tree_mapreduce(
        leaf -> if leaf.constant
            collect(f_constant(leaf.val::T))
        else
            collect(f_variable(leaf.feature, variable_names))
        end,
        branch -> if branch.degree == 1
            dispatch_op_name(Val(1), operators, branch.op)
        else
            dispatch_op_name(Val(2), operators, branch.op)
        end,
        combine_op_with_inputs,
        tree,
        Vector{Char};
        f_on_shared=(c, is_shared) -> if is_shared
            out = ['{']
            append!(out, c)
            push!(out, '}')
            out
        else
            c
        end,
    )
    return String(strip_brackets(raw_output))
end

# Print an equation
for io in ((), (:(io::IO),))
    @eval function print_tree(
        $(io...),
        tree::AbstractExpressionNode,
        operators::Union{AbstractOperatorEnum,Nothing}=nothing;
        f_variable::F1=string_variable,
        f_constant::F2=string_constant,
        variable_names::Union{Array{String,1},Nothing}=nothing,
        # Deprecated
        varMap=nothing,
    ) where {F1<:Function,F2<:Function}
        variable_names = deprecate_varmap(variable_names, varMap, :print_tree)
        return println(
            $(io...), string_tree(tree, operators; f_variable, f_constant, variable_names)
        )
    end
end

function pretty_string_graph(
    tree::AbstractExpressionNode{T},
    operators::Union{AbstractOperatorEnum,Nothing}=nothing;
    f_variable::F1=string_variable,
    f_constant::F2=string_constant,
    variable_names::Union{Vector{String},Nothing}=nothing,
) where {T,F1,F2}
    output = Char[]

    # First, we build a mapping of shared nodes. We skip varible nodes
    # as it wouldn't simplify printing.
    shared_nodes = _build_shared_node_dict(tree, node -> node.degree != 0 || node.constant)
    # NOTE THAT THIS IS ASSUMED BY _leaf_string_or_shared_variable

    # We collect the shared nodes in order of appearance, and will print
    # them in that order (deepest first)
    iter = collect(values(shared_nodes))
    sort!(iter; by=x -> x.index)

    # We also want to print the final expression:
    push!(iter, (node=tree, index=length(iter) + 1))

    for (; node, index) in iter
        raw_output, _ = tree_mapreduce(
            leaf -> _leaf_string_or_shared_variable(
                index, leaf, shared_nodes; f_variable, f_constant, variable_names
            ),
            branch ->
                _branch_string_or_shared_variable(index, branch, shared_nodes, operators),
            _combine_op_with_inputs_or_shared_variable,
            node,
            @NamedTuple{chars::Vector{Char}, shared::Bool};
            break_sharing=Val(true),
        )
        is_output = index == length(iter)
        if is_output
            if !isempty(shared_nodes)
                append!(output, ('│', '\n'))
            end
            append!(output, ('=', ' '))
        else
            append!(output, (index == 1 ? '┌' : '├', '─', '─', '─', ' '))
            append!(output, _get_z_name(index))
            append!(output, (' ', '=', ' '))
        end
        append!(output, strip_brackets(raw_output))
        append!(output, ('\n',))
    end
    return String(output)
end

function _build_shared_node_dict(tree, filter::F=Returns(true)) where {F}
    i = Ref(0)
    node_counts = Dict{UInt,@NamedTuple{node::typeof(tree), index::Int}}()
    tree_mapreduce(
        p -> p,
        (p, _...) -> p,
        tree,
        typeof(tree);
        f_on_shared=(node, is_shared) -> begin
            if is_shared && filter(node)
                oid = objectid(node)
                if !haskey(node_counts, oid)
                    node_counts[oid] = (node=node, index=(i[] += 1))
                end
            end
            node
        end,
    )
    return node_counts
end

function _get_z_name(j::Int)
    out = ['z']
    for k in digits(j)
        push!(out, Char('0' + k))
    end
    return out
end

function _combine_op_with_inputs_or_shared_variable(p, c...)
    if p.shared
        # We assume that this is an intermediate variable, so don't wish to expand it!
        return (chars=p.chars, shared=false)
    else
        return (
            chars=combine_op_with_inputs(p.chars, (ci -> ci.chars).(c)...), shared=false
        )
    end
end

function _leaf_string_or_shared_variable(
    cur_intermediate_variable,
    leaf::AbstractExpressionNode{T},
    shared_nodes;
    f_variable::F1,
    f_constant::F2,
    variable_names,
) where {T,F1,F2}
    if leaf.constant
        oid = objectid(leaf)
        if haskey(shared_nodes, oid) &&
            (cur_i = @inbounds(shared_nodes[oid][2])) < cur_intermediate_variable
            return (chars=_get_z_name(cur_i), shared=true)
        else
            return (chars=collect(f_constant(leaf.val::T)), shared=false)
        end
    else
        return (chars=collect(f_variable(leaf.feature, variable_names)), shared=false)
    end
end
function _branch_string_or_shared_variable(
    cur_intermediate_variable, branch, shared_nodes, operators
)
    oid = objectid(branch)
    if haskey(shared_nodes, oid) &&
        (cur_i = @inbounds(shared_nodes[oid][2])) < cur_intermediate_variable
        return (chars=_get_z_name(cur_i), shared=true)
    else
        if branch.degree == 1
            return (chars=dispatch_op_name(Val(1), operators, branch.op), shared=false)
        else
            return (chars=dispatch_op_name(Val(2), operators, branch.op), shared=false)
        end
    end
end

end
