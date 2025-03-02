module StringsModule

using ..UtilsModule: deprecate_varmap
using ..OperatorEnumModule: AbstractOperatorEnum
using ..NodeModule: AbstractExpressionNode, tree_mapreduce

function dispatch_op_name(::Val{deg}, ::Nothing, idx)::Vector{Char} where {deg}
    if deg == 1
        return vcat(collect("unary_operator["), collect(string(idx)), [']'])
    else
        return vcat(collect("binary_operator["), collect(string(idx)), [']'])
    end
end
function dispatch_op_name(::Val{deg}, operators::AbstractOperatorEnum, idx) where {deg}
    if deg == 1
        return collect(get_op_name(operators.unaops[idx])::String)
    else
        return collect(get_op_name(operators.binops[idx])::String)
    end
end

const OP_NAME_CACHE = (; x=Dict{UInt64,String}(), lock=Threads.SpinLock())

function get_op_name(op::F) where {F}
    h = hash(op)
    lock(OP_NAME_CACHE.lock)
    try
        cache = OP_NAME_CACHE.x
        if haskey(cache, h)
            return cache[h]
        end
        op_s = if op isa Broadcast.BroadcastFunction
            base_op_s = string(op.f)
            if length(base_op_s) == 1 && first(base_op_s) in ('+', '-', '*', '/', '^')
                # Like `.+`
                string('.', base_op_s)
            else
                # Like `cos.`
                string(base_op_s, '.')
            end
        else
            string(op)
        end
        cache[h] = op_s
        return op_s
    finally
        unlock(OP_NAME_CACHE.lock)
    end
end

@inline function strip_brackets(s::Vector{Char})::Vector{Char}
    if first(s) == '(' && last(s) == ')'
        return s[(begin + 1):(end - 1)]
    else
        return s
    end
end

const FEATURE_PLACEHOLDER_FIRST_HALF_LENGTH = length("{FEATURE_")
function replace_feature_placeholders(s::String, f_variable::Function, variable_names)
    return replace(
        s,
        r"\{FEATURE_(\d+)\}" =>
            m -> f_variable(
                parse(Int, m[(begin + FEATURE_PLACEHOLDER_FIRST_HALF_LENGTH):(end - 1)]),
                variable_names,
            ),
    )
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
    if variable_names === nothing ||
        feature > lastindex(variable_names) ||
        feature < firstindex(variable_names)
        return 'x' * string(feature)
    else
        return variable_names[feature]
    end
end

# Vector of chars is faster than strings, so we use that.
function combine_op_with_inputs(op, l, r)::Vector{Char}
    if first(op) in ('+', '-', '*', '/', '^', '.', '>', '<', '=') || op == "!="
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
    # Check if this is an assignment operator with our special prefix
    op_str = String(op)
    if startswith(op_str, "ASSIGN_OP:")
        # Extract the variable name from the operator name
        var_name = op_str[11:end]
        # Format: (var ← expr)
        out = ['(']
        append!(out, collect(var_name))
        append!(out, collect(" ← "))
        # Ensure the expression is always wrapped in parentheses for clarity
        if l[1] == '(' && l[end] == ')'
            append!(out, l)
        else
            push!(out, '(')
            append!(out, strip_brackets(l))
            push!(out, ')')
        end
        push!(out, ')')
        return out
    else
        # Regular unary operator: "op(l)"
        out = copy(op)
        push!(out, '(')
        append!(out, strip_brackets(l))
        push!(out, ')')
        return out
    end
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
    variable_names=nothing,
    pretty::Union{Bool,Nothing}=nothing, # Not used, but can be used by other types
    # Deprecated
    raw::Union{Bool,Nothing}=nothing,
    varMap=nothing,
)::String where {T,F1<:Function,F2<:Function}
    !isnothing(raw) &&
        Base.depwarn("`raw` is deprecated; use `pretty` instead", :string_tree)
    pretty = @something(pretty, _not(raw), false)
    variable_names = deprecate_varmap(variable_names, varMap, :string_tree)
    raw_output = tree_mapreduce(
        let f_constant = f_constant,
            f_variable = f_variable,
            variable_names = variable_names

            (leaf,) -> if leaf.constant
                collect(f_constant(leaf.val))::Vector{Char}
            else
                collect(f_variable(leaf.feature, variable_names))::Vector{Char}
            end
        end,
        let operators = operators
            (branch,) -> if branch.degree == 1
                dispatch_op_name(Val(1), operators, branch.op)::Vector{Char}
            else
                dispatch_op_name(Val(2), operators, branch.op)::Vector{Char}
            end
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
    string_output = String(strip_brackets(raw_output))
    string_output = replace_feature_placeholders(string_output, f_variable, variable_names)
    return string_output
end

# Print an equation
for io in ((), (:(io::IO),))
    @eval function print_tree(
        $(io...),
        tree::AbstractExpressionNode,
        operators::Union{AbstractOperatorEnum,Nothing}=nothing;
        f_variable::F1=string_variable,
        f_constant::F2=string_constant,
        variable_names=nothing,
        pretty::Union{Bool,Nothing}=nothing, # Not used, but can be used by other types
        # Deprecated
        raw::Union{Bool,Nothing}=nothing,
        varMap=nothing,
    ) where {F1<:Function,F2<:Function}
        !isnothing(raw) &&
            Base.depwarn("`raw` is deprecated; use `pretty` instead", :print_tree)
        pretty = @something(pretty, _not(raw), false)
        variable_names = deprecate_varmap(variable_names, varMap, :print_tree)
        return println(
            $(io...),
            string_tree(tree, operators; f_variable, f_constant, variable_names, pretty),
        )
    end
end
_not(::Nothing) = nothing
_not(x) = !x

end
