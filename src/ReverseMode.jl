module ReverseModeModule

import ..EquationModule: Node
import ..OperatorEnumModule: OperatorEnum
import ..EvaluateEquationModule: deg0_eval, deg1_eval, deg2_eval, @return_on_nonfinite_array
import ..UtilsModule: @return_on_false, @maybe_turbo, is_bad_array

const TapeType{T,A} = IdDict{Node{T},A}

"""Instantly extract `Array` from `x::Array{T,3}`."""
@generated function constructor_of(x)
    constructor = getfield(x.name.module, Symbol(x.name.name))
    return :($constructor)
end

function reverse_grad(
    tree::Node{T}, X::AbstractMatrix{T}, operators::OperatorEnum; variable=true
) where {T}
    A = constructor_of(X)
    tape = TapeType{T,A{T,1}}()
    result, complete = load_tape!(tape, tree, X, operators)
    # init_grad = ones(T, 1, size(X, 2))
    # return reverse_grad_from_tape(
    #     init_grad, tape, tree, operators; variable=variable, ngrads=size(X, 1)
    # )
    return tape
end

function reverse_grad_from_tape(
    grad::AbstractMatrix{T},
    tape::TapeType{T},
    tree::Node{T},
    operators::OperatorEnum;
    variable::Bool,
    ngrads::Int,
)::AbstractMatrix{T} where {T}
    if tree.degree == 0
        if variable
            tree.constant && return grad .* 0
            my_grad = zeros(T, ngrads, size(grad))
            my_grad[tree.feature] .= one(T)
        else
        end
    elseif tree.degree == 1
    else
    end
end

function load_tape!(
    tape::TapeType{T,A}, tree::Node{T}, X::AbstractMatrix{T}, operators::OperatorEnum
)::Tuple{Vector{T},Bool} where {T,A}
    n = size(X, 2)
    # haskey(tape, tree) && return tape[tree], true
    result, complete = if tree.degree == 0
        deg0_eval(tree, X)
    elseif tree.degree == 1
        deg1_eval_tape!(tape, tree, X, operators.unaops[tree.op], operators)
    else
        deg2_eval_tape!(tape, tree, X, operators.binops[tree.op], operators)
    end
    @return_on_false complete result
    @return_on_nonfinite_array result T n
    tape[tree] = copy(result)
    return result, true
end

function deg1_eval_tape!(
    tape::TapeType{T,A},
    tree::Node{T},
    cX::AbstractMatrix{T},
    op::F,
    operators::OperatorEnum,
) where {T,A,F}
    left, complete = load_tape!(tape, tree.l, cX, operators)
    @return_on_false complete left
    @return_on_nonfinite_array left T n
    return deg1_eval(left, op, Val(false))
end

function deg2_eval_tape!(
    tape::TapeType{T,A},
    tree::Node{T},
    cX::AbstractMatrix{T},
    op::F,
    operators::OperatorEnum,
) where {T,A,F}
    left, complete = load_tape!(tape, tree.l, cX, operators)
    @return_on_false complete left
    @return_on_nonfinite_array left T n
    right, complete = load_tape!(tape, tree.r, cX, operators)
    @return_on_false complete right
    @return_on_nonfinite_array right T n
    return deg2_eval(left, right, op, Val(false))
end

end
